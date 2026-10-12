#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Funktioner för att hämta data ur SCB:s allmänna företagsregister (apiafr.scb.se)
# Bygger på Kristoffer Sehlbergs skript (2026-10-01), omskrivet med
# httr2::req_perform_iterative() + purrr. Anpassat efter OpenAPI-specen v1.
# Kräver httr2 >= 1.1.0 (cli följer med httr2)
#
# OBS: API:et är nere 04:00–04:30 varje natt (datauppdatering).
# OBS: API:et kan inte filtrera på status, så statusfiltreringen görs i R efter
#      hämtning. Det sparar minne men inte tid eller antal anrop.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(httr2)
library(dplyr)
library(purrr)

# Allt nedan körs i en egen miljö via local(). Endast de publika funktionerna
# (hamta_arbetsstallen, hamta_foretag, hamta_antal, hamta_kodtabell) läggs i den
# globala miljön med <<-. Hjälpfunktioner och inställningar syns alltså inte för
# användaren men nås ändå av de publika funktionerna.
local({
  
  afr_bas_url <- "https://apiafr.scb.se"
  afr_version <- "v1"
  
  # Miljövariabeln SCB_AFR_API_KEY (t.ex. i ~/.Renviron) används i första hand,
  # annars hämtas nyckeln från keyring
  afr_nyckel <- function() {
    nyckel <- Sys.getenv("SCB_AFR_API_KEY")
    if (nzchar(nyckel)) return(nyckel)
    keyring::key_get(service = "SCB", username = "Foretagsregister")
  }
  
  # Klartexter för aeStat och ftgStat (samma koder för båda)
  afr_statustext <- c("0" = "har aldrig varit verksam",
                      "1" = "verksam",
                      "9" = "ej längre verksam")
  
  # Sekunder per sida (5 000 rader) för tidsuppskattning i hamta_antal().
  # Startvärdet för urval kommer från en testkörning (Dalarna, okt 2026). Värdena
  # skrivs över av senaste faktiska uttag i sessionen. /full är ännu inte uppmätt.
  afr_matning <- new.env()
  afr_matning$sek_per_sida <- c(urval = 2.1, full = NA_real_)
  afr_matning$kalla        <- c(urval = "standardvärde", full = "ej uppmätt")
  
  # ---- Hjälpfunktioner ----------------------------------------------------------
  
  afr_formatera_tid <- function(tid) {
    sek <- as.numeric(tid, units = "secs")
    if (sek < 60) return(sprintf("%.1f s", sek))
    h <- as.integer(sek %/% 3600)
    m <- as.integer((sek %% 3600) %/% 60)
    s <- as.integer(round(sek %% 60))
    if (h > 0) sprintf("%d h %d min %d s", h, m, s) else sprintf("%d min %d s", m, s)
  }
  
  afr_formatera_antal <- function(n) format(n, big.mark = " ", scientific = FALSE)
  
  # Grundanrop: url, nyckel, throttling (max 5 anrop/s enligt SCB), retry och timeout.
  # `sokvag` är en teckenvektor med sökvägsdelar, t.ex. c("arbetsstallen", "lan", "20")
  afr_request <- function(sokvag, api_key = afr_nyckel()) {
    request(afr_bas_url) |>
      req_url_path_append(afr_version, sokvag) |>
      req_headers("X-API-Key" = api_key, .redact = "X-API-Key") |>
      req_throttle(rate = 4, realm = "apiafr.scb.se") |>  # lite marginal under 5/s
      req_retry(max_tries = 5) |>                        # 429/503, respekterar Retry-After
      req_timeout(120)
  }
  
  # Samma parse-argument överallt så att httr2:s cache för JSON-svaret återanvänds
  afr_las_svar <- function(resp) resp_body_json(resp, simplifyVector = TRUE, flatten = TRUE)
  
  # Antal enheter för en list-sökväg. /full saknar egen count, så "full" tas bort.
  afr_antal_sokvag <- function(sokvag, api_key) {
    afr_request(c(sokvag[sokvag != "full"], "count"), api_key = api_key) |>
      req_perform() |>
      afr_las_svar() |>
      pluck("count")
  }
  
  # as.character() eftersom aeStat är sträng i urvalsvarianten men heltal i /full
  afr_filtrera_status <- function(df, statuskolumn, status) {
    if (is.null(status) || NROW(df) == 0) return(df)
    filter(df, as.character(.data[[statuskolumn]]) %in% as.character(status))
  }
  
  # ---- Paginerad hämtning av en sökväg ------------------------------------------
  # Returnerar list(data = <filtrerad tibble>, antal_hamtade = <rader före filter>)
  
  afr_hamta_paginerat <- function(sokvag, element, statuskolumn, status,
                                  api_key, limit = 5000,
                                  visa_progress = TRUE, tid_per_chunk = FALSE) {
    etikett <- paste(sokvag, collapse = "/")
    start   <- Sys.time()
    senaste <- start
    sida    <- 0L
    rader   <- 0L
    
    # Totalt antal (ett extra anrop) behövs för procent och beräknad återstående tid.
    # Misslyckas det visas progress utan total i stället för att uttaget avbryts.
    totalt <- NA_integer_
    if (visa_progress || tid_per_chunk) {
      totalt <- tryCatch(afr_antal_sokvag(sokvag, api_key), error = \(e) NA_integer_)
    }
    har_totalt <- isTRUE(totalt > 0)
    
    if (visa_progress) {
      pb <- cli::cli_progress_bar(
        total  = if (har_totalt) totalt else NA,
        format = if (har_totalt) {
          paste("{etikett} {cli::pb_bar} {cli::pb_percent} |",
                "{afr_formatera_antal(rader)} av {afr_formatera_antal(totalt)} rader |",
                "ca {cli::pb_eta} kvar")
        } else {
          "{cli::pb_spin} {etikett} | {afr_formatera_antal(rader)} rader hämtade"
        },
        clear = TRUE                                    # rensas, sammanfattning skrivs nedan
      )
    }
    
    # Anropas efter varje svar: uppdaterar progress, loggar ev. chunk-tid, ger nästa cursor
    nasta_cursor <- function(resp) {
      svar <- afr_las_svar(resp)
      n    <- NROW(svar[[element]])
      sida  <<- sida + 1L
      rader <<- rader + n
      
      if (visa_progress) {
        cli::cli_progress_update(id = pb, set = if (har_totalt) min(rader, totalt) else rader)
      }
      if (tid_per_chunk) {
        nu    <- Sys.time()
        andel <- if (har_totalt) sprintf(" (%.0f %%)", 100 * rader / totalt) else ""
        cli::cli_verbatim(sprintf(
          "  %s | sida %d: %s rader (totalt %s%s) på %s",
          etikett, sida, afr_formatera_antal(n), afr_formatera_antal(rader),
          andel, afr_formatera_tid(nu - senaste)
        ))
        senaste <<- nu
      }
      if (isTRUE(svar$pagination$hasMore)) svar$pagination$nextCursorId else NULL
    }
    
    svar <- afr_request(sokvag, api_key = api_key) |>
      req_url_query(limit = limit) |>                   # samma limit genom hela flödet
      req_perform_iterative(
        next_req = iterate_with_cursor("cursorId", resp_param_value = nasta_cursor),
        max_reqs = Inf,                                 # standard är 20 sidor!
        progress = FALSE                                # egen progress bar ovan
      )
    
    # Spara uppmätt tid per sida (kräver några sidor för att vara meningsfullt)
    if (sida >= 3) {
      typ <- if ("full" %in% sokvag) "full" else "urval"
      afr_matning$sek_per_sida[typ] <- as.numeric(Sys.time() - start, units = "secs") / sida
      afr_matning$kalla[typ]        <- "uppmätt i denna session"
    }
    
    if (visa_progress) {
      cli::cli_progress_done(id = pb)
      # Egen rad i stället för format_done, som i vissa konsoler saknar radbrytning
      cli::cli_alert(paste0(etikett, ": ", afr_formatera_antal(rader), " rader på ",
                            afr_formatera_tid(Sys.time() - start)))
    }
    
    sidor <- map(svar, \(resp) {
      df <- afr_las_svar(resp)[[element]]
      if (length(df) == 0) NULL else df                 # tomma sidor ger list(), inte df
    })
    
    list(
      data = sidor |>
        map(\(df) afr_filtrera_status(df, statuskolumn, status)) |>
        list_rbind(),
      antal_hamtade = sum(map_int(sidor, NROW))
    )
  }
  
  # ---- Urval: regionkod och övriga filter ----------------------------------------
  # regionkod: "00" = hela Sverige, 2 siffror = län, 4 siffror = kommun (får blandas).
  # API:et kan bara filtrera på en sak åt gången, så regionkod och övriga filter
  # (naringsgren, anstalldaklass osv.) kan inte kombineras.
  # Returnerar en lista med sökvägar, en per anrop.
  
  afr_bygg_sokvagar <- function(objekt, regionkod, ovriga_urval, full) {
    ovriga_urval <- compact(ovriga_urval)               # tar bort NULL
    
    if (!is.null(regionkod)) {
      regionkod <- as.character(regionkod)
      if ("00" %in% regionkod) {
        if (length(regionkod) > 1) {
          stop("regionkod \"00\" (hela Sverige) kan inte kombineras med andra regionkoder.",
               call. = FALSE)
        }
        regionkod <- NULL
      }
    }
    
    if ((!is.null(regionkod)) + length(ovriga_urval) > 1) {
      stop("API:et kan bara filtrera på en sak åt gången (regionkod, naringsgren, ",
           "anstalldaklass, omsattningsklass eller juridiskform). ",
           "Hämta på en av dem och filtrera på resten i R.", call. = FALSE)
    }
    
    ar_hela_sverige <- is.null(regionkod) && length(ovriga_urval) == 0
    if (full && !ar_hela_sverige) {
      stop("Alla variabler (full = TRUE) finns bara för hela Sverige. ",
           "Hämta utan urval och filtrera i R i stället.", call. = FALSE)
    }
    
    if (ar_hela_sverige) return(list(c(objekt, if (full) "full")))
    
    if (!is.null(regionkod)) {
      regiontyp <- case_when(
        grepl("^[0-9]{2}$", regionkod) ~ "lan",
        grepl("^[0-9]{4}$", regionkod) ~ "kommun"
      )
      if (anyNA(regiontyp)) {
        stop("Ogiltig regionkod: ", paste(regionkod[is.na(regiontyp)], collapse = ", "),
             ". Ange länskod (2 siffror), kommunkod (4 siffror) eller \"00\".",
             call. = FALSE)
      }
      return(map2(regiontyp, regionkod, \(typ, kod) c(objekt, typ, kod)))
    }
    
    map(as.character(ovriga_urval[[1]]), \(kod) c(objekt, names(ovriga_urval), kod))
  }
  
  # Läsbar text för meddelandet, t.ex. "lan 20, kommun 2080"
  afr_urvalstext <- function(sokvagar) {
    if (length(sokvagar[[1]]) <= 2) return("hela Sverige")
    texter <- map_chr(sokvagar, \(s) paste(tail(s, 2), collapse = " "))
    if (length(texter) > 10) texter <- c(head(texter, 10), "...")
    paste(texter, collapse = ", ")
  }
  
  # ---- Gemensam logik för arbetsställen och juridiska enheter -------------------
  
  afr_hamta <- function(objekt, element, statuskolumn, regionkod, ovriga_urval, full,
                        status, meddelande, visa_progress, tid_per_chunk, api_key, ...) {
    okanda <- setdiff(as.character(status), names(afr_statustext))
    if (length(okanda) > 0) {
      stop("Okänd status: ", paste(okanda, collapse = ", "),
           ". Giltiga värden är 0, 1, 9 eller NULL (alla).", call. = FALSE)
    }
    
    sokvagar <- afr_bygg_sokvagar(objekt, regionkod, ovriga_urval, full)
    start    <- Sys.time()
    
    resultat <- map(sokvagar, \(s) afr_hamta_paginerat(
      s, element, statuskolumn, status,
      api_key = api_key, visa_progress = visa_progress,
      tid_per_chunk = tid_per_chunk, ...
    ))
    
    data <- resultat |>
      map("data") |>
      list_rbind() |>
      as_tibble() |>
      mutate(uttag_datum = Sys.Date())
    
    if (meddelande) {
      urvalstext <- afr_urvalstext(sokvagar)
      filtertext <- if (is.null(status)) {
        "ingen statusfiltrering (alla statusar)"
      } else {
        paste0(statuskolumn, " = ",
               paste0(status, " (", afr_statustext[as.character(status)], ")",
                      collapse = ", "))
      }
      antal_hamtade <- sum(map_dbl(resultat, "antal_hamtade"))
      
      cli::cli_inform(c(
        "v" = "Uttag klart: {objekt}{if (full) ' (alla variabler)' else ''}, {urvalstext}",
        "i" = "Filter: {filtertext}",
        "i" = "Rader: {afr_formatera_antal(nrow(data))} av {afr_formatera_antal(antal_hamtade)} hämtade behölls",
        "i" = "Total tid: {afr_formatera_tid(Sys.time() - start)}"
      ))
    }
    
    data
  }
  
  # ---- Publika funktioner -------------------------------------------------------
  # regionkod: NULL eller "00" = hela Sverige, "20" = län, "2080" = kommun.
  #            Flera koder går bra, t.ex. c("20", "2180").
  # status:    "1" = verksamma (standard), NULL = alla, eller t.ex. c("1", "9")
  
  hamta_arbetsstallen <<- function(regionkod = NULL,
                                   naringsgren = NULL,
                                   anstalldaklass = NULL,
                                   full = FALSE,
                                   status = "1",
                                   meddelande = TRUE,
                                   visa_progress = TRUE,
                                   tid_per_chunk = FALSE,
                                   api_key = afr_nyckel(), ...) {
    afr_hamta("arbetsstallen", "arbetsstallen", "aeStat", regionkod,
              list(naringsgren = naringsgren, anstalldaklass = anstalldaklass),
              full, status, meddelande, visa_progress, tid_per_chunk, api_key, ...)
  }
  
  # För juridiska enheter avser regionkod troligen säteskommun/-län
  hamta_foretag <<- function(regionkod = NULL,
                             naringsgren = NULL,
                             anstalldaklass = NULL,
                             omsattningsklass = NULL,
                             juridiskform = NULL,
                             full = FALSE,
                             status = "1",
                             meddelande = TRUE,
                             visa_progress = TRUE,
                             tid_per_chunk = FALSE,
                             api_key = afr_nyckel(), ...) {
    afr_hamta("juridiskaenheter", "jes", "ftgStat", regionkod,
              list(naringsgren = naringsgren, anstalldaklass = anstalldaklass,
                   omsattningsklass = omsattningsklass, juridiskform = juridiskform),
              full, status, meddelande, visa_progress, tid_per_chunk, api_key, ...)
  }
  
  # Antal enheter (alla statusar, API:et kan inte filtrera på status).
  # uppskatta_tid = TRUE skriver ut en grov uppskattning av hämtningstiden.
  # full = TRUE gäller bara uppskattningen (hela Sverige med alla variabler).
  hamta_antal <<- function(objekt = c("arbetsstallen", "juridiskaenheter"),
                           regionkod = NULL,
                           naringsgren = NULL,
                           anstalldaklass = NULL,
                           omsattningsklass = NULL,
                           juridiskform = NULL,
                           full = FALSE,
                           uppskatta_tid = TRUE,
                           limit = 5000,
                           api_key = afr_nyckel()) {
    objekt <- match.arg(objekt)
    if (objekt == "arbetsstallen" && !(is.null(omsattningsklass) && is.null(juridiskform))) {
      stop("omsattningsklass och juridiskform finns bara för juridiskaenheter.", call. = FALSE)
    }
    
    sokvagar <- afr_bygg_sokvagar(
      objekt, regionkod,
      list(naringsgren = naringsgren, anstalldaklass = anstalldaklass,
           omsattningsklass = omsattningsklass, juridiskform = juridiskform),
      full = full
    )
    antal <- map_int(sokvagar, \(s) afr_antal_sokvag(s, api_key))
    if (length(sokvagar[[1]]) > 2) {
      antal <- set_names(antal, map_chr(sokvagar, \(s) tail(s, 1)))
    }
    
    if (uppskatta_tid) {
      typ   <- if (full) "full" else "urval"
      kalla <- afr_matning$kalla[[typ]]
      sek   <- afr_matning$sek_per_sida[[typ]]
      if (is.na(sek)) {                                 # /full ej uppmätt: använd urval
        sek   <- afr_matning$sek_per_sida[["urval"]]
        kalla <- "uppmätt för urvalsvarianten, /full är troligen långsammare"
      }
      # Varje kod pagineras för sig, plus ett count-anrop per kod för progress bar
      sidor <- sum(pmax(ceiling(antal / limit), 1))
      tid   <- sidor * sek + length(antal) * 0.5
      
      cli::cli_inform(c(
        "i" = paste0("Uppskattad hämtningstid: ca ", afr_formatera_tid(tid), " (",
                     afr_formatera_antal(sidor), " sidor à ", sprintf("%.1f", sek),
                     " s, ", kalla, ")."),
        " " = "Grov uppskattning: svarstiden varierar med belastningen hos SCB."
      ))
    }
    
    antal
  }
  
  hamta_kodtabell <<- function(kodtabell, api_key = afr_nyckel()) {
    afr_request(c("kodtabeller", kodtabell), api_key = api_key) |>
      req_perform() |>
      afr_las_svar()
  }
  
})

# Exempel:
# ae_dalarna <- hamta_arbetsstallen("20")                    # län, verksamma
# ae_kommun  <- hamta_arbetsstallen(c("2080", "2081"))       # kommuner
# ae_blandat <- hamta_arbetsstallen(c("20", "2180"))         # län + kommun
# ae_alla    <- hamta_arbetsstallen("20", status = NULL)     # alla statusar
# ae_full    <- hamta_arbetsstallen(full = TRUE)             # hela Sverige, alla variabler
# je_handel  <- hamta_foretag(naringsgren = "47110")         # urval på näringsgren
# hamta_antal("arbetsstallen", regionkod = c("20", "21"))

# hamta_antal("arbetsstallen", full = TRUE)
# hamta_antal("juridiskaenheter", full = TRUE)
# 
# ae_sverige <- hamta_arbetsstallen(full = TRUE)
# je_sverige <- hamta_foretag(full = TRUE)