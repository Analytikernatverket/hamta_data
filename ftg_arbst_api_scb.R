# ===== ftg_arbst_api_scb_func.R =====

# för att kunna använda detta skript måste ett certifikat från SCB
# först installeras på datorn. Certifikatet får man från SCB om man
# ber om det, kontaktuppgifter finns här:
# https://www.scb.se/vara-tjanster/bestall-data-och-statistik/foretagsregistret/avgiftsfria-uppgifter-i-foretagsregistret/#fatillgang

# Dessa skript består av två filer:
# 1. ftg_arbst_api_scb_func.R (denna fil)
# 2. ftg_arbst_api_scb.R

# Den senare filen source:ar in den första så det räcker att köra en source på den andra
# Principen i denna funktionsfil är att det funktioner som börjar på "scb_"
# kan man använda, övriga funktioner används av skripten.

# Det är välkommet att förbättra skripten, men se gärna till att de är bakåt-
# kompatibla. SCB har aviserat att API:et ska ersättas av ett rimligare så
# lägg inte alltför mycket tid på att förbättra detta skript. =)

# Peter Möller, Region Dalarna, augusti 2025

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(glue)
  library(tidyverse)
})

# hämtar lokalisering för certifikatet, som kan ligga på LocalMachine
# eller CurrentUser
scb_hamta_cert <- function() {
  parse_store <- function(cmd, store) {
    rr <- try(system(cmd, intern = TRUE), silent = TRUE)
    if (inherits(rr, "try-error")) return(NULL)
    r <- iconv(rr, from = "windows-1252", to = "UTF-8")
    df <- tibble(rad = r) |>
      tidyr::fill(rad, .direction = "down") |>
      mutate(
        subject = if_else(grepl("^Subject:", rad), sub(".*CN=", "", rad), NA_character_),
        issuer  = if_else(grepl("^Issuer:",  rad), sub("^Issuer: ", "", rad), NA_character_),
        thumb   = if_else(grepl("^Cert Hash(.*):", rad), gsub(".*: ", "", rad), NA_character_)
      ) |>
      tidyr::fill(subject, issuer) |>
      filter(!is.na(thumb)) |>
      mutate(subject_lc = tolower(subject), issuer_lc = tolower(issuer)) |>
      # matcha: CN innehåller sokpavar/sökpåver ELLER issuer innehåller scb
      filter(grepl("sokpavar|sökpåver|sokpåver", subject_lc) & grepl("\\bscb\\b", issuer_lc)) |>
      transmute(store = store, thumb = gsub(" ", "", thumb))
    if (nrow(df)) df[1,] else NULL
  }
  hit <- parse_store("certutil -user -store My", "CurrentUser")
  if (is.null(hit)) hit <- parse_store("certutil -store My",      "LocalMachine")
  if (is.null(hit)) stop("Hittade inget SCB-sökpåver-cert i CurrentUser eller LocalMachine.")
  list(store = hit$store, thumb = hit$thumb)
}

# global variabel som används istället för att alltid köra funktionen vid varje postning
cert_info <- scb_hamta_cert()

# Hämta certifikat-thumbprint (Windows, certutil)
scb_hamta_cert_thumb <- function() {
  cert_rader_raw <- tryCatch(system("certutil -user -store My", intern = TRUE), error = function(e) character())
  cert_local     <- tryCatch(system("certutil -store My", intern = TRUE),      error = function(e) character())
  cert_rader_raw_alla <- c(cert_rader_raw, cert_local)
  if (!length(cert_rader_raw_alla)) stop("Kunde inte läsa certifikat från certutil.")

  cert_rader <- iconv(cert_rader_raw_alla, from = "windows-1252", to = "UTF-8")
  cert_df <- tibble(rad = cert_rader) %>%
    fill(rad, .direction = "down") %>%
    mutate(
      cert_namn = if_else(grepl("^Subject:", rad), gsub(".*CN=", "", rad), NA_character_),
      thumb     = if_else(grepl("^Cert Hash(.*):", rad), gsub(".*: ", "", rad), NA_character_)
    ) %>%
    fill(cert_namn) %>%
    filter(!is.na(thumb)) %>%
    select(cert_namn, thumb) %>%
    filter(str_detect(tolower(cert_namn), "scb"), str_detect(tolower(cert_namn), "sokpavar"))

  if (!nrow(cert_df)) stop("Hittade inget SCB-sökpåver-certifikat i Windows certifikatförråd.")
  cert_df$thumb[[1]]
}

# Rate-limitad POST mot API:t (max ~10 anrop per 10s)
api_posta <- function(url, body, cert_thumb, .api_calls_env, visa_progress = TRUE, visa_medd = TRUE) {
  now <- as.numeric(Sys.time())
  .api_calls_env$timestamps <- .api_calls_env$timestamps[.api_calls_env$timestamps > now - 10]
  if (length(.api_calls_env$timestamps) >= 10) {
    wait_time <- 10 - (now - min(.api_calls_env$timestamps))
    if (visa_progress && visa_medd) message(sprintf("⏳ Väntar %.1f sek pga anropsgräns...", wait_time))
    Sys.sleep(wait_time)
  }
  .api_calls_env$timestamps <- c(.api_calls_env$timestamps, as.numeric(Sys.time()))

  httr::POST(
    url,
    add_headers(`Content-Type`="application/json", Accept="application/json"),
    body   = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    httr::config(sslcert = paste0(cert_info$store, "\\MY\\", cert_thumb), sslcerttype = "Schannel")
  )
}

# Generisk rakna()-hjälpare
rakna_api <- function(rakna_url, payload, cert_thumb, .api_calls_env, visa_progress = TRUE, visa_medd = TRUE) {
  r <- api_posta(rakna_url, payload, cert_thumb, .api_calls_env, visa_progress, visa_medd)
  if (r$status_code != 200) return(NA)
  httr::content(r)
}

# Generisk hamta()-hjälpare (returnerar tibble eller NULL)
hamta_api <- function(hamta_url, payload, cert_thumb, .api_calls_env, visa_progress = TRUE, visa_medd = TRUE) {
  r <- api_posta(hamta_url, payload, cert_thumb, .api_calls_env, visa_progress, visa_medd)
  if (r$status_code != 200) return(NULL)
  innehall <- httr::content(r, simplifyVector = TRUE)
  if (is.null(innehall)) return(NULL)
  if (is.data.frame(innehall)) {
    as_tibble(innehall)
  } else if (is.list(innehall) && length(innehall) > 0 && is.data.frame(innehall[[1]])) {
    as_tibble(innehall[[1]])
  } else {
    NULL
  }
}

# Säker appender som inte skriver över och ger index + logg
skapa_resultat_lagg_till <- function(resultat_ref, visa_medd = TRUE) {
  .res_idx <- 0L
  force(resultat_ref)
  function(df, label = NULL) {
    if (is.null(df) || !nrow(df)) return(invisible(NULL))
    .res_idx <<- .res_idx + 1L
    resultat_ref[[.res_idx]] <<- df
    if (visa_medd) {
      if (is.null(label)) {
        message(sprintf("Lade till lista #%d: %d rader", .res_idx, nrow(df)))
      } else {
        message(sprintf("Lade till lista #%d (%s): %d rader", .res_idx, label, nrow(df)))
      }
    }
    invisible(NULL)
  }
}

# Hjälpare: formatering
scb_format <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)
scb_sec <- function(start_time) round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)

# Hämta kommunnamn ur resultat (första förekomsten)
api_kommunnamn_fran_resultat <- function(resultat_lista, kom, kolumn_namn) {
  out <- resultat_lista %>%
    keep(~ !is.null(.x) && "kod_kommun" %in% names(.x) && any(.x$kod_kommun == kom)) %>%
    pluck(1)
  if (is.null(out) || !nrow(out) || !(kolumn_namn %in% names(out))) return(kom)
  val <- out %>% filter(kod_kommun == kom) %>% slice(1) %>% dplyr::pull(all_of(kolumn_namn))
  val %||% kom
}

# Summera antal rader i resultat för given kommunkod
api_summa_for_kom <- function(resultat_lista, kom) {
  if (is.null(resultat_lista)) return(0L)
  sum(purrr::map_int(resultat_lista, ~ if (!is.null(.x) && "kod_kommun" %in% names(.x) && any(.x$kod_kommun == kom)) nrow(.x) else 0L))
}

# ---- Hämta KategorierMedKodtabeller (Je/Ae) och gör tibble ----
scb_kategorier_med_kodtabeller <- function(
    tabell = "foretag",         # "foretag"  eller "arbetsstalle"
    med_varden = TRUE,          # TRUE = variabler med värden, FALSE = bara variabler
    cert_thumb = scb_hamta_cert_thumb()) {

  del <- if_else(tolower(tabell) %in% c("ftg", "foretag", "företag"), "Je", "Ae")
  base_url <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/"
  url <- paste0(base_url, del, "/KategorierMedKodtabeller")

  r <- GET(url,
           config = config(sslcert = paste0(cert_info$store, "\\MY\\", cert_thumb),
                           sslcerttype = "Schannel"))
  stop_for_status(r)
  kats <- content(r, as = "parsed", encoding = "UTF-8")

  # Platta ut: en rad per (kategori, kod)
  retur_df <- tibble(
    kategori_id = map_chr(kats, ~ .x$Id_Kategori_JE %||% .x$Id_Kategori_AE %||% .x$Id_Kategori %||% .x$Id),
    kategori_typ = map_chr(kats, ~ .x$Datatyp %||% NA_character_),
    varde_lista = map(kats, ~ .x$VardeLista %||% list())
  ) |>
    mutate(varde_lista = map(varde_lista, ~ keep(.x, ~ is.list(.x) && (!is.null(.x$Varde) || !is.null(.x$Kod)))) ) |>
    unnest_longer(varde_lista, keep_empty = TRUE) |>
    mutate(
      kod  = map_chr(varde_lista, ~ as.character(.x$Varde %||% .x$Kod %||% NA_character_)),
      text = map_chr(varde_lista, ~ as.character(.x$Text  %||% .x$Beskrivning %||% NA_character_))
    ) |>
    select(-varde_lista)

  if (!med_varden) {
    retur_df <- retur_df %>%
      group_by(kategori_id) %>%
      summarise(antal_unika_varden = n(), .groups = "drop")
  }
  return(retur_df)
}

# # ---- Exempel ----
# # Alla kategorier för företag med kodtabeller
# ftg_kat_varden <- scb_kategorier_med_kodtabeller("foretag")
# ftg_bara_kat <- scb_kategorier_med_kodtabeller("foretag", med_varden = FALSE)

# # Alla kategorier för arbetsställen med kodtabeller
# arbst_kat_varden <- scb_kategorier_med_kodtabeller("arbetsstalle")
# arbst_bara_kat <- scb_kategorier_med_kodtabeller("arbetsstalle", med_varden = FALSE)

# # Titta på en kategori:
# je_kat |> filter(kategori_id == "Juridisk form") |> select(kategori_id, kod, text) |> distinct()

# Bygg payload av bas-kategorier + extra kategorier och valfria statusfält
api_bygg_payload <- function(
    kategori_lista,           # list(list(Kategori="SätesKommun", Kod=list("2080")), ...)
    foretagsstatus = NULL,    # t.ex. "1" eller c("0","1","9") eller NULL
    regstatus = NULL,         # t.ex. "1" eller c("1","9") eller NULL
    arbetsstallestatus = NULL,# t.ex. "1" eller NULL
    extra_kategorier = NULL   # samma struktur som kategori_lista
){
  kat <- kategori_lista
  if (!is.null(extra_kategorier) && length(extra_kategorier)) {
    kat <- c(kat, extra_kategorier)
  }
  payload <- list(Kategorier = kat)
  if (!is.null(foretagsstatus))      payload$Företagsstatus      <- unname(foretagsstatus)
  if (!is.null(regstatus))           payload$Registreringsstatus <- unname(regstatus)
  if (!is.null(arbetsstallestatus))  payload$Arbetsställestatus  <- unname(arbetsstallestatus)
  payload
}

scb_rakna_foretag_i_kommuner <- function(
    kommunkoder,
    foretagsstatus = "1",                # NULL = alla; eller vektor c("0","1","9")
    registreringsstatus = "1",          # NULL = alla; eller vektor c("1","9")
    extra_kategorier = NULL,            # t.ex. list(list(Kategori="2-siffrig bransch 1", Kod=c("10","11")))
    cert_thumb = scb_hamta_cert_thumb(),
    returnera_till_konsol = TRUE,
    returnera_df = FALSE
){
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  base_url  <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/"
  rakna_url <- paste0(base_url, "je/raknaforetag/")
  .api_calls <- new.env(); .api_calls$timestamps <- numeric(0)

  retur_df <- map_dfr(kommunkoder, function(kom){
    payload <- api_bygg_payload(
      kategori_lista = list(list(Kategori = "SätesKommun", Kod = list(kom))),
      foretagsstatus = foretagsstatus,
      regstatus      = registreringsstatus,
      extra_kategorier = extra_kategorier
    )
    n <- rakna_api(rakna_url, payload, cert_thumb, .api_calls, visa_medd, visa_medd)
    kommun_namn <- hamtaregion_kod_namn(kom)$region
    if (returnera_till_konsol) message(glue::glue("📏 {kommun_namn}: {n %||% NA} företag"))
    tibble(kommun = kom, antal_foretag = as.integer(n %||% NA))
  })
  if (returnera_df) return(retur_df)
}

scb_rakna_arbetsstallen_i_kommuner <- function(
    kommunkoder,
    arbetsstallestatus = "1",           # NULL = alla; eller vektor
    extra_kategorier = NULL,            # t.ex. list(list(Kategori="2-siffrig bransch 1", Kod=c("10","11")))
    cert_thumb = scb_hamta_cert_thumb(),
    returnera_till_konsol = TRUE,
    returnera_df = FALSE
){
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  base_url  <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/"
  rakna_url <- paste0(base_url, "ae/raknaarbetsstallen/")
  .api_calls <- new.env(); .api_calls$timestamps <- numeric(0)

  retur_df <- map_dfr(kommunkoder, function(kom){
    payload <- api_bygg_payload(
      kategori_lista = list(list(Kategori = "Kommun", Kod = list(kom))),
      arbetsstallestatus = arbetsstallestatus,
      extra_kategorier = extra_kategorier
    )
    n <- rakna_api(rakna_url, payload, cert_thumb, .api_calls, visa_medd, visa_medd)
    kommun_namn <- hamtaregion_kod_namn(kom)$region
    if (returnera_till_konsol) message(glue::glue("📏 {kommun_namn}: {n %||% NA} arbetsställen"))
    tibble(kommun = kom, antal_arbetsstallen = as.integer(n %||% NA))
  })
  if (returnera_df) return(retur_df)
}


scb_hamta_foretag <- function(
    kommunkoder = NULL,                          # kommunkoder för de kommuner man vill ha data för, en eller flera i en vektor, tex: c("2080", "2085")
    grundfilter = list(),                        # möjlighet att skicka med fler variabler för filtrering, görs i form av en namngiven list, tex. list(list(Kategori="SätesKommun", Kod=list("2080")), ...)
    juridisk_kod = NULL,                         # default är alla företag, ange juridisk_kod = c("10", "31", "49") för fysiska personer (enskilda firmor), Handelsbolag/kommanditbolag samt Övriga aktiebolag
    oms_klass = NULL,                            # default är NULL = alla omsättningsklasser - använd scb_kategorier_med_kodtabeller("foretag") för att se vilka värden som finns
    branscher = NULL,                            # default är NULL = alla branscher - använd scb_kategorier_med_kodtabeller("foretag") för att se vilka värden som finns
    vald_foretagsstatus = "1",                   # default är 1 = är verksam (finns också 0 = har aldrig varit verksam och 9 = är ej längre verksam)
    vald_registreringsstatus = "1",              # default är 1 = registrerat företag (finns också 9 = avregistrerat företag)
    cert_thumb = scb_hamta_cert_thumb(),         # thumb från installerat certifikat, kan skickas med men behöver oftast inte göras
    visa_meddelanden_konsol = FALSE,             # detaljerade meddelanden om vad som händer
    visa_resultat_meddelanden_konsol = TRUE,     # meddelanden om vad som laddats ned
    visa_progress = TRUE                         # progress-bar för om man har flera kommuner
) {
  total_start <- Sys.time()
  library(tidyverse); library(glue)

  # --- API-endpoints
  base_url <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/"
  rakna_url <- paste0(base_url, "je/raknaforetag/")
  hamta_url <- paste0(base_url, "je/hamtaforetag/")
  kategorier_url <- paste0(base_url, "je/kategoriermedkodtabeller")
  max_rader <- 2000

  # --- cert och rate-limit env
  .api_calls <- new.env(); .api_calls$timestamps <- numeric(0)

  # --- progress
  total_kombinationer <- length(kommunkoder)
  if (visa_progress) {
    pb <- progress::progress_bar$new(total = total_kombinationer, format = ":current/:total [:bar] :percent :message", clear = FALSE)
  } else {
    pb <- list(tick = function(...) NULL)
  }

  # --- resultat
  resultat <- list()

  resultat_lagg_till <- local({
    .res_idx <- 0L
    function(df, label = NULL) {
      if (is.null(df) || !nrow(df)) return(invisible(NULL))
      .res_idx <<- .res_idx + 1L
      resultat[[.res_idx]] <<- df
      if (!is.null(label)) {
        if (visa_meddelanden_konsol) message(sprintf("Lade till lista #%d (%s): %d rader", .res_idx, label, nrow(df)))
      } else {
        if (visa_meddelanden_konsol) message(sprintf("Lade till lista #%d: %d rader", .res_idx, nrow(df)))
      }
      invisible(NULL)
    }
  })


  # --- helpers lokalt
  rakna <- function(payload) rakna_api(rakna_url, payload, cert_thumb, .api_calls, visa_progress, visa_meddelanden_konsol)
  hamta <- function(payload) hamta_api(hamta_url, payload, cert_thumb, .api_calls, visa_progress, visa_meddelanden_konsol)

  skapa_payload <- function(kom = NULL, oms = NULL, bransch = NULL) {
    kat <- list()
    if (!is.null(kom))       kat <- append(kat, list(list(Kategori = "SätesKommun",         Kod = list(kom))))
    if (!is.null(juridisk_kod)) kat <- append(kat, list(list(Kategori = "Juridisk form",      Kod = as.list(juridisk_kod))))
    if (!is.null(oms))       kat <- append(kat, list(list(Kategori = "Omsättningsklass grov", Kod = as.list(oms))))
    if (!is.null(bransch))   kat <- append(kat, list(list(Kategori = "2-siffrig bransch 1",   Kod = as.list(bransch))))
    if (!is.null(vald_foretagsstatus))      kat <- append(kat, list(list(Kategori = "Företagsstatus",      Kod = as.list(vald_foretagsstatus))))
    if (!is.null(vald_registreringsstatus)) kat <- append(kat, list(list(Kategori = "Registreringsstatus", Kod = as.list(vald_registreringsstatus))))
    payload <- list(Kategorier = kat)
    if (length(grundfilter) > 0) payload$variabler <- grundfilter
    payload
  }

  hamta_kategorier <- function() {
    if (visa_progress && visa_meddelanden_konsol) message("📋 Hämtar tillgängliga kategorier för automatisk uppdelning...")
    resp <- httr::GET(url = kategorier_url, config = httr::config(sslcert = paste0(cert_info$store, "\\MY\\", cert_thumb), sslcerttype = "Schannel"))
    if (resp$status_code != 200) return(list(oms = as.character(0:12), bransch = sprintf("%02d", 1:99)))
    kategorier <- httr::content(resp, as = "parsed", encoding = "UTF-8")
    oms_kategori <- NULL; bransch_kategori <- NULL
    for (kat in kategorier) {
      if (grepl("omsättningsklass grov", tolower(kat$Id_Kategori_JE))) oms_kategori <- kat
      if (grepl("2-siffrig bransch 1", tolower(kat$Id_Kategori_JE))) bransch_kategori <- kat
    }
    oms_koder <- oms_kategori$VardeLista %>% keep(~ is.list(.x) && !is.null(.x$Varde)) %>% map_chr(~ as.character(.x$Varde))
    bransch_koder <- bransch_kategori$VardeLista %>% keep(~ is.list(.x) && !is.null(.x$Varde)) %>% map_chr(~ as.character(.x$Varde))
    if (visa_progress && visa_meddelanden_konsol) message(sprintf("✅ Hämtade %d omsättningsklasser och %d branscher", length(oms_koder), length(bransch_koder)))
    list(oms = oms_koder, bransch = bransch_koder)
  }

  auto_kategorier <- NULL

  # --- loop per kommun
  for (kom in kommunkoder %||% list(NULL)) {
    kommun_start <- Sys.time()
    pb$tick(tokens = list(message = kom))
    if (visa_progress && visa_meddelanden_konsol) message(sprintf("🔍 Kommun %s: startar hämtning", kom))

    payload_kom <- skapa_payload(kom = kom)
    n_kom <- rakna(payload_kom)
    if (!is.na(n_kom) && n_kom <= max_rader) {
      res <- hamta(payload_kom)
      if (!is.null(res)) {
        resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
        if (visa_resultat_meddelanden_konsol) {
          kommun_namn <- api_kommunnamn_fran_resultat(resultat, kom, "Säteskommun")
          antal <- api_summa_for_kom(resultat, kom)
          message(glue("🏁 {kommun_namn}: hämtade {scb_format(antal)} företag på {scb_sec(kommun_start)} sekunder"))
        }
        next
      }
    }

    if (is.null(auto_kategorier)) auto_kategorier <- hamta_kategorier()
    aktuell_oms      <- oms_klass   %||% auto_kategorier$oms
    aktuell_bransch  <- branscher   %||% auto_kategorier$bransch

    # Steg 1: försök per oms-grupper
    aktuell_oms_lista <- map(seq_along(aktuell_oms), ~ aktuell_oms[.x:length(aktuell_oms)])
    lyckade_oms <- character(0)
    testade_oms <- character(0)

    repeat {
      hittade <- FALSE
      aktuell_oms_lista <- map(aktuell_oms_lista, ~ setdiff(.x, lyckade_oms)) %>% keep(~ length(.x) > 0)

      for (oms_grupp in aktuell_oms_lista) {
        if (visa_meddelanden_konsol) message(glue("Testar OMS-grupp: {paste(oms_grupp, collapse=', ')}"))
        if (all(oms_grupp %in% lyckade_oms)) next

        if (length(oms_grupp) == 2) {
          for (oms_kod in oms_grupp) {
            if (oms_kod %in% lyckade_oms || oms_kod %in% testade_oms) next
            n <- rakna(skapa_payload(kom = kom, oms = oms_kod))
            testade_oms <- union(testade_oms, oms_kod)
            if (visa_meddelanden_konsol) message(glue("🔢 OMS-kod {oms_kod} → {n} företag\n"))
            if (!is.na(n) && n <= max_rader) {
              res <- hamta(skapa_payload(kom = kom, oms = oms_kod))
              if (!is.null(res)) {
                resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
                lyckade_oms <- union(lyckade_oms, oms_kod)
                hittade <- TRUE; break
              }
            }
          }
        } else {
          kvarvarande_grupp <- setdiff(oms_grupp, lyckade_oms)
          if (!length(kvarvarande_grupp)) next
          n <- rakna(skapa_payload(kom = kom, oms = kvarvarande_grupp))
          if (visa_meddelanden_konsol) message(glue("Räknar rader i uttag: {n} stycken.\n"))
          if (!is.na(n) && n <= max_rader) {
            res <- hamta(skapa_payload(kom = kom, oms = kvarvarande_grupp))
            if (!is.null(res)) {
              resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
              lyckade_oms <- union(lyckade_oms, kvarvarande_grupp)
              hittade <- TRUE; break
            }
          }
        }
      }
      if (!hittade) break
    }

    # Steg 2: bryt upp i branscher för OMS som är kvar
    koder_att_bryta_upp <- setdiff(aktuell_oms, lyckade_oms)

    dela_upp_branschgrupp <- function(bransch_koder, kom, oms_kod, max_rader, lyckade_branscher) {
      if (!length(bransch_koder)) return(list(lyckade_branscher = lyckade_branscher, hittade = FALSE))
      n <- rakna(skapa_payload(kom = kom, oms = oms_kod, bransch = bransch_koder))
      if (length(bransch_koder) == 1) {
        if (visa_meddelanden_konsol) message(glue("🔢 OMS {oms_kod} + Bransch {bransch_koder} → {n} företag\n"))
      } else {
        if (visa_meddelanden_konsol) message(glue("🔢 OMS {oms_kod} + Branscher {min(bransch_koder)}-{max(bransch_koder)} → {n} företag\n"))
      }
      if (!is.na(n) && n <= max_rader) {
        res <- hamta(skapa_payload(kom = kom, oms = oms_kod, bransch = bransch_koder))
        if (!is.null(res)) {
          resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
          lyckade_branscher <- union(lyckade_branscher, bransch_koder)
          return(list(lyckade_branscher = lyckade_branscher, hittade = TRUE))
        }
      } else if (!is.na(n) && n > max_rader && length(bransch_koder) > 1) {
        if (visa_meddelanden_konsol) message(glue("Gruppen är för stor ({n} rader), delar upp...\n"))
        mitt <- ceiling(length(bransch_koder) / 2)
        r1 <- dela_upp_branschgrupp(bransch_koder[1:mitt], kom, oms_kod, max_rader, lyckade_branscher)
        r2 <- dela_upp_branschgrupp(bransch_koder[(mitt+1):length(bransch_koder)], kom, oms_kod, max_rader, r1$lyckade_branscher)
        return(list(lyckade_branscher = r2$lyckade_branscher, hittade = r1$hittade || r2$hittade))
      }
      list(lyckade_branscher = lyckade_branscher, hittade = FALSE)
    }

    for (oms_kod in koder_att_bryta_upp) {
      if (visa_meddelanden_konsol) message(glue("\n🔍 Delar upp OMS-kod {oms_kod} i branscher...\n"))
      bransch_grupper <- split(aktuell_bransch, cut(seq_along(aktuell_bransch), breaks = 5, labels = FALSE))
      aktuell_bransch_lista <- map(seq_along(bransch_grupper), function(i) flatten_chr(bransch_grupper[i:length(bransch_grupper)])) %>%
        set_names(map_chr(seq_along(bransch_grupper), function(i) { alla <- flatten_chr(bransch_grupper[i:length(bransch_grupper)]); sprintf("%s-%s", min(alla), max(alla)) }))
      lyckade_branscher <- character(0)

      repeat {
        hittade <- FALSE
        aktuell_bransch_lista <- map(aktuell_bransch_lista, ~ setdiff(.x, lyckade_branscher)) %>% keep(~ length(.x) > 0)
        for (namn in names(aktuell_bransch_lista)) {
          kvarvarande <- setdiff(aktuell_bransch_lista[[namn]], lyckade_branscher)
          if (!length(kvarvarande)) next
          if (length(kvarvarande) == 2) {
            for (br_kod in kvarvarande) {
              n <- rakna(skapa_payload(kom = kom, oms = oms_kod, bransch = br_kod))
              if (visa_meddelanden_konsol) message(glue("🔢 OMS {oms_kod} + Bransch {br_kod} → {n} företag\n"))
              if (!is.na(n) && n <= max_rader) {
                res <- hamta(skapa_payload(kom = kom, oms = oms_kod, bransch = br_kod))
                if (!is.null(res)) {
                  resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
                  lyckade_branscher <- union(lyckade_branscher, br_kod)
                  hittade <- TRUE; break
                }
              }
            }
          } else {
            n <- rakna(skapa_payload(kom = kom, oms = oms_kod, bransch = kvarvarande))
            if (!is.na(n) && n <= max_rader) {
              res <- hamta(skapa_payload(kom = kom, oms = oms_kod, bransch = kvarvarande))
              if (!is.null(res)) {
                resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
                lyckade_branscher <- union(lyckade_branscher, kvarvarande)
                hittade <- TRUE; break
              }
            } else if (!is.na(n)) {
              del <- dela_upp_branschgrupp(kvarvarande, kom, oms_kod, max_rader, lyckade_branscher)
              lyckade_branscher <- del$lyckade_branscher
              if (del$hittade) hittade <- TRUE
            }
          }
        }
        if (!hittade) break
        if (length(aktuell_bransch_lista) > 0)
          aktuell_bransch_lista <- aktuell_bransch_lista %>% imap(~ .x) %>% set_names(map_chr(., ~ sprintf("%s-%s", min(.x), max(.x))))
      }
    }

    if (visa_resultat_meddelanden_konsol) {
      kommun_namn <- api_kommunnamn_fran_resultat(resultat, kom, "Säteskommun")
      antal <- api_summa_for_kom(resultat, kom)
      message(glue("🏁 {kommun_namn}: hämtade {scb_format(antal)} företag på {scb_sec(kommun_start)} sekunder"))
    }
  } # kommunloop

  retur_df <- resultat %>% list_rbind()
  if (visa_resultat_meddelanden_konsol) {
    kommun_txt <- if (n_distinct(retur_df$`Säteskommun`) > 1) "kommuner" else "kommun"
    message(glue("📊 Totalt hämtades {scb_format(nrow(retur_df))} företag från {length(kommunkoder)} {kommun_txt} på {scb_sec(total_start)} sekunder"))
  }
  retur_df
}

scb_hamta_arbetsstallen <- function(
    kommunkoder = NULL,               # kommunkoder för de kommuner man vill ha data för, en eller flera i en vektor, tex: c("2080", "2085")
    grundfilter = list(),             # möjlighet att skicka med fler variabler för filtrering, görs i form av en namngiven list, tex. list(list(Kategori="SätesKommun", Kod=list("2080")), ...)
    anstallda = NULL,                 # default är NULL = alla anställda-klasser - använd scb_kategorier_med_kodtabeller("arbetsstallen") för att se vilka värden som finns
    branscher = NULL,                 # default är NULL = alla branscher - använd scb_kategorier_med_kodtabeller("arbetsstallen") för att se vilka värden som finns
    vald_arbetsstallestatus = 1,      # default är 1 = är verksam (finns också 0 = har aldrig varit verksam och 9 = är ej längre verksam)
    cert_thumb = scb_hamta_cert_thumb(),         # thumb från installerat certifikat, kan skickas med men behöver oftast inte göras
    visa_meddelanden_konsol = FALSE,             # detaljerade meddelanden om vad som händer
    visa_resultat_meddelanden_konsol = TRUE,     # meddelanden om vad som laddats ned
    visa_progress = TRUE                         # progress-bar för om man har flera kommuner
) {
  total_start <- Sys.time()
  library(tidyverse); library(glue)

  base_url <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/"
  rakna_url <- paste0(base_url, "ae/raknaarbetsstallen/")
  hamta_url <- paste0(base_url, "ae/hamtaarbetsstallen/")
  kategorier_url <- paste0(base_url, "ae/kategoriermedkodtabeller")
  max_rader <- 2000

  .api_calls <- new.env(); .api_calls$timestamps <- numeric(0)

  total_kombinationer <- length(kommunkoder)
  if (visa_progress) {
    pb <- progress::progress_bar$new(total = total_kombinationer, format = ":current/:total [:bar] :percent :message", clear = FALSE)
  } else {
    pb <- list(tick = function(...) NULL)
  }

  # --- resultat
  resultat <- list()

  resultat_lagg_till <- local({
    .res_idx <- 0L
    function(df, label = NULL) {
      if (is.null(df) || !nrow(df)) return(invisible(NULL))
      .res_idx <<- .res_idx + 1L
      resultat[[.res_idx]] <<- df
      if (!is.null(label)) {
        if (visa_meddelanden_konsol) message(sprintf("Lade till lista #%d (%s): %d rader", .res_idx, label, nrow(df)))
      } else {
        if (visa_meddelanden_konsol) message(sprintf("Lade till lista #%d: %d rader", .res_idx, nrow(df)))
      }
      invisible(NULL)
    }
  })

  rakna <- function(payload) rakna_api(rakna_url, payload, cert_thumb, .api_calls, visa_progress, visa_meddelanden_konsol)
  hamta <- function(payload) hamta_api(hamta_url, payload, cert_thumb, .api_calls, visa_progress, visa_meddelanden_konsol)

  skapa_payload <- function(kom = NULL, anst = NULL, bransch = NULL) {
    kat <- list()
    if (!is.null(kom))     kat <- append(kat, list(list(Kategori = "Kommun",               Kod = list(kom))))
    if (!is.null(anst))    kat <- append(kat, list(list(Kategori = "Anställda",            Kod = as.list(anst))))
    if (!is.null(bransch)) kat <- append(kat, list(list(Kategori = "2-siffrig bransch 1",  Kod = as.list(bransch))))
    payload <- list(Arbetsställestatus = vald_arbetsstallestatus, Kategorier = kat)
    if (length(grundfilter) > 0) payload$variabler <- grundfilter
    payload
  }

  hamta_kategorier <- function() {
    if (visa_progress && visa_meddelanden_konsol) message("📋 Hämtar tillgängliga kategorier för automatisk uppdelning...")
    resp <- httr::GET(url = kategorier_url, config = httr::config(sslcert = paste0(cert_info$store, "\\MY\\", cert_thumb), sslcerttype = "Schannel"))
    if (resp$status_code != 200) stop("❌ Kunde inte hämta kategorier från API:t")
    kategorier <- httr::content(resp, as = "parsed", encoding = "UTF-8")
    anst_kategori <- NULL; bransch_kategori <- NULL
    for (kat in kategorier) {
      if ("anställda" == tolower(kat$Id_Kategori_AE)) anst_kategori <- kat
      if (grepl("2-siffrig bransch 1", tolower(kat$Id_Kategori_AE))) bransch_kategori <- kat
    }
    anst_koder <- anst_kategori$VardeLista %>% keep(~ is.list(.x) && !is.null(.x$Varde)) %>% map_chr(~ as.character(.x$Varde))
    bransch_koder <- bransch_kategori$VardeLista %>% keep(~ is.list(.x) && !is.null(.x$Varde)) %>% map_chr(~ as.character(.x$Varde))
    if (visa_progress && visa_meddelanden_konsol) message(sprintf("✅ Hämtade %d anställda-grupper och %d branscher", length(anst_koder), length(bransch_koder)))
    list(anst = anst_koder, bransch = bransch_koder)
  }

  auto_kategorier <- NULL

  for (kom in kommunkoder %||% list(NULL)) {
    kommun_start <- Sys.time()
    pb$tick(tokens = list(message = kom))
    if (visa_progress && visa_meddelanden_konsol) message(sprintf("🔍 Kommun %s: startar hämtning", kom))

    if (is.null(auto_kategorier)) auto_kategorier <- hamta_kategorier()
    aktuell_anst     <- anstallda %||% auto_kategorier$anst
    aktuell_bransch  <- branscher %||% auto_kategorier$bransch

    # Kan vi hämta allt i kommunen direkt?
    payload_kom <- skapa_payload(kom = kom)
    n_kom <- rakna(payload_kom)
    if (!is.na(n_kom) && n_kom <= max_rader) {
      res <- hamta(payload_kom)
      if (!is.null(res)) {
        resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
        if (visa_resultat_meddelanden_konsol) {
          kommun_namn <- api_kommunnamn_fran_resultat(resultat, kom, "Kommun") %||% kom
          antal <- api_summa_for_kom(resultat, kom)
          message(glue("🏁 {kommun_namn}: hämtade {scb_format(antal)} arbetsställen på {scb_sec(kommun_start)} sekunder"))
        }
        next
      }
    }

    # Steg 1: försök per anställda-grupper
    aktuell_anst_lista <- map(seq_along(aktuell_anst), ~ aktuell_anst[.x:length(aktuell_anst)])
    lyckade_anst <- character(0)

    repeat {
      hittade <- FALSE
      aktuell_anst_lista <- map(aktuell_anst_lista, ~ setdiff(.x, lyckade_anst)) %>% keep(~ length(.x) > 0)

      for (anst_grupp in aktuell_anst_lista) {
        if (all(anst_grupp %in% lyckade_anst)) next

        if (length(anst_grupp) == 2) {
          for (anst_kod in anst_grupp) {
            if (anst_kod %in% lyckade_anst) next
            n <- rakna(skapa_payload(kom = kom, anst = anst_kod))
            if (visa_meddelanden_konsol) message(glue("🔢 anst-kod {anst_kod} → {n} arbetsställen\n"))
            if (!is.na(n) && n <= max_rader) {
              res <- hamta(skapa_payload(kom = kom, anst = anst_kod))
              if (!is.null(res)) {
                resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
                lyckade_anst <- union(lyckade_anst, anst_kod)
                hittade <- TRUE; break
              }
            }
          }
        } else {
          kvarvarande <- setdiff(anst_grupp, lyckade_anst)
          if (!length(kvarvarande)) next
          n <- rakna(skapa_payload(kom = kom, anst = kvarvarande))
          if (visa_meddelanden_konsol) message(glue("Räknar rader i uttag: {n} stycken.\n"))
          if (!is.na(n) && n <= max_rader) {
            res <- hamta(skapa_payload(kom = kom, anst = kvarvarande))
            if (!is.null(res)) {
              resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
              lyckade_anst <- union(lyckade_anst, kvarvarande)
              hittade <- TRUE; break
            }
          }
        }
      }
      if (!hittade) break
    }

    # Steg 2: bryt upp i branscher
    koder_att_bryta_upp <- setdiff(aktuell_anst, lyckade_anst)

    dela_upp_branschgrupp <- function(bransch_koder, kom, anst_kod, max_rader, lyckade_branscher) {
      if (!length(bransch_koder)) return(list(lyckade_branscher = lyckade_branscher, hittade = FALSE))
      n <- rakna(skapa_payload(kom = kom, anst = anst_kod, bransch = bransch_koder))
      if (length(bransch_koder) == 1) {
        if (visa_meddelanden_konsol) message(glue("🔢 anst {anst_kod} + Bransch {bransch_koder} → {n} arbetsställen\n"))
      } else {
        if (visa_meddelanden_konsol) message(glue("🔢 anst {anst_kod} + Branscher {min(bransch_koder)}-{max(bransch_koder)} → {n} arbetsställen\n"))
      }
      if (!is.na(n) && n <= max_rader) {
        res <- hamta(skapa_payload(kom = kom, anst = anst_kod, bransch = bransch_koder))
        if (!is.null(res)) {
          resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
          lyckade_branscher <- union(lyckade_branscher, bransch_koder)
          return(list(lyckade_branscher = lyckade_branscher, hittade = TRUE))
        }
      } else if (!is.na(n) && n > max_rader && length(bransch_koder) > 1) {
        if (visa_meddelanden_konsol) message(glue("Gruppen är för stor ({n} rader), delar upp...\n"))
        mitt <- ceiling(length(bransch_koder) / 2)
        r1 <- dela_upp_branschgrupp(bransch_koder[1:mitt], kom, anst_kod, max_rader, lyckade_branscher)
        r2 <- dela_upp_branschgrupp(bransch_koder[(mitt+1):length(bransch_koder)], kom, anst_kod, max_rader, r1$lyckade_branscher)
        return(list(lyckade_branscher = r2$lyckade_branscher, hittade = r1$hittade || r2$hittade))
      }
      list(lyckade_branscher = lyckade_branscher, hittade = FALSE)
    }

    for (anst_kod in koder_att_bryta_upp) {
      if (visa_meddelanden_konsol) message(glue("\n🔍 Delar upp anst-kod {anst_kod} i branscher...\n"))
      bransch_grupper <- split(aktuell_bransch, cut(seq_along(aktuell_bransch), breaks = 5, labels = FALSE))
      aktuell_bransch_lista <- map(seq_along(bransch_grupper), function(i) flatten_chr(bransch_grupper[i:length(bransch_grupper)])) %>%
        set_names(map_chr(seq_along(bransch_grupper), function(i) { alla <- flatten_chr(bransch_grupper[i:length(bransch_grupper)]); sprintf("%s-%s", min(alla), max(alla)) }))
      lyckade_branscher <- character(0)

      repeat {
        hittade <- FALSE
        aktuell_bransch_lista <- map(aktuell_bransch_lista, ~ setdiff(.x, lyckade_branscher)) %>% keep(~ length(.x) > 0)
        for (namn in names(aktuell_bransch_lista)) {
          kvarvarande <- setdiff(aktuell_bransch_lista[[namn]], lyckade_branscher)
          if (!length(kvarvarande)) next
          if (length(kvarvarande) == 2) {
            for (br_kod in kvarvarande) {
              n <- rakna(skapa_payload(kom = kom, anst = anst_kod, bransch = br_kod))
              if (visa_meddelanden_konsol) message(glue("🔢 anst {anst_kod} + Bransch {br_kod} → {n} arbetsställen\n"))
              if (!is.na(n) && n <= max_rader) {
                res <- hamta(skapa_payload(kom = kom, anst = anst_kod, bransch = br_kod))
                if (!is.null(res)) {
                  resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
                  lyckade_branscher <- union(lyckade_branscher, br_kod)
                  hittade <- TRUE; break
                }
              }
            }
          } else {
            n <- rakna(skapa_payload(kom = kom, anst = anst_kod, bransch = kvarvarande))
            if (!is.na(n) && n <= max_rader) {
              res <- hamta(skapa_payload(kom = kom, anst = anst_kod, bransch = kvarvarande))
              if (!is.null(res)) {
                resultat_lagg_till(res %>% mutate(kod_kommun = kom), label = "kommun")
                lyckade_branscher <- union(lyckade_branscher, kvarvarande)
                hittade <- TRUE; break
              }
            } else if (!is.na(n)) {
              del <- dela_upp_branschgrupp(kvarvarande, kom, anst_kod, max_rader, lyckade_branscher)
              lyckade_branscher <- del$lyckade_branscher
              if (del$hittade) hittade <- TRUE
            }
          }
        }
        if (!hittade) break
        if (length(aktuell_bransch_lista) > 0)
          aktuell_bransch_lista <- aktuell_bransch_lista %>% imap(~ .x) %>% set_names(map_chr(., ~ sprintf("%s-%s", min(.x), max(.x))))
      }
    }

    if (visa_resultat_meddelanden_konsol) {
      kommun_namn <- api_kommunnamn_fran_resultat(resultat, kom, "Kommun") %||% kom
      antal <- api_summa_for_kom(resultat, kom)
      message(glue("🏁 {kommun_namn}: hämtade {scb_format(antal)} arbetsställen på {scb_sec(kommun_start)} sekunder"))
    }
  } # kommunloop

  retur_df <- resultat %>% list_rbind()
  if (visa_resultat_meddelanden_konsol) {
    kommun_txt <- if (n_distinct(retur_df$Kommun) > 1) "kommuner" else "kommun"
    message(glue("📊 Totalt hämtades {scb_format(nrow(retur_df))} arbetsställen från {length(kommunkoder)} {kommun_txt} på {scb_sec(total_start)} sekunder"))
  }
  retur_df
} # slut funktion, scb_hamta_arbetsstallen()

#
# Stänger av dessa funktioner pga blev ändå fel då enskilda postnummer numera har fler än 2000 rader.
#
# # ===================================================================================================
# #
# # Skript som hämtar alla företag i hela Sverige, funktion som returnerar en tibble. Skriptet är en
# # vidareutveckling av ett skript som Johan Jonssons, Region Örebro, har skrivit. Framförallt hanteras
# # att vissa postnummerintervall har fått fler än 2000 rader, i dessa fall delas uttaget upp i två
# # enskilda uttag. Jag har försökt optimera litegrann också men det tar lång tid att hämta alla företag
# # i Sverige. Ett nytt API planeras vara på plats i februari 2026 med reservation för att det kan ta
# # längre tid.
# #
# # Skapat av: Johan Jonsson, Region Örebro
# # Reviderat av: Peter Möller, Region Dalarna
# #               1 dec 2025
# #
# # ===================================================================================================
#
#
#
# library(httr)
# library(rlang)
# library(data.table)
# library(dplyr)
# library(stringr)
# library(tibble)
# library(logger)
# library(purrr)
#
# source("https://raw.githubusercontent.com/Analytikernatverket/hamta_data/refs/heads/main/ftg_arbst_api_scb.R")
#
#
# # Global enkel "sliding window" limiter: högst 10 calls per 10 # max calls under fönstret# Global enkel "sliding window" limiter: högst 10 calls per 10 sek
# .call_times  <- numeric(0)  # timestamps (sek) för senaste anropen
# .rate_window <- 10          # sekunder
# .rate_limit  <- 10   # max anrop per fönster
#
# .enforce_rate_limit <- function() {
#   now <- as.numeric(Sys.time())
#   # Rensa bort timestamps äldre än fönstret
#   .call_times <<- .call_times[.call_times > (now - .rate_window)]
#   if (length(.call_times) >= .rate_limit) {
#     # Beräkna hur länge vi måste vänta tills ett "slot" frigörs
#     wait_sec <- (.call_times[1] + .rate_window) - now
#     if (wait_sec > 0) {
#       log_info("Rate limiter: väntar {round(wait_sec, 2)} sek")
#       Sys.sleep(wait_sec)
#     }
#     # Uppdatera igen efter väntan
#     now <- as.numeric(Sys.time())
#     .call_times <<- .call_times[.call_times > (now - .rate_window)]
#   }
#   # Registrera detta anrop
#   .call_times <<- c(.call_times, now)
# }
#
# # Exponentiell backoff med jitter och stöd för Retry-After
# do_request <- function(verb = POST, url, headers = NULL, body = NULL, encode = "json",
#                        cfg = NULL, max_retries = 5, base_delay = 1.0, max_delay = 15.0) {
#   attempt <- 0
#   repeat {
#     attempt <- attempt + 1
#
#     # Proaktivt rate-limit innan vi skickar
#     .enforce_rate_limit()
#
#     # Skicka request
#     resp <- tryCatch({
#       verb(url,
#            headers %||% add_headers(`Content-Type` = "application/json", Accept = "application/json"),
#            body   = body,
#            encode = encode,
#            cfg)
#     }, error = function(e) {
#       # Nätverksfel, behandla som 500/503: backoff
#       structure(list(error = e), class = "try-error")
#     })
#
#     # Hantera nätverksfel
#     if (inherits(resp, "try-error")) {
#       if (attempt > max_retries) {
#         stop(sprintf("Nätverksfel efter %d försök: %s", attempt - 1, resp$error$message))
#       } else {
#         delay <- min(max_delay, base_delay * 2^(attempt - 1)) * runif(1, 0.8, 1.2)
#         log_warn("Nätverksfel, försök {attempt}/{max_retries}. Väntar {round(delay,2)} s")
#         Sys.sleep(delay)
#         next
#       }
#     }
#
#     status <- status_code(resp)
#
#     # OK: returnera svaret
#     if (status >= 200 && status < 300) {
#       return(resp)
#     }
#
#     # Om 429 eller 5xx: försök igen med backoff (respekt för Retry-After)
#     if (status == 429 || (status >= 500 && status < 600)) {
#       if (attempt > max_retries) {
#         stop(sprintf("HTTP %d efter %d försök: %s", status, attempt - 1, http_status(resp)$message))
#       }
#       # Kolla Retry-After (sekunder)
#       ra <- headers(resp)$`retry-after`
#       if (!is.null(ra)) {
#         delay <- suppressWarnings(as.numeric(ra))
#         # Om inte numerisk, defaulta till exponentiell backoff
#         if (is.na(delay)) {
#           delay <- min(max_delay, base_delay * 2^(attempt - 1)) * runif(1, 0.8, 1.2)
#         }
#       } else {
#         delay <- min(max_delay, base_delay * 2^(attempt - 1)) * runif(1, 0.8, 1.2)
#       }
#       log_warn("HTTP {status}. Backoff: väntar {round(delay,2)} s (försök {attempt}/{max_retries})")
#       Sys.sleep(delay)
#       next
#     }
#
#     # Annat fel (4xx som inte är 429): stoppa direkt
#     stop(sprintf("HTTP %d: %s", status, http_status(resp)$message))
#   }
# }
#
#
#
# # en reviderad version av Johan Jonssons skript för att hämta alla företag i hela Sverige
# scb_hamta_foretag_hela_sverige <- function() {
#   start_tid <- Sys.time()
#
#   cert_info <- scb_hamta_cert()
#
#   postnr_grupper <- data.frame(PostNr_start = c("10000",
#                                                 "10131", "10235", "10261", "10338", "10391", "10430", "10539",
#                                                 "10631", "10637", "10776", "11120", "11123", "11130", "11137",
#                                                 "11143", "11147", "11157", "11175", "11220", "11225", "11233",
#                                                 "11241", "11249", "11259", "11267", "11323", "11328", "11335",
#                                                 "11342", "11348", "11353", "11358", "11362", "11418", "11426",
#                                                 "11431", "11435", "11440", "11446", "11451", "11455", "11459",
#                                                 "11522", "11528", "11537", "11547", "11593", "11625", "11633",
#                                                 "11640", "11649", "11668", "11730", "11738", "11758", "11765",
#                                                 "11822", "11829", "11849", "11855", "11864", "12030", "12048",
#                                                 "12060", "12069", "12133", "12145", "12155", "12241", "12262",
#                                                 "12346", "12361", "12456", "12476", "12544", "12572", "12632",
#                                                 "12648", "12730", "12744", "12835", "12863", "12935", "12946",
#                                                 "13043", "13133", "13142", "13150", "13160", "13234", "13240",
#                                                 "13248", "13334", "13343", "13443", "13469", "13546", "13555",
#                                                 "13625", "13649", "13666", "13731", "13763", "13831", "13934",
#                                                 "13954", "14132", "14140", "14146", "14169", "14191", "14260",
#                                                 "14440", "14556", "14575", "14653", "14753", "14891", "14942",
#                                                 "15146", "15166", "15243", "15330", "15395", "16126", "16263",
#                                                 "16343", "16354", "16372", "16441", "16479", "16571", "16732",
#                                                 "16753", "16767", "16839", "16855", "16867", "16903", "16941",
#                                                 "16965", "17061", "17104", "17149", "17165", "17237", "17275",
#                                                 "17457", "17549", "17569", "17734", "17757", "17836", "17893",
#                                                 "18121", "18135", "18145", "18157", "18166", "18235", "18254",
#                                                 "18266", "18321", "18355", "18368", "18433", "18444", "18463",
#                                                 "18494", "18541", "18637", "18670", "18730", "18746", "18767",
#                                                 "19127", "19142", "19150", "19252", "19270", "19332", "19436",
#                                                 "19455", "19479", "19553", "19592", "19638", "19793", "20122",
#                                                 "21114", "21121", "21131", "21141", "21149", "21175", "21223",
#                                                 "21233", "21362", "21377", "21431", "21446", "21535", "21583",
#                                                 "21617", "21625", "21742", "21754", "21774", "21871", "22223",
#                                                 "22350", "22421", "22478", "22651", "23053", "23166", "23194",
#                                                 "23252", "23363", "23436", "23543", "23635", "23691", "23837",
#                                                 "24123", "24165", "24234", "24297", "24391", "24465", "24543",
#                                                 "24632", "24734", "24791", "25110", "25224", "25251", "25353",
#                                                 "25433", "25450", "25468", "25654", "25733", "26144", "26192",
#                                                 "26255", "26293", "26358", "26391", "26454", "26572", "26692",
#                                                 "26774", "26876", "26944", "26996", "27151", "27192", "27293",
#                                                 "27392", "27450", "27536", "27572", "27735", "28141", "28232",
#                                                 "28276", "28391", "28537", "28693", "28942", "29130", "29154",
#                                                 "29169", "29198", "29436", "29537", "29641", "29894", "30235",
#                                                 "30247", "30269", "30560", "30594", "31137", "31161", "31172",
#                                                 "31198", "31260", "31296", "31397", "31496", "33152", "33193",
#                                                 "33331", "33391", "33572", "34137", "34174", "34230", "34262",
#                                                 "34371", "35104", "35235", "35245", "35256", "35575", "36131",
#                                                 "36252", "36298", "36394", "36531", "37134", "37162", "37261",
#                                                 "37332", "37361", "37434", "37530", "38273", "38430", "38630",
#                                                 "38694", "38792", "38897", "39235", "39351", "39477", "40015",
#                                                 "40053", "40242", "40523", "41110", "41120", "41128", "41137",
#                                                 "41252", "41261", "41271", "41302", "41311", "41322", "41451",
#                                                 "41462", "41475", "41515", "41562", "41653", "41665", "41702",
#                                                 "41717", "41748", "41767", "41874", "42139", "42165", "42246",
#                                                 "42336", "42354", "42441", "42530", "42668", "42677", "42831",
#                                                 "42941", "43130", "43140", "43161", "43232", "43244", "43263",
#                                                 "43276", "43295", "43338", "43362", "43431", "43446", "43496",
#                                                 "43538", "43634", "43651", "43740", "43853", "43898", "43954",
#                                                 "43974", "44131", "44156", "44191", "44230", "44246", "44275",
#                                                 "44297", "44339", "44373", "44455", "44502", "44697", "44834",
#                                                 "45115", "45154", "45191", "45198", "45295", "45541", "45651",
#                                                 "45750", "45870", "45994", "46144", "46191", "46237", "46296",
#                                                 "46433", "46495", "46795", "47172", "47295", "47493", "47542",
#                                                 "50336", "50453", "50494", "50730", "51153", "51192", "51260",
#                                                 "51350", "51463", "51692", "51831", "51997", "52156", "52194",
#                                                 "52292", "52360", "52396", "52495", "53141", "53191", "53199",
#                                                 "53294", "53394", "53491", "54126", "54193", "54243", "54336",
#                                                 "54494", "54672", "54942", "54998", "55310", "55323", "55448",
#                                                 "55475", "56131", "56162", "56241", "56433", "56597", "56733",
#                                                 "57134", "57173", "57240", "57322", "57391", "57437", "57476",
#                                                 "57537", "57636", "57771", "57891", "58106", "58224", "58244",
#                                                 "58275", "58391", "58571", "58596", "58735", "58957", "59046",
#                                                 "59152", "59194", "59330", "59373", "59494", "59592", "59731",
#                                                 "59870", "59931", "60210", "60225", "60245", "60367", "60591",
#                                                 "61031", "61137", "61168", "61197", "61293", "61492", "61595",
#                                                 "61830", "61991", "62145", "62158", "62254", "62344", "62377",
#                                                 "63102", "63230", "63348", "63506", "63535", "64153", "64237",
#                                                 "64393", "64532", "64592", "64692", "64792", "65218", "65225",
#                                                 "65341", "65450", "65591", "65632", "66193", "66291", "66342",
#                                                 "66494", "66593", "66730", "66930", "67152", "67195", "67241",
#                                                 "67296", "67398", "68142", "68234", "68394", "68592", "68635",
#                                                 "68696", "69133", "69193", "69293", "69472", "69630", "70135",
#                                                 "70216", "70227", "70234", "70342", "70361", "70378", "70594",
#                                                 "71135", "71330", "71493", "71693", "71995", "72131", "72211",
#                                                 "72221", "72337", "72356", "72471", "72592", "72630", "73150",
#                                                 "73247", "73336", "73396", "73531", "73748", "74047", "74171",
#                                                 "74194", "74292", "74373", "74491", "74560", "74621", "74693",
#                                                 "74830", "74948", "74971", "75184", "75237", "75272", "75319",
#                                                 "75326", "75337", "75435", "75454", "75591", "75645", "75655",
#                                                 "76111", "76141", "76173", "76198", "76296", "76491", "77152",
#                                                 "77461", "77596", "77693", "78064", "78162", "78197", "78330",
#                                                 "78437", "78472", "78631", "79015", "79023", "79131", "79150",
#                                                 "79175", "79232", "79277", "79297", "79360", "79430", "79535",
#                                                 "79597", "80006", "80266", "80310", "80595", "80647", "81171",
#                                                 "81294", "81538", "81694", "81895", "82141", "82193", "82392",
#                                                 "82455", "82478", "82596", "82675", "82735", "82763", "82893",
#                                                 "82996", "83134", "83149", "83188", "83255", "83349", "83497",
#                                                 "83591", "83671", "83752", "83797", "84195", "84393", "84497",
#                                                 "84593", "84695", "85104", "85123", "85232", "85357", "85597", "85730",
#                                                 "86191", "86296", "86495", "87133", "87193", "87340", "88135",
#                                                 "88370", "89140", "89195", "89392", "89592", "90102", "90326",
#                                                 "90339", "90364", "90441", "90591", "90638", "90753", "91231",
#                                                 "91342", "91591", "91731", "92132", "92231", "92331", "92532",
#                                                 "93137", "93156", "93194", "93252", "93395", "93498", "93732",
#                                                 "93993", "94151", "94292", "94492", "94692", "95291", "95423",
#                                                 "95691", "96136", "96178", "96233", "97236", "97341", "97455",
#                                                 "97596", "98107", "98146", "98239", "98492"),
#                                PostNr_slut = c("10130",
#                                                "10234", "10260", "10337", "10390", "10429", "10538", "10630",
#                                                "10636", "10775", "11119", "11122", "11129", "11136", "11142",
#                                                "11146", "11156", "11174", "11219", "11224", "11232", "11240",
#                                                "11248", "11258", "11266", "11322", "11327", "11334", "11341",
#                                                "11347", "11352", "11357", "11361", "11417", "11425", "11430",
#                                                "11434", "11439", "11445", "11450", "11454", "11458", "11521",
#                                                "11527", "11536", "11546", "11592", "11624", "11632", "11639",
#                                                "11648", "11667", "11729", "11737", "11757", "11764", "11821",
#                                                "11828", "11848", "11854", "11863", "12029", "12047", "12059",
#                                                "12068", "12132", "12144", "12154", "12240", "12261", "12345",
#                                                "12360", "12455", "12475", "12543", "12571", "12631", "12647",
#                                                "12729", "12743", "12834", "12862", "12934", "12945", "13042",
#                                                "13132", "13141", "13149", "13159", "13233", "13239", "13247",
#                                                "13333", "13342", "13442", "13468", "13545", "13554", "13624",
#                                                "13648", "13665", "13730", "13762", "13830", "13933", "13953",
#                                                "14131", "14139", "14145", "14168", "14190", "14259", "14439",
#                                                "14555", "14574", "14652", "14752", "14890", "14941", "15145",
#                                                "15165", "15242", "15329", "15394", "16125", "16262", "16342",
#                                                "16353", "16371", "16440", "16478", "16570", "16731", "16752",
#                                                "16766", "16838", "16854", "16866", "16902", "16940", "16964",
#                                                "17060", "17103", "17148", "17164", "17236", "17274", "17456",
#                                                "17548", "17568", "17733", "17756", "17835", "17892", "18120",
#                                                "18134", "18144", "18156", "18165", "18234", "18253", "18265",
#                                                "18320", "18354", "18367", "18432", "18443", "18462", "18493",
#                                                "18540", "18636", "18669", "18729", "18745", "18766", "19126",
#                                                "19141", "19149", "19251", "19269", "19331", "19435", "19454",
#                                                "19478", "19552", "19591", "19637", "19792", "20121", "21113",
#                                                "21120", "21130", "21140", "21148", "21174", "21222", "21232",
#                                                "21361", "21376", "21430", "21445", "21534", "21582", "21616",
#                                                "21624", "21741", "21753", "21773", "21870", "22222", "22349",
#                                                "22420", "22477", "22650", "23052", "23165", "23193", "23251",
#                                                "23362", "23435", "23542", "23634", "23690", "23836", "24122",
#                                                "24164", "24233", "24296", "24390", "24464", "24542", "24631",
#                                                "24733", "24790", "25109", "25223", "25250", "25352", "25432",
#                                                "25449", "25467", "25653", "25732", "26143", "26191", "26254",
#                                                "26292", "26357", "26390", "26453", "26571", "26691", "26773",
#                                                "26875", "26943", "26995", "27150", "27191", "27292", "27391",
#                                                "27449", "27535", "27571", "27734", "28140", "28231", "28275",
#                                                "28390", "28536", "28692", "28941", "29129", "29153", "29168",
#                                                "29197", "29435", "29536", "29640", "29893", "30234", "30246",
#                                                "30268", "30559", "30593", "31136", "31160", "31171", "31197",
#                                                "31259", "31295", "31396", "31495", "33151", "33192", "33330",
#                                                "33390", "33571", "34136", "34173", "34229", "34261", "34370",
#                                                "35103", "35234", "35244", "35255", "35574", "36130", "36251",
#                                                "36297", "36393", "36530", "37133", "37161", "37260", "37331",
#                                                "37360", "37433", "37529", "38272", "38429", "38629", "38693",
#                                                "38791", "38896", "39234", "39350", "39476", "40014", "40052",
#                                                "40241", "40522", "41109", "41119", "41127", "41136", "41251",
#                                                "41260", "41270", "41301", "41310", "41321", "41450", "41461",
#                                                "41474", "41514", "41561", "41652", "41664", "41701", "41716",
#                                                "41747", "41766", "41873", "42138", "42164", "42245", "42335",
#                                                "42353", "42440", "42529", "42667", "42676", "42830", "42940",
#                                                "43129", "43139", "43160", "43231", "43243", "43262", "43275",
#                                                "43294", "43337", "43361", "43430", "43445", "43495", "43537",
#                                                "43633", "43650", "43739", "43852", "43897", "43953", "43973",
#                                                "44130", "44155", "44190", "44229", "44245", "44274", "44296",
#                                                "44338", "44372", "44454", "44501", "44696", "44833", "45114",
#                                                "45153", "45190", "45197", "45294", "45540", "45650", "45749",
#                                                "45869", "45993", "46143", "46190", "46236", "46295", "46432",
#                                                "46494", "46794", "47171", "47294", "47492", "47541", "50335",
#                                                "50452", "50493", "50729", "51152", "51191", "51259", "51349",
#                                                "51462", "51691", "51830", "51996", "52155", "52193", "52291",
#                                                "52359", "52395", "52494", "53140", "53190", "53198", "53293",
#                                                "53393", "53490", "54125", "54192", "54242", "54335", "54493",
#                                                "54671", "54941", "54997", "55309", "55322", "55447", "55474",
#                                                "56130", "56161", "56240", "56432", "56596", "56732", "57133",
#                                                "57172", "57239", "57321", "57390", "57436", "57475", "57536",
#                                                "57635", "57770", "57890", "58105", "58223", "58243", "58274",
#                                                "58390", "58570", "58595", "58734", "58956", "59045", "59151",
#                                                "59193", "59329", "59372", "59493", "59591", "59730", "59869",
#                                                "59930", "60209", "60224", "60244", "60366", "60590", "61030",
#                                                "61136", "61167", "61196", "61292", "61491", "61594", "61829",
#                                                "61990", "62144", "62157", "62253", "62343", "62376", "63101",
#                                                "63229", "63347", "63505", "63534", "64152", "64236", "64392",
#                                                "64531", "64591", "64691", "64791", "65217", "65224", "65340",
#                                                "65449", "65590", "65631", "66192", "66290", "66341", "66493",
#                                                "66592", "66729", "66929", "67151", "67194", "67240", "67295",
#                                                "67397", "68141", "68233", "68393", "68591", "68634", "68695",
#                                                "69132", "69192", "69292", "69471", "69629", "70134", "70215",
#                                                "70226", "70233", "70341", "70360", "70377", "70593", "71134",
#                                                "71329", "71492", "71692", "71994", "72130", "72210", "72220",
#                                                "72336", "72355", "72470", "72591", "72629", "73149", "73246",
#                                                "73335", "73395", "73530", "73747", "74046", "74170", "74193",
#                                                "74291", "74372", "74490", "74559", "74620", "74692", "74829",
#                                                "74947", "74970", "75183", "75236", "75271", "75318", "75325",
#                                                "75336", "75434", "75453", "75590", "75644", "75654", "76110",
#                                                "76140", "76172", "76197", "76295", "76490", "77151", "77460",
#                                                "77595", "77692", "78063", "78161", "78196", "78329", "78436",
#                                                "78471", "78630", "79014", "79022", "79130", "79149", "79174",
#                                                "79231", "79276", "79296", "79359", "79429", "79534", "79596",
#                                                "80005", "80265", "80309", "80594", "80646", "81170", "81293",
#                                                "81537", "81693", "81894", "82140", "82192", "82391", "82454",
#                                                "82477", "82595", "82674", "82734", "82762", "82892", "82995",
#                                                "83133", "83148", "83187", "83254", "83348", "83496", "83590",
#                                                "83670", "83751", "83796", "84194", "84392", "84496", "84592",
#                                                "84694", "85103", "85122", "85231", "85356", "85596", "85729", "86190",
#                                                "86295", "86494", "87132", "87192", "87339", "88134", "88369",
#                                                "89139", "89194", "89391", "89591", "90101", "90325", "90338",
#                                                "90363", "90440", "90590", "90637", "90752", "91230", "91341",
#                                                "91590", "91730", "92131", "92230", "92330", "92531", "93136",
#                                                "93155", "93193", "93251", "93394", "93497", "93731", "93992",
#                                                "94150", "94291", "94491", "94691", "95290", "95422", "95690",
#                                                "96135", "96177", "96232", "97235", "97340", "97454", "97595",
#                                                "98106", "98145", "98238", "98491", "99999"))
#
#   # Förbehandla postnummer en gång
#   postnr_grupper$PostNr_start <- str_pad(postnr_grupper$PostNr_start, 5, "0")
#   postnr_grupper$PostNr_slut  <- str_pad(postnr_grupper$PostNr_slut, 5, "0")
#
#   # httr-konfig
#   cfg <- config(
#     sslcert     = paste0(cert_info$store, "\\MY\\", cert_info$thumb),
#     sslcerttype = "Schannel"
#   )
#
#   # Hjälpare: skapa JSON-kropp
#   make_body <- function(start, slut) {
#     list(
#       Företagsstatus      = "1",
#       Registreringsstatus = "1",
#       variabler = list(list(
#         Varde1   = start,
#         Varde2   = slut,
#         Operator = "Mellan",
#         Variabel = "Postnr"
#       ))
#     )
#   }
#
#   # Hjälpare: räkna antal företag
#   scb_count <- function(start, slut) {
#     body <- make_body(start, slut)
#     resp <- do_request(
#       verb = POST,
#       url = "https://privateapi.scb.se/nv0101/v1/sokpavar/api/je/raknaforetag/",
#       headers = add_headers(`Content-Type` = "application/json", Accept = "application/json"),
#       body   = body,
#       encode = "json",
#       cfg = cfg,
#       max_retries = 5,
#       base_delay = 1.0,
#       max_delay = 15.0
#     ) %>%
#       stop_for_status() %>%
#       content()
#
#     as.numeric(resp)
#   }
#
#   # Hjälpare: hämta företag
#   scb_fetch <- function(start, slut) {
#     body <- make_body(start, slut)
#     resp <- do_request(
#       verb = POST,
#       url = "https://privateapi.scb.se/nv0101/v1/sokpavar/api/je/hamtaforetag/",
#       headers = add_headers(`Content-Type` = "application/json", Accept = "application/json"),
#       body   = body,
#       encode = "json",
#       cfg = cfg,
#       max_retries = 5,
#       base_delay = 1.0,
#       max_delay = 15.0
#     ) %>%
#       stop_for_status() %>%
#       content(simplifyVector = TRUE) %>%
#       as_tibble()
#
#     resp
#   }
#
#   # Rekursiv funktion: delar intervallet tills varje del har <= 2000 företag
#   fetch_interval <- function(start, slut, max_per_chunk = 2000) {
#     n <- scb_count(start, slut)
#     log_info("Postnummer {start}-{slut}: {n} företag")
#
#     if (is.na(n)) {
#       log_warn("Okänt antal för intervall {start}-{slut}, hämtar ändå.")
#       return(scb_fetch(start, slut))
#     }
#
#     if (n <= max_per_chunk) {
#       return(scb_fetch(start, slut))
#     } else {
#       mid <- floor((as.integer(start) + as.integer(slut)) / 2)
#       left_end    <- str_pad(mid,      5, "0")
#       right_start <- str_pad(mid + 1L, 5, "0")
#
#       log_info("Delar intervallet {start}-{slut} i två: {start}-{left_end} och {right_start}-{slut}")
#
#       left_df  <- fetch_interval(start, left_end,  max_per_chunk)
#       right_df <- fetch_interval(right_start, slut, max_per_chunk)
#
#       return(bind_rows(left_df, right_df))
#     }
#   }
#
#   # ---- Samla resultat i lista ----
#   je <- map(seq_len(nrow(postnr_grupper)), function(i) {
#     fetch_interval(postnr_grupper$PostNr_start[i], postnr_grupper$PostNr_slut[i])
#   }, .progress = TRUE) %>%
#     rbindlist(use.names = TRUE, fill = TRUE) %>%
#     as_tibble()
#
#   slut_tid <- Sys.time()
#   kor_tid <- difftime(slut_tid, start_tid, units = "secs")
#   cat(glue("Funktionen tog totalt {round(as.numeric(kor_tid), 1)} sekunder ({round(as.numeric(kor_tid)/60, 2)} minuter)."))
#
#   return(je)
# }
#

