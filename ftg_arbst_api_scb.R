# ===== ftg_arbst_api_scb.R =====
#
# Hämtar företag och arbetsställen från SCB:s sökpåver-API med automatisk
# uppdelning när uttag överstiger API:ts gräns på 2000 rader per call.
#
# Strategibaserad uppdelning
# --------------------------
# En "cell" är en payload (statusfält + kategorier + variabler). Om cellen
# har > 2000 rader applicerar funktionen strategier i prioritetsordning:
#
#   1. OrgNr-prefix                            (variabel, BorjarPa, 10-delar)
#   2. Postnr-intervall                        (variabel, Mellan, halvering)
#   3. Omsättningsklass / Anställda            (kategori, binärhalvering)
#   4. 2-siffrig bransch 1                     (kategori, binärhalvering)
#   5. Juridisk form                           (företag, binärhalvering)
#
# OrgNr-prefix ligger först eftersom varje företag/arbetsställe har ett
# 10-siffrigt orgnr — strategin täcker 100% av posterna utan undantag.
# Kategoristrategier hoppar tyst över poster som saknar värde i kategorin
# (t.ex. nystartade utan omsättningsklass) och kan tappa rader vid stora uttag.
#
# Varje strategi delar cellen i mindre celler längs sin dimension. Rekursionen
# fortsätter tills varje delcell ryms inom 2000 rader (eller alla strategier
# är slut — då hoppas cellen över med varning). Strategier som har "förbrukats"
# (kategorin är nere på ett enda värde, postnr-intervallet är 1 nummer brett,
# orgnr-prefixet är 10 siffror långt) returnerar tom partition och nästa
# strategi tas in.
#
# Användning av orgnr-listor
# --------------------------
# Skicka en vektor med 10-siffriga organisationsnummer via parametern `orgnr`
# så görs en ArLikaMed-call per orgnr. För arbetsställen kombineras detta med
# postnr-halvering om något orgnr har >2000 arbetsställen (extremt sällsynt).
#
# Förutsättningar
# ---------------
# - SCB-certifikat installerat i Windows certifikatförråd (CN innehåller
#   "sokpavar"/"sökpåver"), CurrentUser eller LocalMachine.
# - Använder INTE betalda tilläggsgrupper.
#
# Peter Möller, Region Dalarna — november 2026 (strategi-refaktor)

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(glue)
  library(tidyverse)
})

# ───────────────────────────────────────────────────────────────────────────
# Certifikat
# ───────────────────────────────────────────────────────────────────────────

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
      filter(grepl("sokpavar|sökpåver|sokpåver", subject_lc) & grepl("\\bscb\\b", issuer_lc)) |>
      transmute(store = store, thumb = gsub(" ", "", thumb))
    if (nrow(df)) df[1, ] else NULL
  }
  hit <- parse_store("certutil -user -store My", "CurrentUser")
  if (is.null(hit)) hit <- parse_store("certutil -store My", "LocalMachine")
  if (is.null(hit)) stop("Hittade inget SCB-sökpåver-cert i CurrentUser eller LocalMachine.")
  list(store = hit$store, thumb = hit$thumb)
}

cert_info <- scb_hamta_cert()
scb_hamta_cert_thumb <- function() cert_info$thumb

# ───────────────────────────────────────────────────────────────────────────
# HTTP-anrop med rate-limit (max 10 anrop per 10 sek)
# ───────────────────────────────────────────────────────────────────────────

api_posta <- function(url, body, cert_thumb, .api_calls_env,
                      visa_medd = FALSE, max_forsok = 3L) {
  for (forsok in seq_len(max_forsok)) {
    # Rate-limit (max 10 anrop per 10 sek)
    now <- as.numeric(Sys.time())
    .api_calls_env$timestamps <- .api_calls_env$timestamps[.api_calls_env$timestamps > now - 10]
    if (length(.api_calls_env$timestamps) >= 10) {
      wait_time <- 10 - (now - min(.api_calls_env$timestamps))
      if (visa_medd) message(sprintf("⏳ Väntar %.1f sek pga anropsgräns...", wait_time))
      Sys.sleep(wait_time)
    }
    .api_calls_env$timestamps <- c(.api_calls_env$timestamps, as.numeric(Sys.time()))

    r <- tryCatch(
      httr::POST(
        url,
        add_headers(`Content-Type` = "application/json", Accept = "application/json"),
        body   = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "json",
        httr::config(sslcert = paste0(cert_info$store, "\\MY\\", cert_thumb), sslcerttype = "Schannel")
      ),
      error = function(e) list(status_code = -1L, error = conditionMessage(e))
    )

    # Lyckat: returnera direkt
    if (!is.null(r$status_code) && r$status_code == 200L) return(r)
    # Klient-fel (400-499): retry hjälper inte
    if (!is.null(r$status_code) && r$status_code >= 400L && r$status_code < 500L) return(r)

    # Transient fel (timeout, 5xx, nätverk): backoff och försök igen
    if (forsok < max_forsok) {
      backoff <- 2 ^ (forsok - 1L) * 2  # 2, 4, 8 sek
      status_txt <- if (is.null(r$status_code)) "okänt" else as.character(r$status_code)
      err_txt <- if (!is.null(r$error)) sprintf(" — %s", r$error) else ""
      message(sprintf("⚠ API-fel (status %s)%s försök %d/%d — väntar %d sek och försöker igen",
                      status_txt, err_txt, forsok, max_forsok, backoff))
      Sys.sleep(backoff)
    }
  }
  r  # sista (misslyckade) svaret
}

rakna_api <- function(rakna_url, payload, cert_thumb, .api_calls_env,
                      visa_medd = FALSE, max_forsok = 3L) {
  r <- api_posta(rakna_url, payload, cert_thumb, .api_calls_env, visa_medd, max_forsok)
  if (is.null(r$status_code) || r$status_code != 200) return(NA_integer_)
  suppressWarnings(as.integer(httr::content(r)))
}

hamta_api <- function(hamta_url, payload, cert_thumb, .api_calls_env,
                      visa_medd = FALSE, max_forsok = 3L) {
  r <- api_posta(hamta_url, payload, cert_thumb, .api_calls_env, visa_medd, max_forsok)
  if (is.null(r$status_code) || r$status_code != 200) return(NULL)
  innehall <- httr::content(r, simplifyVector = TRUE)
  if (is.null(innehall)) return(NULL)
  if (is.data.frame(innehall)) return(as_tibble(innehall))
  if (is.list(innehall) && length(innehall) > 0 && is.data.frame(innehall[[1]])) return(as_tibble(innehall[[1]]))
  NULL
}

# ───────────────────────────────────────────────────────────────────────────
# Kategori-tabeller från API:t (med cache)
# ───────────────────────────────────────────────────────────────────────────

scb_kategorier_med_kodtabeller <- function(
    tabell = "foretag",
    med_varden = TRUE,
    cert_thumb = scb_hamta_cert_thumb()) {

  del <- if_else(tolower(tabell) %in% c("ftg", "foretag", "företag"), "Je", "Ae")
  url <- paste0("https://privateapi.scb.se/nv0101/v1/sokpavar/api/", del, "/KategorierMedKodtabeller")

  r <- httr::GET(url, config = httr::config(sslcert = paste0(cert_info$store, "\\MY\\", cert_thumb), sslcerttype = "Schannel"))
  httr::stop_for_status(r)
  kats <- httr::content(r, as = "parsed", encoding = "UTF-8")

  retur_df <- tibble(
    kategori_id  = map_chr(kats, ~ .x$Id_Kategori_JE %||% .x$Id_Kategori_AE %||% .x$Id_Kategori %||% .x$Id),
    kategori_typ = map_chr(kats, ~ .x$Datatyp %||% NA_character_),
    varde_lista  = map(kats, ~ .x$VardeLista %||% list())
  ) |>
    mutate(varde_lista = map(varde_lista, ~ keep(.x, ~ is.list(.x) && (!is.null(.x$Varde) || !is.null(.x$Kod))))) |>
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
  retur_df
}

.kategori_cache <- new.env(parent = emptyenv())

hamta_kategorivarden <- function(tabell, kategori_id, cert_thumb = scb_hamta_cert_thumb()) {
  nyckel <- paste(tabell, kategori_id, sep = "::")
  if (!is.null(.kategori_cache[[nyckel]])) return(.kategori_cache[[nyckel]])
  df <- scb_kategorier_med_kodtabeller(tabell = tabell, cert_thumb = cert_thumb)
  varden <- df %>% filter(kategori_id == !!kategori_id, !is.na(kod)) %>% dplyr::pull(kod) %>% unique()
  .kategori_cache[[nyckel]] <- varden
  varden
}

# ───────────────────────────────────────────────────────────────────────────
# Lazy-hämtning av Sveriges alla kommunkoder via Region Dalarnas hjälpfunktion
# (Källa: https://github.com/Region-Dalarna/funktioner). Cache:as i miljön så
# att GitHub-källningen bara görs en gång per R-session.
# ───────────────────────────────────────────────────────────────────────────

.kommun_cache <- new.env(parent = emptyenv())

hamta_alla_kommunkoder <- function() {
  if (!is.null(.kommun_cache$koder)) return(.kommun_cache$koder)

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R",
         encoding = "utf-8", echo = FALSE)
  kommuner <- hamtakommuner(tamedlan = FALSE, tamedriket = FALSE, allakommuner = TRUE)

  # Plocka ut den kolumn som innehåller 4-siffriga kommunkoder. Stödjer både
  # data.frame med kolumn (kommun/kommunkod/kod/region) och ren teckenvektor.
  if (is.data.frame(kommuner)) {
    kod_kolumn <- NULL
    for (kn in c("kommun", "kommunkod", "kod", "region")) {
      if (kn %in% names(kommuner)) {
        v <- as.character(kommuner[[kn]])
        if (all(grepl("^\\d{4}$", v))) { kod_kolumn <- kn; break }
      }
    }
    if (is.null(kod_kolumn))
      stop("Hittade ingen 4-siffrig kommunkod-kolumn i hamtakommuner()-resultatet")
    koder <- as.character(kommuner[[kod_kolumn]])
  } else {
    koder <- as.character(kommuner)
  }
  koder <- unique(koder[grepl("^\\d{4}$", koder)])

  .kommun_cache$koder <- koder
  koder
}

# Expanderar en blandad vektor av län- och kommunkoder till en vektor 4-siffriga
# kommunkoder. Regler:
#   "00"        → alla kommuner i Sverige (hela landet)
#   "20"        → alla kommuner vars 4-siffriga kod börjar med 20 (Dalarna)
#   "2080"      → behålls som den är (Falun)
#   c("21","2080") → alla Gävleborgskommuner + Falun, deduplicerat
# NULL → NULL (oförändrat; anroparen får hantera "ingen kommunavgränsning")
expandera_kommunkoder <- function(koder) {
  if (is.null(koder)) return(NULL)
  koder <- as.character(koder)
  koder <- koder[!is.na(koder) & nzchar(koder)]
  if (!length(koder)) return(NULL)

  ogiltiga <- koder[!grepl("^\\d{2}$|^\\d{4}$", koder)]
  if (length(ogiltiga)) {
    stop(sprintf("Ogiltiga koder (måste vara 2- eller 4-siffriga): %s",
                 paste(unique(ogiltiga), collapse = ", ")))
  }

  # "00" = hela Sverige — kortsluter
  if ("00" %in% koder) return(hamta_alla_kommunkoder())

  lan_koder <- unique(koder[nchar(koder) == 2L])
  kom_koder <- unique(koder[nchar(koder) == 4L])

  if (length(lan_koder)) {
    alla <- hamta_alla_kommunkoder()
    expanderade <- unlist(lapply(lan_koder, function(l) alla[startsWith(alla, l)]))
    kom_koder <- c(kom_koder, expanderade)
  }
  sort(unique(kom_koder))
}

# ───────────────────────────────────────────────────────────────────────────
# Payload-hjälpare (läs/sätt kategorier och variabler i en payload-list)
# ───────────────────────────────────────────────────────────────────────────

skapa_baspayload <- function(typ,
                             grundfilter_kategorier = list(),
                             grundfilter_variabler  = list(),
                             foretagsstatus = NULL,
                             registreringsstatus = NULL,
                             arbetsstallestatus = NULL) {
  payload <- list()
  if (typ == "foretag") {
    if (!is.null(foretagsstatus))      payload$Företagsstatus      <- unname(foretagsstatus)
    if (!is.null(registreringsstatus)) payload$Registreringsstatus <- unname(registreringsstatus)
  } else {
    if (!is.null(arbetsstallestatus))  payload$Arbetsställestatus  <- unname(arbetsstallestatus)
  }
  if (length(grundfilter_kategorier)) payload$Kategorier <- grundfilter_kategorier
  if (length(grundfilter_variabler))  payload$variabler  <- grundfilter_variabler
  payload
}

las_kategori_koder <- function(payload, kategori_namn, branschniva = NULL) {
  if (is.null(payload$Kategorier)) return(NULL)
  for (k in payload$Kategorier) {
    if (identical(k$Kategori, kategori_namn) &&
        identical(k$Branschniva %||% NULL, branschniva %||% NULL)) {
      return(unlist(k$Kod))
    }
  }
  NULL
}

satt_kategori <- function(payload, kategori_namn, koder, branschniva = NULL) {
  ny <- list(Kategori = kategori_namn, Kod = as.list(koder))
  if (!is.null(branschniva)) ny$Branschniva <- branschniva

  if (is.null(payload$Kategorier)) {
    payload$Kategorier <- list(ny)
    return(payload)
  }
  for (i in seq_along(payload$Kategorier)) {
    if (identical(payload$Kategorier[[i]]$Kategori, kategori_namn) &&
        identical(payload$Kategorier[[i]]$Branschniva %||% NULL, branschniva %||% NULL)) {
      payload$Kategorier[[i]] <- ny
      return(payload)
    }
  }
  payload$Kategorier <- c(payload$Kategorier, list(ny))
  payload
}

las_variabel <- function(payload, variabel_namn) {
  if (is.null(payload$variabler)) return(NULL)
  for (v in payload$variabler) {
    if (identical(v$Variabel, variabel_namn)) return(v)
  }
  NULL
}

satt_variabel <- function(payload, variabel_namn, varde1, varde2 = "", operator = "ArLikaMed") {
  ny <- list(Varde1 = varde1, Varde2 = varde2, Operator = operator, Variabel = variabel_namn)
  if (is.null(payload$variabler)) {
    payload$variabler <- list(ny)
    return(payload)
  }
  for (i in seq_along(payload$variabler)) {
    if (identical(payload$variabler[[i]]$Variabel, variabel_namn)) {
      payload$variabler[[i]] <- ny
      return(payload)
    }
  }
  payload$variabler <- c(payload$variabler, list(ny))
  payload
}

# ───────────────────────────────────────────────────────────────────────────
# Strategier för uppdelning
# ───────────────────────────────────────────────────────────────────────────

# Kategoristrategi: binärhalvering över kodlistan. Om payload redan har
# kategorin satt halveras den befintliga listan; annars hämtas full lista
# från API:t (cache:as) och halveras.
strategi_kategori <- function(kategori_namn, varden_fn, branschniva = NULL) {
  list(
    namn = if (!is.null(branschniva)) sprintf("%s (Branschniva %s)", kategori_namn, branschniva) else kategori_namn,
    partition = function(payload) {
      nuvarande <- las_kategori_koder(payload, kategori_namn, branschniva)
      varden <- if (is.null(nuvarande)) varden_fn() else nuvarande
      if (length(varden) <= 1) return(list())
      mitt <- ceiling(length(varden) / 2)
      list(
        satt_kategori(payload, kategori_namn, varden[1:mitt], branschniva),
        satt_kategori(payload, kategori_namn, varden[(mitt + 1):length(varden)], branschniva)
      )
    }
  )
}

# Postnr (Mellan): halverar intervallet rekursivt
strategi_postnr <- list(
  namn = "Postnr (intervall)",
  partition = function(payload) {
    v <- las_variabel(payload, "Postnr")
    if (is.null(v) || !identical(v$Operator, "Mellan")) {
      start <- 0L; slut <- 99999L
    } else {
      start <- suppressWarnings(as.integer(v$Varde1))
      slut  <- suppressWarnings(as.integer(v$Varde2))
      if (is.na(start) || is.na(slut) || slut <= start) return(list())
    }
    mitt <- (start + slut) %/% 2L
    list(
      satt_variabel(payload, "Postnr", sprintf("%05d", start),      sprintf("%05d", mitt),  "Mellan"),
      satt_variabel(payload, "Postnr", sprintf("%05d", mitt + 1L),  sprintf("%05d", slut),  "Mellan")
    )
  }
)

# OrgNr (10 siffror) BorjarPa: förlänger prefixet en siffra i taget (10 barn)
strategi_orgnr <- list(
  namn = "OrgNr-prefix",
  partition = function(payload) {
    v <- las_variabel(payload, "OrgNr (10 siffror)")
    if (!is.null(v) && !identical(v$Operator, "BorjarPa")) return(list())
    prefix <- if (is.null(v)) "" else v$Varde1
    if (nchar(prefix) >= 10) return(list())
    map(as.character(0:9), function(d) {
      satt_variabel(payload, "OrgNr (10 siffror)", paste0(prefix, d), "", "BorjarPa")
    })
  }
)

# ───────────────────────────────────────────────────────────────────────────
# Rekursiv hämtning med strategilista
# ───────────────────────────────────────────────────────────────────────────

hamta_rekursivt <- function(payload, strategier, ctx, djup = 0L) {
  n <- ctx$rakna(payload)

  if (is.na(n)) {
    if (ctx$visa_medd) message(sprintf("%s⚠ NA från räkna — hoppar över denna cell", strrep("  ", djup)))
    return(NULL)
  }
  if (n == 0L) return(NULL)
  if (n <= ctx$max_rader) {
    if (ctx$visa_medd) message(sprintf("%s✅ %d rader → hämtar", strrep("  ", djup), n))
    res <- ctx$hamta(payload)
    if (!is.null(res) && !is.null(ctx$fetch_pb)) {
      suppressWarnings(ctx$fetch_pb$tick(nrow(res)))
    }
    return(res)
  }

  if (!length(strategier)) {
    # Sista chans: OrgNr-prefix om det ännu inte är uttömt. Garanterar att
    # även om kategoristrategierna ger upp tidigt så finns alltid en
    # backstop kvar (orgnr är unika → 10-siffrigt prefix = max 1 träff).
    v <- las_variabel(payload, "OrgNr (10 siffror)")
    pa_orgnr <- !is.null(v) && identical(v$Operator, "BorjarPa")
    if (!pa_orgnr || nchar(v$Varde1 %||% "") < 10) {
      if (ctx$visa_medd)
        message(sprintf("%s🆘 Sista chans: OrgNr-prefix (cell %d rader)", strrep("  ", djup), n))
      return(hamta_rekursivt(payload, list(strategi_orgnr), ctx, djup))
    }
    message(sprintf("⚠ Cell med %d rader kunde inte delas mer (alla strategier slut) — hoppar över", n))
    if (!is.null(ctx$skip_env)) {
      ctx$skip_env$rader  <- ctx$skip_env$rader + n
      ctx$skip_env$celler <- ctx$skip_env$celler + 1L
    }
    return(NULL)
  }

  strategi <- strategier[[1]]
  if (ctx$visa_medd)
    message(sprintf("%s🔀 %d rader > %d, delar via '%s'",
                    strrep("  ", djup), n, ctx$max_rader, strategi$namn))

  barn <- strategi$partition(payload)
  if (!length(barn)) {
    return(hamta_rekursivt(payload, strategier[-1], ctx, djup))
  }

  # Varje barn-payload använder samma fulla strategi-lista — strategin kan
  # behöva halveras flera gånger innan vi går till nästa dimension.
  resultat_list <- map(barn, function(bp) hamta_rekursivt(bp, strategier, ctx, djup + 1L))
  compact(resultat_list) %>% list_rbind()
}

# ───────────────────────────────────────────────────────────────────────────
# OrgNr-batchning via prefix (BorjarPa)
# ───────────────────────────────────────────────────────────────────────────
#
# När man vill hämta en lång lista 10-siffriga orgnr går det ofta mycket
# snabbare att gruppera dem på prefix och göra en BorjarPa-call per grupp
# (upp till 2000 rader per call), och sedan filtrera ner resultatet till de
# orgnr som efterfrågats. För typiska företagslistor (10 000–100 000 orgnr)
# blir det ofta 20–80x färre API-anrop än en call per orgnr.

# Hittar OrgNr-kolumnen i ett SCB-svar (med fallback för udda kolumnnamn)
hitta_orgnr_kolumn <- function(df) {
  kandidater <- c("OrgNr", "Orgnr", "orgnr", "OrgNr (10 siffror)",
                  "OrganisationsNummer", "Organisationsnummer")
  hit <- intersect(kandidater, names(df))
  if (length(hit)) return(hit[1])
  sannolika <- map_lgl(df, function(v) {
    vc <- as.character(v)
    length(vc) > 0 && mean(grepl("^\\d{10}$", head(vc, 200)), na.rm = TRUE) > 0.8
  })
  hits <- names(df)[which(sannolika)]
  if (length(hits)) hits[1] else NA_character_
}

# Rekursiv prefix-batchning:
#   - Om gruppen har 1 orgnr eller prefix-längden nått 10 → ArLikaMed direkt.
#   - Annars räkna BorjarPa för aktuellt prefix. Om ≤ max_rader → hämta
#     BorjarPa och filtrera resultatet till gruppens orgnr. Om > max_rader →
#     dela in i (max 10) undergrupper med prefix-längd+1 och rekursera.
#   - Returnerar list(df, klara_orgnr) där klara_orgnr är vektorn över de
#     orgnr som har hanterats (oavsett om de gav träff eller inte).
hamta_orgnr_batch <- function(orgnr_grupp, prefix_len, payload_bas, ctx_local) {
  if (!length(orgnr_grupp)) return(list(df = NULL, klara_orgnr = character()))

  # Bas-fall: enstaka orgnr eller maximalt prefix → ArLikaMed per orgnr
  if (length(orgnr_grupp) == 1L || prefix_len >= 10L) {
    res_lista <- list()
    for (o in orgnr_grupp) {
      p <- satt_variabel(payload_bas, "OrgNr (10 siffror)", o, "", "ArLikaMed")
      r <- ctx_local$hamta(p)
      if (!is.null(r)) res_lista[[length(res_lista) + 1L]] <- r
      if (!is.null(ctx_local$pb)) suppressWarnings(ctx_local$pb$tick(tokens = list(message = o)))
    }
    return(list(df = bind_rows(res_lista), klara_orgnr = orgnr_grupp))
  }

  # Gruppera på aktuellt prefix
  prefixar  <- substr(orgnr_grupp, 1, prefix_len)
  grupper   <- split(orgnr_grupp, prefixar)

  res_total   <- list()
  klara_total <- character()

  for (pf in names(grupper)) {
    grupp <- grupper[[pf]]

    # Singel-grupp → direkt ArLikaMed (slipper extra räknings-call)
    if (length(grupp) == 1L) {
      p <- satt_variabel(payload_bas, "OrgNr (10 siffror)", grupp, "", "ArLikaMed")
      r <- ctx_local$hamta(p)
      if (!is.null(r)) res_total[[length(res_total) + 1L]] <- r
      klara_total <- c(klara_total, grupp)
      if (!is.null(ctx_local$pb)) suppressWarnings(ctx_local$pb$tick(tokens = list(message = grupp)))
      next
    }

    p_test <- satt_variabel(payload_bas, "OrgNr (10 siffror)", pf, "", "BorjarPa")
    n <- ctx_local$rakna(p_test)

    if (is.na(n) || n == 0L) {
      # Inga företag matchar (avregistrerade, fel status etc) — markera klara
      klara_total <- c(klara_total, grupp)
      if (!is.null(ctx_local$pb))
        suppressWarnings(ctx_local$pb$tick(length(grupp), tokens = list(message = sprintf("%s (tom)", pf))))
      next
    }

    if (n <= ctx_local$max_rader) {
      r <- ctx_local$hamta(p_test)
      if (!is.null(r) && NROW(r) > 0) {
        orgnr_kol <- hitta_orgnr_kolumn(r)
        if (!is.na(orgnr_kol)) {
          r <- r[as.character(r[[orgnr_kol]]) %in% grupp, , drop = FALSE]
        }
        if (NROW(r) > 0) res_total[[length(res_total) + 1L]] <- r
      }
      klara_total <- c(klara_total, grupp)
      if (!is.null(ctx_local$pb))
        suppressWarnings(ctx_local$pb$tick(length(grupp),
                                           tokens = list(message = sprintf("%s (%d→%d)", pf, n, length(grupp)))))
    } else {
      if (ctx_local$visa_medd)
        message(sprintf("  🔀 prefix '%s' = %d företag — splittar (prefix-längd %d)",
                        pf, n, prefix_len + 1L))
      sub <- hamta_orgnr_batch(grupp, prefix_len + 1L, payload_bas, ctx_local)
      if (!is.null(sub$df) && NROW(sub$df) > 0) res_total[[length(res_total) + 1L]] <- sub$df
      klara_total <- c(klara_total, sub$klara_orgnr)
    }
  }

  list(df = bind_rows(res_total), klara_orgnr = klara_total)
}

# ───────────────────────────────────────────────────────────────────────────
# scb_hamta_foretag()
# ───────────────────────────────────────────────────────────────────────────

scb_hamta_foretag <- function(
    kommunkoder = NULL,                  # vektor av kommun- och/eller länskoder, t.ex.
    #   c("2080","2081")          → Falun + Borlänge
    #   "20"                       → alla kommuner i Dalarna
    #   c("21","2080")             → hela Gävleborg + Falun
    #   "00"                       → hela Sverige
    # NULL = ingen kommunavgränsning (räkna totalen och ev. auto-loopa)
    orgnr = NULL,                         # vektor 10-siffriga orgnr; om angivet körs orgnr-läge
    grundfilter_variabler  = list(),      # extra "variabler"-filter (skickas rakt in i payload)
    grundfilter_kategorier = list(),      # extra "Kategorier"-filter
    juridisk_kod = NULL,                  # extra: c("10","31"); om satt deltar dimensionen ändå i halvering
    oms_klass    = NULL,                  # extra: c("00","02")
    branscher    = NULL,                  # extra 2-siffriga: c("10","11")
    vald_foretagsstatus      = "1",
    vald_registreringsstatus = "1",
    auto_loopa_kommuner      = FALSE,     # om "hela riket" (kommunkoder = NULL eller "00"): TRUE = loopa Sveriges kommuner; FALSE (default) = single-shot
    auto_loopa_trosklel      = 10000L,    # gäller när auto_loopa_kommuner = TRUE: > detta antal → loopa kommuner
    cache_path               = NULL,      # filsökväg → checkpoint per kommun; NULL = ingen cache
    behall_cache             = FALSE,     # TRUE = behåll cachefilen efter framgångsrik körning
    max_forsok               = 3L,        # försök per API-anrop vid nätverksfel
    batch_via_prefix         = TRUE,      # orgnr-läge: gruppera orgnr på prefix och hämta via BorjarPa
    prefix_start             = 4L,        # orgnr-läge: startprefix-längd (4 = ~10 grupper per "block"; 5 = finare)
    cert_thumb = scb_hamta_cert_thumb(),
    visa_meddelanden_konsol           = FALSE,
    visa_resultat_meddelanden_konsol  = TRUE,
    visa_progress                     = TRUE
) {
  total_start <- Sys.time()
  base_url  <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/"
  rakna_url <- paste0(base_url, "je/raknaforetag/")
  hamta_url <- paste0(base_url, "je/hamtaforetag/")
  max_rader <- 2000L

  # Normalisera: NULL och "00" betyder båda "hela riket" — låt rakna+auto_loopa-
  # logiken nedan avgöra om det blir single-shot eller kommun-loop.
  if (is.null(kommunkoder) || "00" %in% as.character(kommunkoder)) {
    kommunkoder <- NULL
  } else {
    kommunkoder <- expandera_kommunkoder(kommunkoder)
  }

  .api_calls <- new.env(); .api_calls$timestamps <- numeric(0)

  rakna <- function(p) rakna_api(rakna_url, p, cert_thumb, .api_calls, visa_meddelanden_konsol, max_forsok)
  hamta <- function(p) hamta_api(hamta_url, p, cert_thumb, .api_calls, visa_meddelanden_konsol, max_forsok)

  ctx <- list(rakna = rakna, hamta = hamta, max_rader = max_rader,
              visa_medd = visa_meddelanden_konsol)

  # Bygg statisk del av baspayload (gemensamt för alla anrop i denna körning)
  bygg_baspayload <- function(extra_kategorier = list()) {
    kat <- grundfilter_kategorier
    if (length(extra_kategorier)) kat <- c(kat, extra_kategorier)
    if (!is.null(juridisk_kod)) kat <- c(kat, list(list(Kategori = "Juridisk form",         Kod = as.list(juridisk_kod))))
    if (!is.null(oms_klass))    kat <- c(kat, list(list(Kategori = "Omsättningsklass grov", Kod = as.list(oms_klass))))
    if (!is.null(branscher))    kat <- c(kat, list(list(Kategori = "2-siffrig bransch 1",   Kod = as.list(branscher))))
    skapa_baspayload(
      typ = "foretag",
      grundfilter_kategorier = kat,
      grundfilter_variabler  = grundfilter_variabler,
      foretagsstatus         = vald_foretagsstatus,
      registreringsstatus    = vald_registreringsstatus
    )
  }

  # ── Orgnr-läge ────────────────────────────────────────────────────────────
  if (!is.null(orgnr) && length(orgnr)) {
    orgnr <- unique(as.character(orgnr))
    orgnr <- orgnr[!is.na(orgnr) & nzchar(orgnr)]

    # Ladda cache om finns
    resultat_lista <- list()
    done_orgnr <- character()
    if (!is.null(cache_path) && file.exists(cache_path)) {
      cache <- tryCatch(readRDS(cache_path), error = function(e) NULL)
      if (!is.null(cache) && is.list(cache)) {
        resultat_lista <- cache$resultat   %||% list()
        done_orgnr     <- cache$done_orgnr %||% character()
        if (visa_resultat_meddelanden_konsol)
          message(glue("♻ Återupptar från cache: {length(done_orgnr)} orgnr redan klara"))
      }
    }
    todo_orgnr <- setdiff(orgnr, done_orgnr)

    pb <- if (visa_progress) progress::progress_bar$new(
      total = length(orgnr),
      format = ":current/:total [:bar] :percent :message eta :eta", clear = FALSE
    ) else list(tick = function(...) NULL)
    if (length(done_orgnr)) suppressWarnings(pb$tick(length(done_orgnr)))

    if (batch_via_prefix && length(todo_orgnr) >= 3L) {
      # ── Adaptiv prefix-batchning ──────────────────────────────────────────
      if (visa_resultat_meddelanden_konsol)
        message(glue("🔎 Hämtar {scb_format(length(todo_orgnr))} orgnr via prefix-batchning (startprefix={prefix_start})..."))

      todo_orgnr <- sort(todo_orgnr)
      payload_bas <- bygg_baspayload()
      ctx_orgnr <- list(
        rakna = rakna, hamta = hamta, max_rader = max_rader,
        pb = if (visa_progress) pb else NULL,
        visa_medd = visa_meddelanden_konsol
      )

      # Top-level: gruppera på prefix_start och hantera en grupp i taget så
      # att vi kan checkpoint:a cachen efter varje stor batch.
      start_prefixar <- substr(todo_orgnr, 1, prefix_start)
      top_grupper <- split(todo_orgnr, start_prefixar)

      for (pf_idx in seq_along(top_grupper)) {
        grupp <- top_grupper[[pf_idx]]
        sub <- hamta_orgnr_batch(grupp, prefix_start, payload_bas, ctx_orgnr)
        if (!is.null(sub$df) && NROW(sub$df) > 0)
          resultat_lista[[length(resultat_lista) + 1L]] <- sub$df
        done_orgnr <- c(done_orgnr, sub$klara_orgnr)

        if (!is.null(cache_path)) {
          tryCatch(
            saveRDS(list(resultat = resultat_lista, done_orgnr = done_orgnr), cache_path),
            error = function(e) message(glue("⚠ Kunde inte spara cache: {conditionMessage(e)}"))
          )
        }
      }
    } else {
      # ── Fallback: en API-call per orgnr ──────────────────────────────────
      if (visa_resultat_meddelanden_konsol)
        message(glue("🔎 Hämtar {scb_format(length(todo_orgnr))} orgnr (en API-call per orgnr)..."))

      spara_var <- 100L
      for (idx in seq_along(todo_orgnr)) {
        o <- todo_orgnr[idx]
        pb$tick(tokens = list(message = o))
        p <- bygg_baspayload()
        p <- satt_variabel(p, "OrgNr (10 siffror)", o, "", "ArLikaMed")
        r <- hamta(p)
        if (!is.null(r)) resultat_lista[[length(resultat_lista) + 1]] <- r
        done_orgnr <- c(done_orgnr, o)

        if (!is.null(cache_path) && (idx %% spara_var == 0L || idx == length(todo_orgnr))) {
          tryCatch(
            saveRDS(list(resultat = resultat_lista, done_orgnr = done_orgnr), cache_path),
            error = function(e) message(glue("⚠ Kunde inte spara cache: {conditionMessage(e)}"))
          )
        }
      }
    }

    df <- resultat_lista %>% compact() %>% list_rbind()

    if (!is.null(cache_path) && !behall_cache && file.exists(cache_path)) {
      tryCatch(file.remove(cache_path), error = function(e) NULL)
    }

    if (visa_resultat_meddelanden_konsol)
      message(glue("📊 Hämtade {scb_format(NROW(df))} företag av {length(orgnr)} efterfrågade på {scb_tid(total_start)}"))
    return(df)
  }

  # ── Räkna totalen (om ingen kommunavgränsning) ──────────────────────────
  # Vi räknar alltid hela uttaget när användaren inte angivit kommuner.
  # Är det fler rader än tröskeln OCH auto_loopa_kommuner är på byter vi till
  # att loopa alla kommunkoder. Annars hämtas allt i single-shot via
  # strategierna och totalen används för progress-bar + sanity check.
  n_tot_auto <- NA_integer_
  if (is.null(kommunkoder)) {
    n_tot_auto <- rakna(bygg_baspayload())
    if (!is.na(n_tot_auto) && n_tot_auto > auto_loopa_trosklel && auto_loopa_kommuner) {
      if (visa_resultat_meddelanden_konsol)
        message(glue("🌍 Hela uttaget = {scb_format(n_tot_auto)} företag — loopar Sveriges kommuner automatiskt"))
      kommunkoder <- hamta_alla_kommunkoder()
    } else if (visa_resultat_meddelanden_konsol && !is.na(n_tot_auto)) {
      message(glue("ℹ Hela uttaget = {scb_format(n_tot_auto)} företag — hämtar utan kommun-loop"))
    }
  }

  # Räknare för bortskippade rader (när alla strategier slut)
  skip_env <- new.env(); skip_env$rader <- 0L; skip_env$celler <- 0L
  ctx$skip_env <- skip_env

  # ── Strategilista för uppdelning ─────────────────────────────────────────
  # OrgNr-prefix ligger först eftersom kategori-strategierna tappar poster som
  # saknar värde i kategorin (gav ~7,6% gap för hela riket). OrgNr finns på
  # alla företag → 100% täckning. Övriga strategier ligger kvar som backup.
  strategier <- list(
    strategi_orgnr,
    strategi_postnr,
    strategi_kategori("Omsättningsklass grov",
                      varden_fn = function() hamta_kategorivarden("foretag", "Omsättningsklass grov", cert_thumb)),
    strategi_kategori("2-siffrig bransch 1",
                      varden_fn = function() hamta_kategorivarden("foretag", "2-siffrig bransch 1", cert_thumb)),
    strategi_kategori("Juridisk form",
                      varden_fn = function() hamta_kategorivarden("foretag", "Juridisk form", cert_thumb))
  )

  # ── Kommunloop ────────────────────────────────────────────────────────────
  kommun_lista <- if (is.null(kommunkoder)) list(NULL) else as.list(kommunkoder)

  pb <- if (visa_progress && length(kommun_lista) > 1) progress::progress_bar$new(
    total = length(kommun_lista),
    format = ":current/:total [:bar] :percent :message", clear = FALSE
  ) else list(tick = function(...) NULL)

  # Single-shot-läge: progress-bar som tickar per hämtad rad mot totalen
  ctx$fetch_pb <- if (visa_progress && length(kommun_lista) == 1 &&
                      !is.na(n_tot_auto) && n_tot_auto > 0) {
    progress::progress_bar$new(
      total = n_tot_auto,
      format = "  📥 hämtade :current/:total [:bar] :percent eta :eta",
      clear = FALSE, show_after = 0
    )
  } else NULL

  # Ladda cache om finns (per-kommun checkpoint)
  resultat <- list()
  done_kommuner <- character()
  if (!is.null(cache_path) && file.exists(cache_path)) {
    cache <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(cache) && is.list(cache)) {
      resultat      <- cache$resultat      %||% list()
      done_kommuner <- cache$done_kommuner %||% character()
      if (visa_resultat_meddelanden_konsol)
        message(glue("♻ Återupptar från cache: {length(done_kommuner)} kommuner redan klara ({scb_format(sum(map_int(resultat, NROW)))} rader)"))
    }
  }

  for (kom in kommun_lista) {
    # Hoppa över redan klara kommuner från cache
    if (!is.null(kom) && kom %in% done_kommuner) {
      pb$tick(tokens = list(message = paste(kom, "(cache)")))
      next
    }

    kom_start <- Sys.time()
    pb$tick(tokens = list(message = kom %||% "(alla)"))

    extra_kat <- if (!is.null(kom)) list(list(Kategori = "SätesKommun", Kod = list(kom))) else list()
    payload <- bygg_baspayload(extra_kategorier = extra_kat)

    df <- hamta_rekursivt(payload, strategier, ctx, djup = 0L)
    if (!is.null(df) && nrow(df)) {
      if (!is.null(kom)) df$kod_kommun <- kom
      resultat[[length(resultat) + 1]] <- df
    }

    # Spara checkpoint efter varje klar kommun
    if (!is.null(kom)) done_kommuner <- c(done_kommuner, kom)
    if (!is.null(cache_path)) {
      tryCatch(
        saveRDS(list(resultat = resultat, done_kommuner = done_kommuner), cache_path),
        error = function(e) message(glue("⚠ Kunde inte spara cache: {conditionMessage(e)}"))
      )
    }

    if (visa_resultat_meddelanden_konsol) {
      n_kom <- if (!is.null(df)) nrow(df) else 0L
      message(glue("🏁 {kom %||% 'alla kommuner'}: hämtade {scb_format(n_kom)} företag på {scb_sec(kom_start)} sek"))
    }
  }

  retur_df <- resultat %>% list_rbind()

  # Bortskippade celler (alla strategier slut)
  if (skip_env$celler > 0L) {
    message(glue("⚠ Hoppade över {skip_env$celler} celler ({scb_format(skip_env$rader)} räknade rader) — strategierna räckte inte hela vägen"))
  }

  # Sanity check: jämför resultat med auto-totalen.
  if (!is.na(n_tot_auto) && n_tot_auto > nrow(retur_df)) {
    gap <- n_tot_auto - nrow(retur_df)
    if (length(kommun_lista) > 1) {
      message(glue("⚠ {scb_format(gap)} företag saknades jfr totalen — sannolikt poster utan registrerad SäteKommun"))
    } else {
      message(glue("⚠ {scb_format(gap)} företag saknades jfr totalen ({scb_format(n_tot_auto)} räknade, {scb_format(nrow(retur_df))} hämtade)"))
    }
  }

  # Städa bort cachefilen vid lyckad körning (om användaren inte begärt att behålla den)
  if (!is.null(cache_path) && !behall_cache && file.exists(cache_path)) {
    tryCatch(file.remove(cache_path), error = function(e) NULL)
  }

  if (visa_resultat_meddelanden_konsol) {
    kom_txt <- if (length(kommun_lista) > 1) "kommuner" else "kommun"
    message(glue("📊 Totalt {scb_format(nrow(retur_df))} företag från {length(kommun_lista)} {kom_txt} på {scb_tid(total_start)}"))
  }
  retur_df
}

# ───────────────────────────────────────────────────────────────────────────
# scb_hamta_arbetsstallen()
# ───────────────────────────────────────────────────────────────────────────

scb_hamta_arbetsstallen <- function(
    kommunkoder = NULL,                  # vektor av kommun- och/eller länskoder, t.ex.
    #   c("2080","2081")          → Falun + Borlänge
    #   "20"                       → alla kommuner i Dalarna
    #   c("21","2080")             → hela Gävleborg + Falun
    #   "00"                       → hela Sverige
    # NULL = ingen kommunavgränsning (räkna totalen och ev. auto-loopa)
    orgnr = NULL,
    grundfilter_variabler  = list(),
    grundfilter_kategorier = list(),
    anstallda = NULL,
    branscher = NULL,
    vald_arbetsstallestatus = "1",
    auto_loopa_kommuner     = FALSE,     # om "hela riket" (kommunkoder = NULL eller "00"): TRUE = loopa Sveriges kommuner; FALSE (default) = single-shot
    auto_loopa_trosklel     = 10000L,    # gäller när auto_loopa_kommuner = TRUE: > detta antal → loopa kommuner
    cache_path              = NULL,      # filsökväg → checkpoint per kommun; NULL = ingen cache
    behall_cache            = FALSE,     # TRUE = behåll cachefilen efter framgångsrik körning
    max_forsok              = 3L,        # försök per API-anrop vid nätverksfel
    cert_thumb = scb_hamta_cert_thumb(),
    visa_meddelanden_konsol          = FALSE,
    visa_resultat_meddelanden_konsol = TRUE,
    visa_progress                    = TRUE
) {
  total_start <- Sys.time()
  base_url  <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/"
  rakna_url <- paste0(base_url, "ae/raknaarbetsstallen/")
  hamta_url <- paste0(base_url, "ae/hamtaarbetsstallen/")
  max_rader <- 2000L

  # Normalisera: NULL och "00" betyder båda "hela riket" — låt rakna+auto_loopa-
  # logiken nedan avgöra om det blir single-shot eller kommun-loop.
  if (is.null(kommunkoder) || "00" %in% as.character(kommunkoder)) {
    kommunkoder <- NULL
  } else {
    kommunkoder <- expandera_kommunkoder(kommunkoder)
  }

  .api_calls <- new.env(); .api_calls$timestamps <- numeric(0)

  rakna <- function(p) rakna_api(rakna_url, p, cert_thumb, .api_calls, visa_meddelanden_konsol, max_forsok)
  hamta <- function(p) hamta_api(hamta_url, p, cert_thumb, .api_calls, visa_meddelanden_konsol, max_forsok)

  ctx <- list(rakna = rakna, hamta = hamta, max_rader = max_rader,
              visa_medd = visa_meddelanden_konsol)

  bygg_baspayload <- function(extra_kategorier = list()) {
    kat <- grundfilter_kategorier
    if (length(extra_kategorier)) kat <- c(kat, extra_kategorier)
    if (!is.null(anstallda)) kat <- c(kat, list(list(Kategori = "Anställda",             Kod = as.list(anstallda))))
    if (!is.null(branscher)) kat <- c(kat, list(list(Kategori = "2-siffrig bransch 1",   Kod = as.list(branscher))))
    skapa_baspayload(
      typ = "arbetsstalle",
      grundfilter_kategorier = kat,
      grundfilter_variabler  = grundfilter_variabler,
      arbetsstallestatus     = vald_arbetsstallestatus
    )
  }

  # OrgNr-prefix först → 100% täckning. Kategori-strategier som backup.
  strategier <- list(
    strategi_orgnr,
    strategi_postnr,
    strategi_kategori("Anställda",
                      varden_fn = function() hamta_kategorivarden("arbetsstalle", "Anställda", cert_thumb)),
    strategi_kategori("2-siffrig bransch 1",
                      varden_fn = function() hamta_kategorivarden("arbetsstalle", "2-siffrig bransch 1", cert_thumb))
  )

  # ── Orgnr-läge ────────────────────────────────────────────────────────────
  if (!is.null(orgnr) && length(orgnr)) {
    # Ladda cache om finns (checkpoint var 100:e orgnr)
    resultat_lista <- list()
    done_orgnr <- character()
    if (!is.null(cache_path) && file.exists(cache_path)) {
      cache <- tryCatch(readRDS(cache_path), error = function(e) NULL)
      if (!is.null(cache) && is.list(cache)) {
        resultat_lista <- cache$resultat   %||% list()
        done_orgnr     <- cache$done_orgnr %||% character()
        if (visa_resultat_meddelanden_konsol)
          message(glue("♻ Återupptar från cache: {length(done_orgnr)} orgnr redan klara"))
      }
    }
    todo_orgnr <- setdiff(orgnr, done_orgnr)

    if (visa_resultat_meddelanden_konsol)
      message(glue("🔎 Hämtar arbetsställen för {scb_format(length(todo_orgnr))} orgnr..."))

    pb <- if (visa_progress) progress::progress_bar$new(
      total = length(orgnr),
      format = ":current/:total [:bar] :percent :message", clear = FALSE
    ) else list(tick = function(...) NULL)
    if (length(done_orgnr)) suppressWarnings(pb$tick(length(done_orgnr)))

    # Ett orgnr kan teoretiskt ha >2000 arbetsställen — fall tillbaka på
    # postnr-halvering om så är fallet.
    inre_strategier <- list(strategi_postnr)

    spara_var <- 100L
    for (idx in seq_along(todo_orgnr)) {
      o <- todo_orgnr[idx]
      pb$tick(tokens = list(message = o))
      p <- bygg_baspayload()
      p <- satt_variabel(p, "OrgNr (10 siffror)", o, "", "ArLikaMed")
      r <- hamta_rekursivt(p, inre_strategier, ctx, djup = 0L)
      if (!is.null(r)) resultat_lista[[length(resultat_lista) + 1]] <- r
      done_orgnr <- c(done_orgnr, o)

      if (!is.null(cache_path) && (idx %% spara_var == 0L || idx == length(todo_orgnr))) {
        tryCatch(
          saveRDS(list(resultat = resultat_lista, done_orgnr = done_orgnr), cache_path),
          error = function(e) message(glue("⚠ Kunde inte spara cache: {conditionMessage(e)}"))
        )
      }
    }

    df <- resultat_lista %>% compact() %>% list_rbind()

    if (!is.null(cache_path) && !behall_cache && file.exists(cache_path)) {
      tryCatch(file.remove(cache_path), error = function(e) NULL)
    }

    if (visa_resultat_meddelanden_konsol)
      message(glue("📊 Hämtade {scb_format(NROW(df))} arbetsställen för {length(orgnr)} orgnr på {scb_tid(total_start)}"))
    return(df)
  }

  # ── Räkna totalen (om ingen kommunavgränsning) ──────────────────────────
  # Samma logik som för företag: räkna alltid totalen när kommun är NULL,
  # för progress-bar + sanity check. Auto-loopen aktiveras bara om över tröskeln.
  n_tot_auto <- NA_integer_
  if (is.null(kommunkoder)) {
    n_tot_auto <- rakna(bygg_baspayload())
    if (!is.na(n_tot_auto) && n_tot_auto > auto_loopa_trosklel && auto_loopa_kommuner) {
      if (visa_resultat_meddelanden_konsol)
        message(glue("🌍 Hela uttaget = {scb_format(n_tot_auto)} arbetsställen — loopar Sveriges kommuner automatiskt"))
      kommunkoder <- hamta_alla_kommunkoder()
    } else if (visa_resultat_meddelanden_konsol && !is.na(n_tot_auto)) {
      message(glue("ℹ Hela uttaget = {scb_format(n_tot_auto)} arbetsställen — hämtar utan kommun-loop"))
    }
  }

  # Räknare för bortskippade rader
  skip_env <- new.env(); skip_env$rader <- 0L; skip_env$celler <- 0L
  ctx$skip_env <- skip_env

  # ── Kommunloop ────────────────────────────────────────────────────────────
  kommun_lista <- if (is.null(kommunkoder)) list(NULL) else as.list(kommunkoder)

  pb <- if (visa_progress && length(kommun_lista) > 1) progress::progress_bar$new(
    total = length(kommun_lista),
    format = ":current/:total [:bar] :percent :message", clear = FALSE
  ) else list(tick = function(...) NULL)

  # Single-shot-läge: progress-bar som tickar per hämtad rad mot totalen
  ctx$fetch_pb <- if (visa_progress && length(kommun_lista) == 1 &&
                      !is.na(n_tot_auto) && n_tot_auto > 0) {
    progress::progress_bar$new(
      total = n_tot_auto,
      format = "  📥 hämtade :current/:total [:bar] :percent eta :eta",
      clear = FALSE, show_after = 0
    )
  } else NULL

  # Ladda cache om finns (per-kommun checkpoint)
  resultat <- list()
  done_kommuner <- character()
  if (!is.null(cache_path) && file.exists(cache_path)) {
    cache <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(cache) && is.list(cache)) {
      resultat      <- cache$resultat      %||% list()
      done_kommuner <- cache$done_kommuner %||% character()
      if (visa_resultat_meddelanden_konsol)
        message(glue("♻ Återupptar från cache: {length(done_kommuner)} kommuner redan klara ({scb_format(sum(map_int(resultat, NROW)))} rader)"))
    }
  }

  for (kom in kommun_lista) {
    # Hoppa över redan klara kommuner från cache
    if (!is.null(kom) && kom %in% done_kommuner) {
      pb$tick(tokens = list(message = paste(kom, "(cache)")))
      next
    }

    kom_start <- Sys.time()
    pb$tick(tokens = list(message = kom %||% "(alla)"))

    extra_kat <- if (!is.null(kom)) list(list(Kategori = "Kommun", Kod = list(kom))) else list()
    payload <- bygg_baspayload(extra_kategorier = extra_kat)

    df <- hamta_rekursivt(payload, strategier, ctx, djup = 0L)
    if (!is.null(df) && nrow(df)) {
      if (!is.null(kom)) df$kod_kommun <- kom
      resultat[[length(resultat) + 1]] <- df
    }

    # Spara checkpoint efter varje klar kommun
    if (!is.null(kom)) done_kommuner <- c(done_kommuner, kom)
    if (!is.null(cache_path)) {
      tryCatch(
        saveRDS(list(resultat = resultat, done_kommuner = done_kommuner), cache_path),
        error = function(e) message(glue("⚠ Kunde inte spara cache: {conditionMessage(e)}"))
      )
    }

    if (visa_resultat_meddelanden_konsol) {
      n_kom <- if (!is.null(df)) nrow(df) else 0L
      message(glue("🏁 {kom %||% 'alla kommuner'}: hämtade {scb_format(n_kom)} arbetsställen på {scb_sec(kom_start)} sek"))
    }
  }

  retur_df <- resultat %>% list_rbind()

  # Bortskippade celler (alla strategier slut)
  if (skip_env$celler > 0L) {
    message(glue("⚠ Hoppade över {skip_env$celler} celler ({scb_format(skip_env$rader)} räknade rader) — strategierna räckte inte hela vägen"))
  }

  # Sanity check: jämför resultat med auto-totalen.
  if (!is.na(n_tot_auto) && n_tot_auto > nrow(retur_df)) {
    gap <- n_tot_auto - nrow(retur_df)
    if (length(kommun_lista) > 1) {
      message(glue("⚠ {scb_format(gap)} arbetsställen saknades jfr totalen — sannolikt poster utan registrerad Kommun"))
    } else {
      message(glue("⚠ {scb_format(gap)} arbetsställen saknades jfr totalen ({scb_format(n_tot_auto)} räknade, {scb_format(nrow(retur_df))} hämtade)"))
    }
  }

  # Städa bort cachefilen vid lyckad körning (om användaren inte begärt att behålla den)
  if (!is.null(cache_path) && !behall_cache && file.exists(cache_path)) {
    tryCatch(file.remove(cache_path), error = function(e) NULL)
  }

  if (visa_resultat_meddelanden_konsol) {
    kom_txt <- if (length(kommun_lista) > 1) "kommuner" else "kommun"
    message(glue("📊 Totalt {scb_format(nrow(retur_df))} arbetsställen från {length(kommun_lista)} {kom_txt} på {scb_tid(total_start)}"))
  }
  retur_df
}

# ───────────────────────────────────────────────────────────────────────────
# scb_hamta_arbetsstallen_med_foretag()
# ───────────────────────────────────────────────────────────────────────────
#
# Hämtar arbetsställen (filter på kommun/län/hela landet/orgnr) och
# motsvarande moderföretag i två faser. Returnerar en list med två tibbles:
#
#   $arbetsstallen  — en rad per arbetsställe
#   $foretag        — en rad per unikt företag (en API-call per orgnr)
#
# Joina vid behov:  left_join(res$arbetsstallen, res$foretag, by = "OrgNr")
#
# Cache: cache_path = "scb.rds" skapar två separata filer:
#   scb_arbst.rds   (fas 1)
#   scb_ftg.rds     (fas 2)
# Båda raderas automatiskt när hela körningen lyckas (behall_cache = FALSE).

scb_hamta_arbetsstallen_med_foretag <- function(
    kommunkoder = NULL,                            # vektor av kommun- och/eller länskoder
    #   ("2080", "20", "00" etc — se scb_hamta_foretag())
    orgnr       = NULL,                            # om angivet hoppas fas 1 över; använd orgnr som källa
    # Filter för arbetsställefasen
    grundfilter_variabler_arbst  = list(),
    grundfilter_kategorier_arbst = list(),
    anstallda  = NULL,
    branscher  = NULL,
    vald_arbetsstallestatus = "1",
    # Filter för företagsfasen
    grundfilter_variabler_ftg  = list(),
    grundfilter_kategorier_ftg = list(),
    vald_foretagsstatus      = "1",
    vald_registreringsstatus = "1",
    # Gemensamt
    auto_loopa_kommuner = FALSE,     # default: single-shot för hela riket (snabbare + 100% täckning via OrgNr-prefix)
    auto_loopa_trosklel = 10000L,
    cache_path   = NULL,
    behall_cache = FALSE,
    max_forsok   = 3L,
    batch_via_prefix = TRUE,        # fas 2 (företag via orgnr): prefix-batchning på/av
    prefix_start     = 4L,          # fas 2: startprefix-längd
    riks_varning_trosklel = 10000L, # > detta antal unika orgnr → varna att riks-strategin oftast är snabbare
    riks_varning_paus     = 5L,     # sek att pausa efter varning så användaren hinner avbryta (0 = ingen paus)
    cert_thumb = scb_hamta_cert_thumb(),
    visa_meddelanden_konsol          = FALSE,
    visa_resultat_meddelanden_konsol = TRUE,
    visa_progress                    = TRUE
) {
  total_start <- Sys.time()

  # Två separata cachefiler (en per fas) härledda från cache_path
  cache_arbst <- if (!is.null(cache_path)) sub("\\.rds$", "", cache_path, ignore.case = TRUE) %>% paste0("_arbst.rds") else NULL
  cache_ftg   <- if (!is.null(cache_path)) sub("\\.rds$", "", cache_path, ignore.case = TRUE) %>% paste0("_ftg.rds")   else NULL

  # ── Fas 1: Arbetsställen ────────────────────────────────────────────────
  if (visa_resultat_meddelanden_konsol)
    message("━━━ Fas 1/2: Hämtar arbetsställen ━━━")

  arbst <- scb_hamta_arbetsstallen(
    kommunkoder = kommunkoder,
    orgnr       = orgnr,
    grundfilter_variabler  = grundfilter_variabler_arbst,
    grundfilter_kategorier = grundfilter_kategorier_arbst,
    anstallda  = anstallda,
    branscher  = branscher,
    vald_arbetsstallestatus = vald_arbetsstallestatus,
    auto_loopa_kommuner = auto_loopa_kommuner,
    auto_loopa_trosklel = auto_loopa_trosklel,
    cache_path   = cache_arbst,
    behall_cache = behall_cache,
    max_forsok   = max_forsok,
    cert_thumb = cert_thumb,
    visa_meddelanden_konsol          = visa_meddelanden_konsol,
    visa_resultat_meddelanden_konsol = visa_resultat_meddelanden_konsol,
    visa_progress                    = visa_progress
  )

  if (is.null(arbst) || !NROW(arbst)) {
    message("⚠ Inga arbetsställen hittades — hoppar över företagsfasen")
    return(list(arbetsstallen = arbst %||% tibble(), foretag = tibble()))
  }

  # Plocka ut unika orgnr ur arbetsställeresultatet
  orgnr_kol <- hitta_orgnr_kolumn(arbst)
  if (is.na(orgnr_kol)) {
    message("⚠ Hittade ingen OrgNr-kolumn i arbetsställeresultatet — kan inte hämta företag")
    return(list(arbetsstallen = arbst, foretag = tibble()))
  }

  orgnr_unika <- unique(as.character(arbst[[orgnr_kol]]))
  orgnr_unika <- orgnr_unika[!is.na(orgnr_unika) & nzchar(orgnr_unika)]

  # Varning för stora uttag: prefix-batchningen ger 20–80× bara om orgnren
  # ligger tätt inom prefix-grupperna. För glesa, breda listor (typ alla orgnr
  # i ett län) är det oftast snabbare att hämta hela riket separat och joina
  # i databasen — det fångar dessutom moderbolag med säte i annan kommun.
  if (!is.null(riks_varning_trosklel) &&
      length(orgnr_unika) > riks_varning_trosklel &&
      visa_resultat_meddelanden_konsol) {
    message("")
    message(glue("⚠ {scb_format(length(orgnr_unika))} unika orgnr — för stora uttag är riks-strategin ofta snabbare:"))
    message("     ftg   <- scb_hamta_foretag(kommunkoder = \"00\", cache_path = \"ftg.rds\")")
    message("     arbst <- scb_hamta_arbetsstallen(kommunkoder = \"00\", cache_path = \"arbst.rds\")")
    message("   Den vägen fångar dessutom moderbolag med säte i annan kommun än arbetsstället.")
    if (isTRUE(riks_varning_paus > 0L)) {
      message(glue("   Fortsätter ändå om {riks_varning_paus} sek — Ctrl+C för att avbryta."))
      Sys.sleep(riks_varning_paus)
    }
    message("")
  }

  # ── Fas 2: Företag ──────────────────────────────────────────────────────
  if (visa_resultat_meddelanden_konsol)
    message(glue("━━━ Fas 2/2: Hämtar {scb_format(length(orgnr_unika))} unika företag ━━━"))

  ftg <- scb_hamta_foretag(
    orgnr = orgnr_unika,
    grundfilter_variabler  = grundfilter_variabler_ftg,
    grundfilter_kategorier = grundfilter_kategorier_ftg,
    vald_foretagsstatus      = vald_foretagsstatus,
    vald_registreringsstatus = vald_registreringsstatus,
    cache_path   = cache_ftg,
    behall_cache = behall_cache,
    max_forsok   = max_forsok,
    batch_via_prefix = batch_via_prefix,
    prefix_start     = prefix_start,
    cert_thumb = cert_thumb,
    visa_meddelanden_konsol          = visa_meddelanden_konsol,
    visa_resultat_meddelanden_konsol = visa_resultat_meddelanden_konsol,
    visa_progress                    = visa_progress
  )

  if (visa_resultat_meddelanden_konsol)
    message(glue("📦 Klart: {scb_format(NROW(arbst))} arbetsställen + {scb_format(NROW(ftg))} företag på {scb_tid(total_start)}"))

  list(arbetsstallen = arbst, foretag = ftg %||% tibble())
}

# ───────────────────────────────────────────────────────────────────────────
# Räknefunktioner (snabb översikt utan att hämta)
# ───────────────────────────────────────────────────────────────────────────

scb_rakna_foretag_i_kommuner <- function(
    kommunkoder,
    foretagsstatus = "1",
    registreringsstatus = "1",
    extra_kategorier = NULL,
    cert_thumb = scb_hamta_cert_thumb(),
    visa_konsol = TRUE
){
  rakna_url <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/je/raknaforetag/"
  kommunkoder <- expandera_kommunkoder(kommunkoder)
  .api_calls <- new.env(); .api_calls$timestamps <- numeric(0)

  map_dfr(kommunkoder, function(kom){
    kat <- list(list(Kategori = "SätesKommun", Kod = list(kom)))
    if (!is.null(extra_kategorier)) kat <- c(kat, extra_kategorier)
    payload <- list(Kategorier = kat)
    if (!is.null(foretagsstatus))      payload$Företagsstatus      <- foretagsstatus
    if (!is.null(registreringsstatus)) payload$Registreringsstatus <- registreringsstatus
    n <- rakna_api(rakna_url, payload, cert_thumb, .api_calls)
    if (visa_konsol) message(glue("📏 {kom}: {n %||% NA} företag"))
    tibble(kommun = kom, antal_foretag = as.integer(n))
  })
}

scb_rakna_arbetsstallen_i_kommuner <- function(
    kommunkoder,
    arbetsstallestatus = "1",
    extra_kategorier = NULL,
    cert_thumb = scb_hamta_cert_thumb(),
    visa_konsol = TRUE
){
  rakna_url <- "https://privateapi.scb.se/nv0101/v1/sokpavar/api/ae/raknaarbetsstallen/"
  kommunkoder <- expandera_kommunkoder(kommunkoder)
  .api_calls <- new.env(); .api_calls$timestamps <- numeric(0)

  map_dfr(kommunkoder, function(kom){
    kat <- list(list(Kategori = "Kommun", Kod = list(kom)))
    if (!is.null(extra_kategorier)) kat <- c(kat, extra_kategorier)
    payload <- list(Arbetsställestatus = arbetsstallestatus, Kategorier = kat)
    n <- rakna_api(rakna_url, payload, cert_thumb, .api_calls)
    if (visa_konsol) message(glue("📏 {kom}: {n %||% NA} arbetsställen"))
    tibble(kommun = kom, antal_arbetsstallen = as.integer(n))
  })
}

# ───────────────────────────────────────────────────────────────────────────
# Formatering
# ───────────────────────────────────────────────────────────────────────────

scb_format <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)
scb_sec    <- function(start_time) round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)

# Formaterar förfluten tid läsbart: "12 sek", "3 min 14 sek", "1h 15m 12s"
scb_tid <- function(start_time) {
  s <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (s < 60)   return(sprintf("%.1f sek", s))
  if (s < 3600) return(sprintf("%d min %d sek", as.integer(s %/% 60), as.integer(round(s %% 60))))
  h <- as.integer(s %/% 3600); rest <- s - h * 3600
  sprintf("%dh %dm %ds", h, as.integer(rest %/% 60), as.integer(round(rest %% 60)))
}


# ───────────────────────────────────────────────────────────────────────────
# Exempel på användning
# ───────────────────────────────────────────────────────────────────────────
#
# # 1) Alla verksamma företag i Falun och Borlänge
# df <- scb_hamta_foretag(kommunkoder = c("2080", "2081"))
#
# # 2) Hela Stockholms kommun (kommer kräva uppdelning)
# df <- scb_hamta_foretag(kommunkoder = "0180")
#
# # 3) Endast ideella föreningar i Dalarna
# df <- scb_hamta_foretag(
#   kommunkoder  = c("2080","2081","2082","2083","2084","2085","2026","2029","2031","2034","2039","2061","2062","2080","2085"),
#   juridisk_kod = "61"
# )
#
# # 4) Hämta specifika orgnr
# df <- scb_hamta_foretag(orgnr = c("5560000000","5560000001"))
#
# # 5) Arbetsställen med >=50 anställda i Falun (Anställda-koden för 50-99: "6")
# df <- scb_hamta_arbetsstallen(kommunkoder = "2080", anstallda = c("6","7","8","9"))
#
# # 6) Företag vars firma innehåller "ask" i Stockholm + har e-post
# df <- scb_hamta_foretag(
#   kommunkoder = "0180",
#   grundfilter_variabler = list(
#     list(Varde1 = "",    Varde2 = "", Operator = "Finns",       Variabel = "E-post"),
#     list(Varde1 = "ask", Varde2 = "", Operator = "Innehaller",  Variabel = "Firma")
#   )
# )
#
# # 7) Alla företag i hela Sverige — auto-läget loopar kommunerna åt dig
# df <- scb_hamta_foretag()
#
# # 8) Tvinga "single-shot"-läge utan kommun-loop (snabbt om filtret är smalt)
# df <- scb_hamta_foretag(
#   grundfilter_variabler = list(list(Varde1="ask", Varde2="", Operator="Innehaller", Variabel="Firma")),
#   auto_loopa_kommuner = FALSE
# )
#
# # 9) Arbetsställen + tillhörande företag i Falun
# res <- scb_hamta_arbetsstallen_med_foretag(kommunkoder = "2080")
# res$arbetsstallen   # tibble med arbetsställen
# res$foretag         # tibble med företag (en rad per unikt orgnr)
# # Slå ihop till bred tabell vid behov:
# bred <- dplyr::left_join(res$arbetsstallen, res$foretag, by = "OrgNr")
#
# # 10) Hela Sverige med cache (återupptagningsbar vid krasch)
# res <- scb_hamta_arbetsstallen_med_foretag(cache_path = "sverige.rds")
# # Skapar sverige_arbst.rds och sverige_ftg.rds under körningen, raderas vid lyckad körning
#
# # 11) Endast arbetsställen med >=50 anställda + deras moderföretag
# res <- scb_hamta_arbetsstallen_med_foretag(
#   kommunkoder = c("2080","2081"),
#   anstallda   = c("6","7","8","9")
# )
#
# # 12) Hämta många orgnr — adaptiv prefix-batchning är på som default och
# #     ger oftast 20–80x färre API-anrop än en call per orgnr.
# df <- scb_hamta_foretag(orgnr = mina_orgnr)                   # batchat (default)
# df <- scb_hamta_foretag(orgnr = mina_orgnr, prefix_start = 5) # finare startindelning
# df <- scb_hamta_foretag(orgnr = mina_orgnr, batch_via_prefix = FALSE)  # gamla läget
