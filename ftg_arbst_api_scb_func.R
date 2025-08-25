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
      filter(grepl("sokpavar|sökpåver|sokpåver", subject_lc) | grepl("\\bscb\\b", issuer_lc)) |>
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
