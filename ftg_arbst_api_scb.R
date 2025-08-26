source("https://raw.githubusercontent.com/Analytikernatverket/hamta_data/refs/heads/main/ftg_arbst_api_scb_func.R")

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
}

