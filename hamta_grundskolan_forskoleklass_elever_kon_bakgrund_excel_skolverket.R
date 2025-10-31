
hamta_elever_grundskola_forskoleklass_skolverket <- function(
  hamta_kommuner = TRUE,
  hamta_lan = TRUE,
  hamta_grundskola = TRUE,
  hamta_forskoleklass = TRUE,
  meddelande_tid = TRUE
  ) {

  # =====================================================================================================================
  #
  # Skript för att hämta antal elever per årskurs i grundskolan + förskoleklass  från Skolverket per kommun och/eller län.
  # Detta skript hämtar alla kommuner och/eller län i en excelfil som innehåller en tidsserie. Det går således inte att
  # snabba upp skriptet genom att enbart välja någon eller några få kommuner och/eller län men man kan hämta filer för
  # ett eller flera år.
  #
  # Skapat av: Peter Möller, Region Dalarna, i oktober 2025.
  # Reviderat:
  #
  # =====================================================================================================================

  starttid <- Sys.time()

  library(tidyverse)
  library(readxl)
  library(httr)

  tabeller_url <- list(
    Grundskola_kommun = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=455&pAr=2024&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=4271ADD41ADEE036E06311BA650A7E92&pFlikar=1&pVerkform=11",
    Grundskola_lan = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=454&pAr=2024&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=4271ADD41ADEE036E06311BA650A7E92&pFlikar=1&pVerkform=11",
    Förskoleklass_kommun = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=479&pAr=2024&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=4271ADD41ADEE036E06311BA650A7E92&pFlikar=1&pVerkform=14",
    Förskoleklass_lan = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=478&pAr=2024&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=4271ADD41ADEE036E06311BA650A7E92&pFlikar=1&pVerkform=14"
  )

  if (!hamta_kommuner) tabeller_url <- tabeller_url[!str_detect(names(tabeller_url), "kommun")]
  if (!hamta_lan) tabeller_url <- tabeller_url[!str_detect(names(tabeller_url), "lan")]
  if (!hamta_grundskola) tabeller_url <- tabeller_url[!str_detect(names(tabeller_url), "Grundskola")]
  if (!hamta_forskoleklass) tabeller_url <- tabeller_url[!str_detect(names(tabeller_url), "Förskoleklass")]
  if (length(tabeller_url) == 0) stop("Minst en av parametrarna hamta_kommuner och hamta_lan respektive hamta_grundskola och hamta_forskoleklass måste vara TRUE.")

  # fixa med år
  akt_ar <- format(Sys.Date(), "%Y") %>% as.numeric()
  ar_prefix <- "&pAr="
  ar_med_prefix <- paste0(ar_prefix, "\\d{4}")

  # skapa en url för grundskola respektive förskoleklass för det senaste året, vilket räcker då Excelfilerna från Skolverket innehåller
  # tidsserier
  senaste_ar_url <- imap(tabeller_url, ~ {
    har_innehall <- FALSE
    test_ar <- akt_ar
    while (!har_innehall) {
        url_test_senaste_ar <- .x %>% str_replace(ar_med_prefix, paste0(ar_prefix, test_ar))         # skapa en url för det år vi har nu
        GET(url_test_senaste_ar, write_disk(test_url <- tempfile(fileext = ".xlsx")))                    # hämta en fil med året vi har nu
        har_innehall <- if (file.size(test_url) < 25000) FALSE else TRUE    # om filen har innehåll (dvs. är större än 25 kb) så finns data för aktuellt år, annars är förra året senaste år i tabellen
        if (test_ar < 1990) return(NULL)
        if (har_innehall) return(url_test_senaste_ar) else test_ar <- test_ar - 1
      }
  }) %>% set_names(names(tabeller_url))

  las_in_excelfil_med_flikar <- function(url_excel, filnamn) {

    GET(url_excel, write_disk(tf_excel <- tempfile(fileext = ".xlsx")))                    # hämta själva filen

    flikar <- excel_sheets(tf_excel) %>% .[!str_detect(., "Rapport")]                                             # hämta alla fliknamn

    # hitta rätt startrad, skiljer sig lite mellan olika år
    rad_df <- read_excel(tf_excel, sheet = 1, range = "A1:A30", col_names = "kommun")
    start_rad <- which(grepl("kommun|län", tolower(rad_df[[1]])))[1]

    retur_flikar_df <- map(flikar, ~ suppressWarnings(read_excel(tf_excel, skip = start_rad-1, sheet = .x, col_types = "text") %>%
                         mutate(across(where(is.character) & starts_with("\\d{4}"), parse_number)) %>%
                         pivot_longer(matches("\\d{4}"), values_to = "varde", names_to = "lasar") %>%   #
                         rename(variabel = Elever)) %>%                                                 # kolumnen Elever döps om till variabler
                           mutate(huvudman = .x)) %>%                                                   # lägg till kolumn med namn från fliknamnet = huvudman
      list_rbind() %>%                                                                                  # bind ihop alla flikar till en df
      mutate(skoltyp = filnamn %>% str_remove("_.*"))                                                   # döp dataset till Grundskola eller Förskola, vi tar bort huruvida det kommer från kommun- eller länsdataset

    # Vi tar bort länskod och län i kommundataset, och döper om till regionkod och region för både kommuner och län
    if ("Kommun" %in% names(retur_flikar_df)) {
      retur_flikar_df <- retur_flikar_df %>%
        rename(regionkod = Kommunkod,
               region = Kommun) %>%
        select(-matches("^län", ignore.case = TRUE))
    } else {
      kod_kol <- intersect(c("Länskod", "Läns-kod"), names(retur_flikar_df))
      retur_flikar_df <- retur_flikar_df %>%
        rename(regionkod = all_of(kod_kol),
               region = Län)
    }

    return(retur_flikar_df)
  } # slut funktion las_in_excelfil_med_flikar

  # här hämtar vi alla filer som finns i senaste_ar_url
  retur_df <- imap(senaste_ar_url, ~ las_in_excelfil_med_flikar(url_excel = .x, filnamn = .y)) %>%
    list_rbind() %>%
    relocate(lasar, .before = 1) %>%
    relocate(skoltyp, .after = lasar) %>%
    relocate(huvudman, .after = skoltyp)

  berakningstid <- as.numeric(Sys.time() - starttid, units = "secs") %>% round(1)         # Beräkna och skriv ut tidsåtgång
  if (meddelande_tid) cat(paste0("Dataset nedladdat på ", berakningstid, " sekunder."))

  return(retur_df)
} # slut funktion
