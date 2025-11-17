
library(httr2)
library(rvest)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

hamta_brott_ntu_lan_bra <- function() {

  # skript för att ladda ner dataset från BRÅ över NTU uppdelat per län och kön
  # datasetet finns i en excelfil på webbsidan nedan, skriptet extraherar rätt länk
  # sparar filen i temporär mapp som sedan läses in till en lista per variabel.
  #
  # Datasetet innehåller flera år, alla län och Hela landet samt totalt, kvinnor och män.
  # Skriptet bör klara årliga uppdateringar om inte BRÅ byter webbadress till sidan där
  # länken finns eller namn på filen som i skriptet måste innehålla texten "län", "resultat"
  # samt "ntu".

  bra_ntu_url <- "https://bra.se/statistik/statistik-fran-enkatundersokningar/nationella-trygghetsundersokningen"

  bra_ntu_lankar <- read_html(bra_ntu_url) %>%                  # läs in webbsidan ovan
    html_elements("a") %>%                                # hitta alla <a>-objekt
    html_attr("href") %>%                                 # ta ut alla länkar, dvs. <href>-objekt
    purrr::discard(is.na)

  avkodade_lankar <- bra_ntu_lankar %>%
    URLdecode()

  ntu_lan_index <- which(                                       # koda om till å, ä och ö istället för koder
    str_detect(avkodade_lankar, "(?i)län") &                             # behåll alla länkar som innehåller "län"
      str_detect(avkodade_lankar, "(?i)resultat") &                        # behåll alla länkar som innehåller "resultat"
      str_detect(avkodade_lankar,"(?i)ntu"))

  inlasfil <- bra_ntu_lankar[ntu_lan_index] %>%
    paste0("https://bra.se", .)

  tmpfile <- tempfile(fileext = ".xlsx")

  resp <- request(inlasfil) %>% req_perform()

  resp %>% resp_body_raw() %>% writeBin(tmpfile)

  dataflikar <- excel_sheets(tmpfile) %>%                            # läs in alla flikar
    purrr::keep(~ !stringr::str_detect(.x, "Tabellförteckning"))     # utom fliken "Tabellförteckning"

  ntu_lista <- purrr::map(dataflikar, ~ {
    # läs in excelfilen i varje dataflik
    inlas_df <- read_excel(tmpfile, sheet = .x, .name_repair = "minimal", col_types = "text")
    dataset_namn <- str_remove(inlas_df[1,1], "^[^.]*\\.") %>%      # extrahera datasetnamn från värdet i rad 1 kolumn 1
      str_trim()

    inlas_df[2,1] <- "kon"          # döp kolumn 1 till "kon"
    inlas_df[2,2] <- "region"       # döp kolumn 2 till "region"

    inlas_df <- inlas_df %>%
      set_names(as.character(inlas_df[2, ])) %>%       # gör kolumnnamn av rad 2
      slice(-c(1:2)) %>%                               # ta bort rad 1 och 2
      fill(kon, .direction = "down") %>%           # fyll nedåt i kolumn 1, dvs. "kon"
      pivot_longer(                                # gör om till long-dataformat
        cols = -(kon:region),                      # alla kolumner efter region
        names_to = "ar",                           # skapa kolumnen "ar"
        values_to = "varde",                       # skapa kolumn "varde" för alla värden
        values_transform = list(varde = as.numeric)    # gör om värde till numerisk
      )

    set_names(list(inlas_df), dataset_namn)        # returnera namngiven lista där datasetnamnet från ovan används

  }) %>%
    purrr::flatten()                               # kör en flatten så att vi får en lista med dataframes som är namngivna

  unlink(tmpfile, recursive = TRUE, force = TRUE)
  return(ntu_lista)
}

