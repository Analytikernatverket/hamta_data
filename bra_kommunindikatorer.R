
hamta_bra_kommunindikatorer <- function(
  region_vekt = "20",            # det går att skicka in både läns- och kommunkoder, vid länskoder så hämtas alla kommuner i länet
  headless = TRUE,
  timeout = 30,
  verbose = TRUE
  ){

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         selenider,
         readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_webbskrapning.R")
  
  region_vekt_kommuner <- region_vekt[nchar(region_vekt) == 4] %>%
    hamtaregion_kod_namn() %>%
    dplyr::pull(region)
  
  region_vekt_lan <- region_vekt[nchar(region_vekt) == 2]
  
  valda_kommuner <- hamtakommuner(region_vekt[nchar(region_vekt) == 2], F, F) %>%
    hamtaregion_kod_namn() %>%
    dplyr::pull(region) %>%
    c(., region_vekt_kommuner)
  
  # ========================== kör webbscraping-delen ===========================================================================
  #                            spara data som en fil i tmpdir, 
  #                            för att vi inte kan läsa in data direkt utan måste spara 
  #                            den genom att klicka på ladda ner fil på webbsidan
  
  logg <- loggning_initiera()
  
  # skapa temporär mapp
  tmpdir <- tempfile("bra_kommunindikatorer_")
  dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
  
  logg("Startar skrapsession ...")
  sess <- starta_skrapsession(headless = headless)
  
  on.exit(suppressMessages(stang_skrapsession(sess)), add = TRUE)
  
  url <- "https://bra.se/statistik/indikatorer-for-kommuners-lagesbild"
  logg("Öppnar webbsidan ...")
  selenider::open_url(url, session = sess$session)
  
  xlsx_fillista <- map(valda_kommuner, function(kommun) {
    logg(paste0("Hämtar data för kommun: ", kommun))
    
    sess |> hitta("#search12_1ad88ffe19545ea6d3113b69") |> selenider::elem_expect(is_visible, timeout = 30)
    textruta_inmatning(sess, "#search12_1ad88ffe19545ea6d3113b69", kommun)
    
    # vänta på att länken till Excel-filen dyker upp
    encoded <- utils::URLencode(kommun, reserved = TRUE)
    css_lank <- sprintf('a[href$="%s.xlsx"]', encoded)
    
    sess |> hitta(css_lank) |> selenider::elem_expect(is_visible, timeout = timeout)
    
    href <- hamta_attribut(sess, css_lank, "href")
    full_url <- paste0("https://bra.se", href)
    
    fil <- hamta_nedladdning(
      sess,
      trigger = function() klicka_via_id(sess, css_lank),
      nedladdningsmapp = tmpdir,
      monster = "\\.xlsx$",
      timeout = timeout
    )
    return(fil)
  }) |> unlist()


  filnamn <- basename(xlsx_fillista)

  bra_xlsx_list <- xlsx_fillista %>%
    set_names(basename(xlsx_fillista) %>% str_remove(".xlsx")) %>%
    map(function(fil) {
    flikar <- excel_sheets(fil)
    suppressMessages(
      map(set_names(flikar), ~ readxl::read_xlsx(fil, sheet = .x, col_names = FALSE)))
  }, .progress = TRUE)

  las_in_data_fran_flik <- function(fil, fliknamn){

    # läs in om det är en anmälda brott-flik ===============================================================================
    if (str_detect(fliknamn, "(anmälda)")){

      # läs in data
      anmalda_data <- fil[[fliknamn]]
      innehall <- anmalda_data[[1,1]]

      anmalda_data[[2,1]] <- "geografi"
      kolumn_namn <- slice(anmalda_data, 2) %>% as.character()
      anmalda_data <- anmalda_data %>% slice(3:nrow(anmalda_data))
      names(anmalda_data) <- kolumn_namn

      anmalda_data_long <- anmalda_data %>%
        pivot_longer(
          cols = -geografi,
          names_to = c("enhet", "ar"),
          names_pattern = "^(.*) (\\d{4})$",
          values_to = "varde"
        ) %>%
        mutate(kalla = "Anmälda brott",
               varde = varde %>% as.numeric(),
               variabel = innehall %>%
                 str_remove(" i kommunen, länet.*") %>%
                 str_remove(fixed("(anmälda brott)")) %>%
                 str_trim()
               ) %>%
        relocate(variabel, .after = ar)

      return(anmalda_data_long)
    } # slut if-sats för om det är en anmälda brott-flik

    # läs in om det är en NTU-flik, eller otrygghets-flikar ===============================================================================
    #if (str_detect(fliknamn, "(NTU)")) {
    if (!str_detect(fliknamn, "(anmälda)")) {

      ntu_data <- fil[[fliknamn]]
      innehall <- ntu_data[[1,1]]
      ar_rader <- which(str_detect(ntu_data[[1]], "^\\d{4}$"))
      rubrik_rad <- ar_rader %>%
        min() - 1
      ar_start <- ar_rader %>% min()
      ar_slut <- ar_rader %>% max()

      geografier <- ntu_data[rubrik_rad-1,] %>% as.character() %>% .[!is.na(.)]
      ntu_data[[rubrik_rad,1]] <- "ar"
      kolumn_namn <- slice(ntu_data, rubrik_rad) %>% as.character()
      ntu_data <- ntu_data %>%
        slice(ar_start:ar_slut)
      names(ntu_data) <- kolumn_namn

      # hitta första
      start_kol <- which(str_detect(tolower(kolumn_namn), "andel|antal")) %>% min()
      slut_kol <- which(str_detect(tolower(kolumn_namn), "ki") & str_detect(tolower(kolumn_namn), "övre")) %>% min()
      if (is.infinite(slut_kol)) slut_kol <- start_kol
      block_storlek <- length(slut_kol:start_kol)

      # 1) Skapa nya, unika kolumnnamn "Variabel_Geografi" för blocken
      ursprung_kolumner <- names(ntu_data)[-1]                      # alla utom 'ar'
      antal_kol        <- length(ursprung_kolumner)

      # Geografiindex per kolumn: 1,1,1, 2,2,2, 3,3,3, ...
      geo_idx <- ceiling(seq_along(ursprung_kolumner) / block_storlek)
      geo_namn <- geografier[geo_idx]

      nya_cols <- paste0(ursprung_kolumner, "_", geo_namn)

      # Döp om kolumnerna i ntu_data
      names(ntu_data)[-1] <- nya_cols

      # 2) Long-isera så att .value blir värdekolumner och "geografi" blir nyckel
      ntu_data_long <- ntu_data %>%
        pivot_longer(
          cols      = -ar,
          names_to  = c(".value", "geografi"),   # .value skapar kolumnerna Andel/KI_undre/KI_ovre
          names_sep = "_"
        ) %>%
        # 3)  # 3) Städa värden: '..' -> NA och parse till numeriskt där det går
        mutate(
          across(ursprung_kolumner[1:3],
                 ~ na_if(as.character(.), "..") %>% parse_number())
        ) %>%
        arrange(geografi, ar) %>%
        filter(if_any(any_of(c("Andel", "Antal")), ~ !is.na(.))) %>% # ta bort rader utan värden
        pivot_longer(
          cols = -c(ar, geografi),
          names_to = "enhet",
          values_to = "varde"
        ) %>%
        mutate(kalla = "Nationella trygghetsundersökningen (NTU)",
               varde = varde %>% as.numeric(),
               variabel = str_remove(innehall, ", enligt NTU.*") %>%
                 str_remove(" [0-9]{4}[-–][0-9]{4}"),
               enhet = if_else(str_detect(enhet, "KI "), enhet, str_extract(innehall, "(?<=NTU [0-9]{4}[-–][0-9]{4}\\. ).*$"))) %>%
        relocate(variabel, .after = ar)


      return(ntu_data_long)
    } # slut if-sats för om det är en NTU-flik

  } # slut funktion för att hämta data i en flik

  dataset_bra <- map(bra_xlsx_list, function(fil) {
    map(names(fil), ~ las_in_data_fran_flik(fil = fil, fliknamn = .x)) %>%
      list_rbind()      # bind ihop alla flikar till en dataframe
  }) %>%
    list_rbind() %>%    # bind ihop alla excelfiler till en dataframe
    distinct()

  return(dataset_bra)
}
