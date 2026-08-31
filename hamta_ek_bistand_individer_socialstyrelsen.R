
hamta_ek_bistand_individer_socialstyrelsen <- function(
    headless = TRUE,
    timeout = 30,
    verbose = TRUE
    ) {
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         selenider,
         readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_webbskrapning.R")
  
  #source("G:/skript/peter/hamta_ek_bistand_individer_socialstyrelsen_funktion.R")
  
  # ========================== kör webbscraping-delen ===========================================================================
  #                            spara data som en fil i tmpdir, 
  #                            för att vi inte kan läsa in data direkt utan måste spara 
  #                            den genom att klicka på ladda ner fil på webbsidan
  
  logg <- loggning_initiera()
  
  # skapa temporär mapp
  tmpdir <- normalizePath(tmpdir, winslash = "/", mustWork = FALSE)
  dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
  
  logg("Startar skrapsession ...")
  sess <- starta_skrapsession(headless = headless)
  
  on.exit(suppressMessages(stang_skrapsession(sess)), add = TRUE)
  
  url <- "https://sdb.socialstyrelsen.se/if_ekb_manad/val.aspx"
  logg("Öppnar webbsidan ...")
  selenider::open_url(url, session = sess$session)
  
  # --- vänta in att sidan laddats: tabellvalslistan är ett stabilt kriterium ---
  sess |> hitta("#TABELL") |> selenider::elem_expect(is_visible, timeout = timeout)
  
  logg("Sidan laddad. Väljer alla kommuner och län ...")
  
  # --- klicka "Alla kommuner" och "Alla län" (globala länkarna högst upp) ---
  # klicka_via_text() istället för s_text()+elem_click(): sidan har ~290
  # "Alla kommuner"-länkar (en per län), vilket gjorde den xpath-baserade
  # varianten mycket långsam.
  klicka_via_text(sess, "Alla kommuner")
  klicka_via_text(sess, "Alla län")
  
  logg("Väljer tabell (Biståndsmottagare) ...")
  
  # --- välj tabell: Biståndsmottagare (individer) i select-listan #TABELL ---
  tabell_options <- hamta_select_options(sess, "#TABELL")
  
  mottagare_ix <- grep("mottagare", tabell_options$text, ignore.case = TRUE)
  if (length(mottagare_ix) != 1) {
    stop(
      "Kunde inte entydigt hitta 'Biståndsmottagare' i #TABELL. ",
      "Alternativen var: ", paste(tabell_options$text, collapse = " | ")
    )
  }
  sess |> hitta("#TABELL") |> selenider::elem_select(value = tabell_options$value[[mottagare_ix]])
  
  # Tabellbytet kan trigga en postback som ritar om formuläret - vänta in
  # att årslistan finns och är synlig innan vi fortsätter.
  sess |> hitta("#AR") |> selenider::elem_expect(is_visible, timeout = timeout)
  
  logg("Väljer år ...")
  
  # --- välj alla år utom 2014-2016 ---
  alla_ar <- hamta_select_varden(sess, "#AR")
  valda_ar <- setdiff(alla_ar, c("2014", "2015", "2016", "2017"))
  sess |> hitta("#AR") |> selenider::elem_select(value = valda_ar)
  
  # Om sidans onchange='antal_Ar()' inte triggas automatiskt (syns genom
  # att antalet valda år inte uppdateras på sidan), avkommentera:
  # kor_js(sess,
  #   "document.getElementById('AR').dispatchEvent(new Event('change'))")
  
  logg("Väljer månader samt inrikes/utrikes född ...")
  
  # --- välj alla månader ---
  klicka_via_id(sess, "#ph1_val_manad_hlAdd")
  
  # --- välj inrikes och utrikes född ---
  klicka_via_id(sess, "#ph1_val_utrikes_bist_hlAdd")
  
  # --- visa resultat ---
  logg("Klickar Visa resultat ...")
  klicka_via_id(sess, "#ph1_val_data_lnkVisaResultat")
  
  logg("Resultatsidan. Kontrollerar att År ligger i Rader ...")
  
  # --- resultatsidan: säkerställ att "År" finns i Rader-listan ---
  # Klicket på "Visa resultat" navigerar till resultatsidan - vänta in att
  # den laddats innan något läses av (körs stegen manuellt hinner sidan
  # ladda av sig själv, men i en sammanhängande körning gör den inte det).
  sess |> hitta("#ph1_ListBoxRader") |> selenider::elem_expect(is_visible, timeout = timeout)
  
  # "År" ligger i Kolumner-listan by default och behöver flyttas till Rader.
  # Kollen nedan gör flytten bara om den behövs - ett skydd ifall sidan
  # någon gång skulle ändra default-beteende.
  rader_options <- hamta_select_options(sess, "#ph1_ListBoxRader")
  
  if (!"År" %in% rader_options$text) {
    sess |> hitta("#ph1_ListBoxKolumner") |> selenider::elem_expect(is_visible, timeout = timeout)
    
    kolumn_options <- hamta_select_options(sess, "#ph1_ListBoxKolumner")
    ar_ix <- which(kolumn_options$text == "År")
    if (length(ar_ix) != 1) {
      stop(
        "Kunde inte entydigt hitta 'År' i #ph1_ListBoxKolumner. ",
        "Alternativen var: ", paste(kolumn_options$text, collapse = " | ")
      )
    }
    
    # Markera "År" UTAN att trigga change-eventet: sidans onchange-hanterare
    # är trasig ("ReferenceError: AR is not defined") och kraschar innan
    # markeringen hunnit registreras - vilket gjorde att elem_select() +
    # knappklick inte flyttade något. Utan change-event körs hanteraren
    # aldrig, och markeringen postas ändå med formuläret vid knapptrycket.
    valj_option_utan_event(sess, "#ph1_ListBoxKolumner", kolumn_options$value[[ar_ix]])
    klicka_via_id(sess, "#ph1_ButtonKolumnerTillRader")
    
    # Knappen triggar en ASP.NET-postback - hela sidan laddas om. Vänta in
    # att Rader-listan finns igen innan vi läser av den, annars körs
    # avläsningens JavaScript mitt i omladdningen och returnerar ingenting.
    sess |> hitta("#ph1_ListBoxRader") |> selenider::elem_expect(is_visible, timeout = timeout)
    
    rader_options <- hamta_select_options(sess, "#ph1_ListBoxRader")
  }
  
  if (!"År" %in% rader_options$text) {
    stop(
      "'År' hamnade inte i Rader-listan. Radernas innehall just nu: ",
      paste(rader_options$text, collapse = " | ")
    )
  }
  
  logg("Exporterar till Excel och väntar in nedladdningen ...")
  
  # --- exportera till Excel och vänta in nedladdningen ---
  sess |> hitta("#ph1_lbXLS") |> selenider::elem_expect(is_visible, timeout = timeout)
  
  fil <- hamta_nedladdning(
    sess,
    trigger = function() {
      klicka_via_id(sess, "#ph1_lbXLS")
    },
    nedladdningsmapp = tmpdir,
    monster = "\\.xlsx?$",
    timeout = timeout
  )
  
  logg(paste0("Klart! Fil: ", fil))
  
  # =========================== läs in filen som vi sparat i tmpdir med webbskrapningen ovan ===========================================
  sokvag_filnamn <- list.files(tmpdir, full.names = TRUE)
  inlasfil <- suppressMessages(read_xlsx(sokvag_filnamn, col_names = FALSE))
  
  # =========================== efterbearbetning av datasetet ========================================================================
  enhet_kol <- inlasfil[[1,1]]
  kol_namn <- inlasfil[2,] %>% as.character()
  
  inlasfil <- inlasfil %>%
    slice(3:nrow(.)) %>%
    setNames(kol_namn)
  
  dataset_slutrad <- which(is.na(inlasfil[["År"]]))[1] - 1
  
  suppress_specific_warning(
    inlasfil <- inlasfil %>%
      slice(1:dataset_slutrad) %>%
      pivot_longer(cols = c("Januari":"December"), names_to = "Månad", values_to = enhet_kol) %>%
      mutate({{ enhet_kol }} := na_if(str_replace_all(.data[[enhet_kol]], "--", NA_character_), NA_character_),
             {{ enhet_kol }} := .data[[enhet_kol]] %>% as.numeric()) %>%
      relocate({{ enhet_kol }}, .after = last_col())
  )
  
  sista_kol <- names(inlasfil)[ncol(inlasfil)]
  
  regionnyckel <- hamtaregtab() %>%
    rename(Regionkod = regionkod)
  
  manadsnyckel <- tibble(
    Månad = format(ISOdate(2000, 1:12, 1), "%B") %>% str_to_sentence(),
    Månad_num = c(1:12)
  )
  
  inlasfil <- inlasfil %>%
    left_join(regionnyckel, by = c("Region" = "region")) %>%
    relocate(Regionkod, .before = "Region") %>%
    left_join(manadsnyckel, by = "Månad") %>%
    relocate(Månad_num, .after = "Månad")
  
  # filtrera bort år-månader där samtliga värden är NA (kommande månader)
  inlasfil <- inlasfil %>%
    group_by(År, Månad) %>%
    filter(!all(across(all_of(sista_kol), is.na))) %>%
    ungroup()
  
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  return(inlasfil)
}
