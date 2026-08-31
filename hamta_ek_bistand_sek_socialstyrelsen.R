#' Hämta "Utbetalt ekonomiskt bistånd tkr" (månad) från Socialstyrelsen
#'
#'
#' @param headless Om webbläsaren ska köras headless. Python-originalet körde
#'   `headless = FALSE`, så det är default här också.
#' @param timeout Max antal sekunder att vänta på att element blir synliga.
#' @param verbose Om loggmeddelanden ska skrivas ut under körningen.
#'
#' @return En character-vektor med sökvägar till de nedladdade Excel-filerna
#'   (ett per varv som lyckades), namngiven med respektive
#'   #UTRIKES_HUSH-värde.
hamta_ek_bistand_sek_socialstyrelsen <- function(
    headless = TRUE,
    timeout = 30,
    verbose = TRUE
) {
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 selenider,
                 jsonlite)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_webbskrapning.R")
  hushall_bakgrund_varden <- 0:2
  tmpdir = tempfile("ekb_manad_")
  
  logg <- loggning_initiera(verbose = verbose)
  
  tmpdir <- normalizePath(tmpdir, winslash = "/", mustWork = FALSE)
  dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
  
  url <- "https://sdb.socialstyrelsen.se/if_ekb_manad/val.aspx"
  
  logg("Startar skrapsession ...")
  sess <- starta_skrapsession(headless = headless)
  on.exit(suppressMessages(stang_skrapsession(sess)), add = TRUE)
  
  # ========================================================================
  # ett varv = en export för ett värde i #UTRIKES_HUSH
  # motsvarar: async def export_one(page, tmpdir, i) i Python-originalet
  # ========================================================================
  exportera_ett_varv <- function(i) {
    
    logg(paste0("Varv (bakgrund i hushållet = ", i, "): öppnar webbsidan ..."))
    selenider::open_url(url, session = sess$session)
    
    # motsvarar: page.wait_for_selector("text=Dalarnas län", state="visible")
    # OBS: sidan har ~290 element med snarlik text (ett "Alla kommuner" per
    # län), så en xpath-baserad elem_expect() skulle bli mycket långsam här
    # (samma fälla som klicka_via_text()s dokumentation varnar för). Vi
    # pollar istället med EN JS-körning per försök via vanta_pa_js(), precis
    # som klicka_via_text() gör internt - snabbt och stabilt.
    vanta_pa_js(
      sess,
      sprintf("document.body.innerText.includes(%s)",
              jsonlite::toJSON("Dalarnas län", auto_unbox = TRUE)),
      timeout = timeout
    )
    
    # motsvarar: page.click("text=Alla kommuner") / page.click("text=Alla län")
    klicka_via_text(sess, "Alla kommuner")
    klicka_via_text(sess, "Alla län")
    
    logg("Väljer mått (Utbetalt ekonomiskt bistånd tkr) ...")
    sess |> hitta("#ph1_val_matt_pRad3") |> selenider::elem_expect(is_visible, timeout = timeout)
    #klicka_via_text(sess, "Utbetalt ekonomiskt bistånd tkr")
    sess |> hitta("#MATT") |> elem_select(text = "Utbetalt ekonomiskt bistånd tkr")
    
    logg("Väljer alla år ...")
    sess |> hitta("#ph1_val_ar_hlAdd") |> selenider::elem_expect(is_visible, timeout = timeout)
    klicka_via_id(sess, "#ph1_val_ar_hlAdd")
    
    logg("Väljer alla månader ...")
    sess |> hitta("#ph1_val_manad_hlAdd") |> selenider::elem_expect(is_visible, timeout = timeout)
    klicka_via_id(sess, "#ph1_val_manad_hlAdd")
    
    logg(paste0("Väljer bakgrund i hushållet = ", i, " ..."))
    sess |> hitta("#UTRIKES_HUSH") |> selenider::elem_expect(is_visible, timeout = timeout)
    # motsvarar: page.select_option("#UTRIKES_HUSH", str(i))
    sess |> hitta("#UTRIKES_HUSH") |> selenider::elem_select(value = as.character(i))
    
    logg("Klickar Visa resultat ...")
    sess |> hitta("#ph1_val_data_lnkVisaResultat") |> selenider::elem_expect(is_visible, timeout = timeout)
    klicka_via_id(sess, "#ph1_val_data_lnkVisaResultat")
    
    logg("Flyttar 'År' från Kolumner till Rader ...")
    sess |> hitta("#ph1_ListBoxKolumner") |> selenider::elem_expect(is_visible, timeout = timeout)
    # motsvarar: page.select_option("#ph1_ListBoxKolumner", value="AR")
    sess |> hitta("#ph1_ListBoxKolumner") |> selenider::elem_select(text = "År")
    klicka_via_id(sess, "#ph1_ButtonKolumnerTillRader")
    
    logg("Exporterar till Excel och väntar in nedladdningen ...")
    sess |> hitta("#ph1_lbXLS") |> selenider::elem_expect(is_visible, timeout = timeout)
    
    # motsvarar: async with page.expect_download(): await page.click("#ph1_lbXLS")
    fil <- hamta_nedladdning(
      sess,
      trigger = function() klicka_via_id(sess, "#ph1_lbXLS"),
      nedladdningsmapp = tmpdir,
      monster = "\\.xlsx?$",
      timeout = timeout
    )
    
    logg(paste0("Klart! Fil: ", fil))
    fil
  }
  
  # ========================================================================
  # kör ett varv per värde i hushall_bakgrund_varden
  # motsvarar: for i in range(3): try: ... except Exception as e: print(...)
  # purrr::safely() fångar felet per varv utan att avbryta övriga varv,
  # och loggmeddelandet nedan motsvarar Pythons "Fel i varv {i+1}: {e}"
  # ========================================================================
  resultat <- hushall_bakgrund_varden |>
    purrr::set_names() |>
    purrr::map(~ purrr::safely(exportera_ett_varv)(.x))
  
  sparade <- resultat |>
    purrr::imap(function(r, i) {
      if (!is.null(r$error)) {
        logg(paste0("Fel i varv (bakgrund i hushållet = ", i, "): ", conditionMessage(r$error)))
        return(NULL)
      }
      r$result
    }) |>
    purrr::compact() |>
    purrr::list_c()
  
  # ====================== efterbearbetning av data =================================================
  
  #sokvag_filnamn <- list.files(tmpdir, full.names = TRUE)
  #inlasfil <- suppressMessages(read_xlsx(sokvag_filnamn, col_names = FALSE))
  retur_df <- map(sparade, ~ {
    suppressMessages(inlasfil <- read_xlsx(.x, col_names = FALSE))
    
    enhet_kol <- inlasfil[[1,1]]
    bakgrund_varde <- str_extract(enhet_kol, "[^,]*$") %>% str_trim()
    enhet_kol <- str_remove(enhet_kol, ",[^,]*$")
    
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
               {{ enhet_kol }} := .data[[enhet_kol]] %>% as.numeric(),
               Bakgrund = bakgrund_varde) %>%
        relocate(Bakgrund, .after = Region) %>%
        relocate({{ enhet_kol }}, .after = last_col())
    )
  }) %>% list_rbind()
  
  sista_kol <- names(retur_df)[ncol(retur_df)]
  
  regionnyckel <- hamtaregtab() %>%
    rename(Regionkod = regionkod)
  
  manadsnyckel <- tibble(
    Månad = format(ISOdate(2000, 1:12, 1), "%B") %>% str_to_sentence(),
    Månad_num = c(1:12)
  )
  
  retur_df <- retur_df %>%
    left_join(regionnyckel, by = c("Region" = "region")) %>%
    relocate(Regionkod, .before = "Region") %>%
    left_join(manadsnyckel, by = "Månad") %>%
    relocate(Månad_num, .after = "Månad")
  
  # filtrera bort år-månader där samtliga värden är NA (kommande månader)
  retur_df <- retur_df %>%
    group_by(År, Månad) %>%
    filter(!all(across(all_of(sista_kol), is.na))) %>%
    ungroup()
  
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  return(retur_df)
  
}
