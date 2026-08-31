library(keyring)
library(selenider)
library(purrr)
library(httr)
library(dplyr)
library(readxl)
library(stringr)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_webbskrapning.R")

#' Hämta servicetabell från PIPOS/serviceanalys (tillvaxtverket.se)
#'
#' Loggar in på serviceanalys med sparade keyring-uppgifter (service = "pipos"),
#' navigerar till Tabeller och statistik -> Servicetabell, laddar ner den som
#' Excel-fil och sparar den i `nedladdningsmapp`.
#'
#' @param nedladdningsmapp Mapp dit Excel-filen ska sparas. Skapas om den
#'   inte finns. Default en unik temp-mapp.
#' @param headless Om Edge ska köras utan synligt fönster. Default FALSE,
#'   samma som pythonskriptet (`headless=False`) - praktiskt vid inloggning
#'   så man kan se om något fastnar, t.ex. en captcha eller extra dialog.
#'
#' @return Sökvägen till den nedladdade Excel-filen.
hamta_lok_per_forening <- function(nedladdningsmapp = tempfile("lok-"),
                                      headless = TRUE) {
  
  rf_url <- "https://www.rf.se/bidrag-och-stod/bidrag/historik-och-statistik-stod"
  

  skrap <- starta_skrapsession(headless = headless)
  on.exit(stang_skrapsession(skrap), add = TRUE)
  
  selenider::open_url(rf_url, session = skrap$session)
  

#   kor_js(skrap, "
#   var sel = document.querySelector(\"select[name='categories']\");
#   sel.value = '19.4665efe718412b3921e1d45';
#   sel.dispatchEvent(new Event('change', { bubbles: true }));
#   sel.dispatchEvent(new Event('input', { bubbles: true }));
# ")
  
  valj_i_lista(skrap, "select[name='categories']", "19.4665efe718412b3921e1d45")
  
  klicka_via_klass_och_text(skrap, "hiq-file-list__filters__button", "Sök", tag = "button")
  
  lankar <- kor_js(skrap, "JSON.stringify([...document.querySelectorAll('a')]
  .filter(a => a.textContent.trim().startsWith('Utbetalt'))
  .map(a => ({text: a.textContent.trim(), href: a.href})))") |>
    jsonlite::fromJSON()
  
  # ladda ner och bearbeta data
  
  # hjälpfunktioner
  hitta_rubrikrad <- function(sokvag, max_rader = 40) {
    forsta_kolumnen <- read_excel(sokvag, col_names = FALSE, n_max = max_rader,
                                  .name_repair = "minimal")[[1]]
    
    rad <- which(forsta_kolumnen == "Föreningsnr.")
    if (length(rad) == 0) {
      stop("Hittade ingen rubrikrad (\"Föreningsnr.\") inom de första ", max_rader,
           " raderna i: ", sokvag)
    }
    rad[1]
  }
  
  kanonisera_kolumnnamn <- function(data) {
    # äldre/alternativa namn -> kanoniskt namn. Slås bara ihop om det
    # kanoniska namnet inte redan finns i just den här filen (varje enskild
    # år-fil har bara en av varianterna, så det är en ren namnbytesoperation
    # per fil - de "möts" först när list_rbind() slår ihop åren).
    mappning <- c(
      "Extra ledarstöd"                        = "Extra Ledarstöd",
      "Deltagartillf. FH"                       = "Deltagartillf. FN",
      "Varav deltagartillf. Funktionsnedsättn"  = "Deltagartillf. FN",
      "P26_"                                    = "P26-",
      "F21-"                                    = "F21-25",
      "P21-"                                    = "P21-25"
    )
    
    for (gammalt in names(mappning)) {
      nytt <- mappning[[gammalt]]
      if (gammalt %in% names(data) && !(nytt %in% names(data))) {
        data <- dplyr::rename(data, !!nytt := dplyr::all_of(gammalt))
      }
    }
    data
  }
  
  lasa_lok_fil <- function(sokvag) {
    rubrikrad <- hitta_rubrikrad(sokvag)
    
    data <- read_excel(sokvag, skip = rubrikrad - 1, .name_repair = "unique_quiet") |>
      dplyr::rename(foreningsnr = `Föreningsnr.`, idrott = Idrott) |>
      kanonisera_kolumnnamn() |>
      dplyr::select(-dplyr::matches("^Led\\.\\s[KM]")) 
    
    # ta bort genererade ...N-kolumner (namnlösa i originalfilen) som är
    # helt tomma - gör det utanför en pipe eftersom predikatet behöver
    # kolumnnamnet, vilket cur_column() bara ger inuti across()
    tomma_genererade <- names(data)[
      grepl("^\\.\\.\\.\\d+$", names(data)) &
        purrr::map_lgl(data, ~ all(is.na(.)))
    ]
    data <- dplyr::select(data, -dplyr::all_of(tomma_genererade))
    
    # summeringsrad = klubbens totalsumma (ej nedbrytning per idrott/period).
    # Markeras olika mellan år: antingen foreningsnr == "Totalt", eller
    # idrott == "Föreningen"/"Total".
    data |>
      dplyr::mutate(
        ar_summeringsrad = foreningsnr == "Totalt" | idrott %in% c("Föreningen", "Total"),
        .after = idrott
      )
  }
  
  dir.create(nedladdningsmapp, showWarnings = FALSE, recursive = TRUE)
  
  lok_data <- lankar |>
    purrr::pmap(function(text, href) {
      ext <- tools::file_ext(href)
      filnamn <- file.path(nedladdningsmapp, paste0(text, ".", ext))
      
      # ladda ner
      svar <- httr::GET(href, httr::write_disk(filnamn, overwrite = TRUE))
      httr::stop_for_status(svar, task = paste("ladda ner", text))
      
      # läs in
      lasa_lok_fil(filnamn) |>
        dplyr::mutate(kalla_ar = stringr::str_extract(text, "\\d{4}"), .before = 1)
    }, .progress = TRUE) |>
    purrr::list_rbind()
  
  lok_data
}
