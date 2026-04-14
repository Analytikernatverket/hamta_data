
library(tidyverse)
library(readxl)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)


hamta_lokstod_kommun_regso_kon_rf <- function() {
  lok_url <- "https://www.rf.se/forskning-och-statistik/statistik/lok-stod-per-omrade"
  
  region_nyckeltabell <- hamtaregtab()
  
  sokvagar_lokstod <- filhamtning_med_url_och_sokord(
    url_webbsida = lok_url, 
    sokord = c("lok", "xlsx"), 
    bas_url = "https://www.rf.se")
  
  lokstod_df <- map(sokvagar_lokstod, ~ {
    
    flikar <- excel_sheets(.x) %>% 
      .[!str_detect(., "Förklaring")]
    
    retur_fil <- map(flikar, function(flik) {
      retur_df <- readxl::read_excel(.x, sheet = flik, skip = 3, col_types = "text") %>% 
        pivot_longer(cols = c(Totalt, Flickor, Pojkar), names_to = "Kon", values_to = "Procent") %>%
        mutate(Procent = parse_number(na_if(Procent, "..")))
      
      beskrivning <- read_excel(.x, sheet = flik, range = "A1", col_names = "minimal") %>% dplyr::pull(1)
      alder_txt <- str_extract(beskrivning, "(?<=\\().*?(?=\\))")
      ar_txt <- str_extract(beskrivning, "(?<=år )\\d{4}")
    
      if (any(str_detect(tolower(names(retur_df)), "regso"))){
        regso_kol <- names(retur_df)[str_detect(names(retur_df), "RegSO") & str_detect(names(retur_df), "kod")]
        retur_df <- retur_df %>% 
          mutate(Kommunkod = str_sub(.[[regso_kol]], 6, 9),
                 Lanskod = str_sub(Kommunkod, 1, 2),
                 "{regso_kol}" := paste0(Kommunkod, "R", str_sub(.[[regso_kol]], 10, 12)),
                 Lan = region_nyckeltabell$region[match(Lanskod, region_nyckeltabell$regionkod)])
        kommun_nyckel <<- retur_df %>% 
          distinct(Kommunkod, Kommun)
        
      } else if (any(str_detect(tolower(names(retur_df)), "kommun"))) {
        retur_df <- retur_df %>% 
          mutate(Kommunkod = kommun_nyckel$Kommunkod[match(Kommun, kommun_nyckel$Kommun)],
                 Lanskod = str_sub(Kommunkod, 1, 2),
                 Lan = region_nyckeltabell$region[match(Lanskod, region_nyckeltabell$regionkod)])
      } # slut test för vilken typ av flik det är (regso eller kommun)
      
      retur_df <- retur_df %>% 
        mutate(Alder = alder_txt,
               Ar = ar_txt)
      return(retur_df)  
    }) %>% list_rbind() # slut map per flik
    
    retur_fil <- retur_fil %>% 
      mutate(Lanskod = if_else(Kommun == "Riket", "00", Lanskod),
             Lan = if_else(Lanskod == "00", "Riket", Lan),
             Kommunkod = if_else(Lanskod == "00", "00", Kommunkod)) %>% 
      select(any_of(c("Ar", "Alder", "RF-SISU distrikt", "Lanskod", "Lan", "Kommunkod", "Kommun", "RegSO-kod", "RegSO-namn", "Kon", "Procent")))
    
    return(retur_fil)
    
  }) %>% 
    list_rbind()
  
  return(lokstod_df)
}