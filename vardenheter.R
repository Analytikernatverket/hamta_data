# =====================================================================================================================
#
# Skript för att hämta vårdutubdspunkter som geodata. Informationen hämtas från 1177.se under avsnittet "Hitta vård".
# Då informationen endast kan extraheras på paginerade webbsidor så har det byggts in en begränsning att bara kunna 
# hämta en region i taget. Det är ok att hämta information på det här sättet men vi försöker att inte belasta 1177.se:s
# servrar mer än nödvändigt. Förhoppningen är att det här skriptet snart ska kunna ersättas av ett offentlig öppet API.
# Möjligen går det att hämta data redan nu via HSA, det bör undersökas framåt. Men tills dess, använd detta skript med 
# måttlighet, hämta helst bara en region eller möjligten grannregionerna, och behövs hela Sverige rekommenderas att det
# läggs in en fördröjning mellan varje region som hämtas. Man kan också begränsa antalet anrop med att begränsa vilka
# vårdenheter som ska hämtas, tex. "Vårdcentraler".
#
# Skapat av: Peter Möller, Region Dalarna, i februari 2026.
# Reviderat:
#
# =====================================================================================================================

library(rvest)
library(stringr)
library(purrr)
library(dplyr)
library(sf)
library(jsonlite)

vardenheter_vardtyper_lista <- function() {
  # url till hitta vård-sidan på 1177.se
  url_1177 <- "https://www.1177.se/hitta-vard/?st=208656b8-211b-4df9-ba72-912c7e02fca7&nearby=false&s=&g=&lat=&lng=&location=&caretype=&q=&openN=&openE=&openW=&openTN=&openTE=&openTW=&ow=&es=&l=&lq="
  
  html_grund <- read_html(url_1177)
  scripts <- html_elements(html_grund, "script")  %>%     # Hämta alla script-element
    html_text()
  geo_script <- scripts[str_detect(scripts, "__PRELOADED_STATE__")]
  json_text <- str_extract(geo_script, "\\{.*\\}")                              # Plocka ut JSON-delen 
  geo_data <- fromJSON(json_text)                                               # Konvertera JSON-texten till en R-lista
  
  vardtyper_df <- geo_data$`__PRELOADED_STATE__`$Content$AOCareTypes$Categories %>% 
    unlist() %>% 
    unique() %>% 
    as.data.frame() %>% 
    setNames("vardtyp")
  return(vardtyper_df)
}

vardenheter_hamta <- function(
    regionkod,                            # bara tillåtet att hämta en region i taget, t.ex. "20" för Dalarna
    vardtyp = "Alla+mottagningar"         # förvalt värde "Alla+mottagningar" som hämtar alla vårdtyper, alternativt kan man välja
                                          # en specifik vårdtyp från listan som vardenheter_vardtyper_lista() hämtar, t.ex. "Vårdcentraler"
    ) {
  
  if (length(regionkod) > 1) {
    regionkod <- regionkod[1]
    cat("Endast en regionkod kan skickas till funktionen och därför kommer enbart den första regionkoden att användas:", regionkod, "\n")
  }
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

  # url till hitta vård-sidan på 1177.se
  url_1177 <- "https://www.1177.se/hitta-vard/?st=208656b8-211b-4df9-ba72-912c7e02fca7&nearby=false&s=&g=&lat=&lng=&location=&caretype=&q=&openN=&openE=&openW=&openTN=&openTE=&openTW=&ow=&es=&l=&lq="
  url_1177 <- str_replace(url_1177, "caretype=", paste0("caretype=", vardtyp))
  
  html_grund <- read_html(url_1177)
  scripts <- html_elements(html_grund, "script")  %>%     # Hämta alla script-element
    html_text()
  geo_script <- scripts[str_detect(scripts, "__PRELOADED_STATE__")]
  json_text <- str_extract(geo_script, "\\{.*\\}")                              # Plocka ut JSON-delen 
  geo_data <- fromJSON(json_text)                                               # Konvertera JSON-texten till en R-lista
  
  regioner_df <- geo_data$`__PRELOADED_STATE__`$Regionalization$Regions$Options
    
  reg_nyckel <- hamtaregion_kod_namn(hamtaAllaLan())
  
  regioner_df <- regioner_df %>% 
    left_join(reg_nyckel %>% select(regionkod, soknamn_region = region), by = c("Value" = "regionkod"))
  
  region_sok <- regioner_df %>%
    filter(Value == regionkod) %>% 
    dplyr::pull(soknamn_region)
  
  # läs in hitta vård-sidan på 1177.se
  url_region <- url_1177 %>% str_replace("location=", paste0("location=", region_sok)) %>% URLencode()
  html_1177 <- read_html(url_region)
  raw_text <- rvest::html_text(rvest::html_element(html_1177, "body"))
  total_hits <- as.integer(regmatches(raw_text, regexpr('(?<="TotalHits":)\\d+', raw_text, perl = TRUE)))
  n_pages <- ceiling(total_hits / 10)
  
  vardenheter_df <- map(1:n_pages, ~ {
    if (.x > 1) {
      url_pag <- str_c(url_region, "&p=", .x)
      
    } else url_pag <- url_region
    
    # läs in paginerad sida
    html <- read_html(url_pag)
    
    # Hämta alla script-element
    scripts <- html_elements(html, "script")  %>%  
      html_text()
    
    # Hitta script som innehåller latitude
    geo_script <- scripts[str_detect(scripts, "__PRELOADED_STATE__")]
    
    # Plocka ut JSON-delen 
    json_text <- str_extract(geo_script, "\\{.*\\}")
    
    # Konvertera JSON-texten till en R-lista
    geo_data <- fromJSON(json_text)
    
    # Plocka ut relevant data
    rader_df <- geo_data$`__PRELOADED_STATE__`$
      Content$
      SearchResult$
      SearchHits
    
    return(rader_df)
    
  }, .progress = TRUE) %>% 
    list_rbind()
  
  # Hitta grupper där en rad är en delsträng av en annan
  df <- vardenheter_df  %>% mutate(row_id = row_number(), match_group = 0L)
  
  headings <- df$Heading
  
  for (i in seq_along(headings)) {
    # Hitta alla rader där heading[i] är en delsträng (men inte identisk)
    children <- which(
      str_detect(headings, regex(paste0("\\b", str_replace_all(fixed(headings[i]), "([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1"), "\\b"), ignore_case = TRUE)) & 
        headings != headings[i] &
        nchar(headings) > nchar(headings[i])
    )
    
    if (length(children) > 0 && any(df$IsCompareableOn1177[c(i, children)]) && all(sapply(children, function(j) {
      parent_words <- str_to_lower(str_split(str_squish(str_remove_all(headings[i], regex("mottagning|,", ignore_case = TRUE))), "[\\s-]+")[[1]])
      child_words  <- str_to_lower(str_split(str_squish(str_remove_all(headings[j], regex("mottagning|,", ignore_case = TRUE))), "[\\s-]+")[[1]])
      str_detect(headings[j], regex("\\bmottagning\\b", ignore_case = TRUE)) && all(child_words %in% parent_words)
    }))) {
  
      group_id <- i
      df$match_group[i]        <- group_id      # förälder: bara siffra
      df$match_group[children] <- -group_id     # barn: negativ = får bokstav
    }
  }
  
  # Omvandla till läsbart format: 0, "1", "1a", "1b", ...
  letters_vec <- c(letters, paste0(letters, letters))  # a-z, aa-bb...
  
  df <- df %>%
    group_by(match_group) %>%
    mutate(
      plot_group = case_when(
        match_group == 0  ~ "0",
        match_group > 0   ~ as.character(match_group),  # förälder: siffra
        match_group < 0   ~ paste0(abs(match_group), letters_vec[row_number()])  # barn: siffra+bokstav
      )
    ) %>% 
    ungroup()  %>% 
    select(-row_id, -match_group)
  
  vard_med_kategori_df <- df %>% 
    mutate(kategori = if_else(plot_group == 0 & IsCompareableOn1177, "Vårdcentral", NA),
           kategori = if_else(plot_group != 0 & str_detect(plot_group, "[a-z]"), "Vårdcentral", kategori),
           kategori = if_else(str_detect(tolower(Heading), "akutmottagning|närakut") & !str_detect(tolower(Heading), "psykiatrisk"), "Akutmottagning", kategori),
           kategori = if_else(str_detect(tolower(Heading), "psykiatrisk akutmottagning"), "Psykiatrisk akutmottagning", kategori),
           kategori = if_else(str_detect(tolower(Heading), "barnavårdscentral"), "Barnavårdscentral", kategori),
           kategori = if_else(str_detect(tolower(Heading), "familjecentral"), "Familjecentral", kategori),
           kategori = if_else(str_detect(tolower(Heading), "tand"), "Tandvård", kategori)
           )
  
  
  # konvertera till sf-objekt
  vardenheter_sf <- vard_med_kategori_df %>% 
    filter(!is.na(Longitude)) %>% 
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(crs = 3006)
  
  tidstagning <- Sys.time() - starttid
  message(sprintf("Processen skapa graf tog %s minuter att köra", Sys.time() - starttid %>% round(., 1)))
  
  return(vardenheter_sf)
} # slut funktion

