
hamta_bygdegardar <- function(returnera_sf = TRUE) {
  # ------------------------------------------------------------------------------
  # Skript för att hämta och (om man vill) skapa sf-objekt över bygdegårdar i 
  # Sverige, data hämtas från https://bygdegardarna.se/.
  #
  # Funktionen nvänder följande paket:
  # - pacman     : installerar de paket som behövs i funktionen
  # - tidyverse  : för datahantering och textbearbetning
  # - httr       : för att hämta webbinnehåll (HTTP-anrop)
  # - rvest      : för att läsa och extrahera data ur HTML
  # - jsonlite   : för att konvertera JSON till R-objekt
  # - V8         : för att köra JavaScript-kod och tolka inbäddade JS-arrayer
  # - sf         : för att skapa och hantera geografiska objekt
  #
  # Skriptet hämtar data om bygdegårdar från Bygdegårdarnas Riksförbunds webbplats,
  # tolkar kartdata (JavaScript), omvandlar till geografiska koordinater,
  # kopplar till kommun- och länsinformation och skapar ett sf-objekt med
  # alla bygdegårdar i Sverige. Sätt returnera_sf till FALSE för att returnera
  # en dataframe istället för sf-objekt.
  #
  # Skriptet är skrivet av Peter Möller, Region Dalarna och används under eget
  # ansvar. Förbättringar eller förslag på förbättringar görs via Analytiker-
  # nätverkets Github här: https://github.com/Analytikernatverket/hamta_data
  #
  # ------------------------------------------------------------------------------
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         httr,
         rvest,
         jsonlite,
         V8)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  # hämta kommunpolygoner för att bestämma komm
  kommuner_sf <- hamta_karta("kommuner_lm") %>% 
    rename(kommun = kommunnamn)
  
  lansnyckel <- hamtaregtab() %>% 
    filter(nchar(regionkod) == 2 & regionkod != "00") %>% 
    rename(lanskod = regionkod, 
           lan = region)
  
  kommunnyckel <- hamtaregtab() %>% 
    filter(nchar(regionkod) == 4) %>% 
    rename(kommunkod = regionkod, 
           kommun = region)
  
  bygdeg_url <- "https://bygdegardarna.se/hitta-bygdegard/?namn=&distrikt=&kommun=&storlek=&sok=S%C3%96K"
  
  res <- GET(bygdeg_url)
  
  res_content <- httr::content(res, as = "text", encoding = "UTF-8")
  
  html_obj <- read_html(res_content)
  
  # 2. Extrahera alla script-taggar som text
  scripts <- html_obj %>% html_elements("script") %>% html_text()
  
  # 3. Hitta den som innehåller markerData
  script_data <- scripts[str_detect(scripts, "var markerData")] [1]
  
  # 4. Plocka ut själva JS-arrayen
  js_array <- str_extract(script_data, "(?s)markerData\\s*=\\s*\\[.*?\\]")
  
  # 5. Rensa bort "markerData =" och semikolon
  js_array_clean <- js_array %>%
    str_replace("markerData\\s*=\\s*", "") %>%
    str_replace(";$", "")
  
  ctx <- v8()
  
  # Ladda in JS-arrayen
  ctx$eval(paste0("var markerData = ", js_array_clean, ";"))
  
  # Konvertera JS-objektet till JSON-string i JS-engine
  ctx$eval("var jsonMarkerData = JSON.stringify(markerData);")
  
  # Hämta JSON som text
  json_data <- ctx$get("jsonMarkerData")
  
  # Konvertera JSON → R-objekt
  marker_data <- fromJSON(json_data)
  
  # konvertera HTML-entity till vanlig text
  marker_data$title <- sapply(marker_data$title, function(x) {
    xml2::xml_text(xml2::read_html(paste0("<x>", x, "</x>")))
  })
  
  
  bygdegardar_sf <- marker_data %>% 
    mutate(lat = position$lat,
           lng = position$lng,
           adress = meta$address,
           postnummer = meta$postalCode,
           postort = meta$city,
           telefonnummer = meta$phone,
           url = meta$permalink,
           epost = meta$email, 
           ) %>%
    st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
    st_transform(3006) %>% 
    select(namn = title, adress, postnummer, postort, telefonnummer,
           url, epost, geometry) %>% 
    st_join(
      kommuner_sf[, c("kommunkod", "kommun")], # välj bara relevanta kolumner
      left = TRUE,
      join = st_within
    ) %>% 
    mutate(lanskod = str_sub(kommunkod, 1, 2)) %>% 
    left_join(lansnyckel, by = "lanskod")
  
  tabell_utan_geo <- html_obj %>% 
    html_element("table") %>% 
    html_table() %>% 
    rename(namn = Bygdegård, kommun = Kommun)
  
  har_geo <- bygdegardar_sf %>% 
    left_join(tabell_utan_geo, by = c("namn", "kommun"))
  
  saknar_geo <- tabell_utan_geo %>% 
    anti_join(bygdegardar_sf, by = c("namn", "kommun"))   
  
  saknas_i_karta <- bygdegardar_sf %>% anti_join(tabell_utan_geo, by = c("namn", "kommun"))
  saknar_geo <- saknar_geo[!saknar_geo$namn %in% saknas_i_karta$namn,] %>% 
    left_join(kommunnyckel, by = "kommun") %>% 
    mutate(lanskod = str_sub(kommunkod, 1, 2)) %>% 
    left_join(lansnyckel, by = "lanskod")
  
  bygdegardar_sf <- bind_rows(har_geo, saknar_geo)

  if (!returnera_sf) bygdegardar_sf <- bygdegardar_sf %>% st_drop_geometry()
  return(bygdegardar_sf) 
  
}