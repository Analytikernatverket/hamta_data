

hamta_laddstationer_nobil <- function(returnera_sf = TRUE) {
  # ------------------------------------------------------------------------------
  # Hämtar laddstationsdata från NOBIL API (Sverige)
  #
  # Funktionen `hamta_laddstationer_nobil()` laddar ned data om publika 
  # laddstationer i Sverige från NOBILs öppna API och returnerar resultatet 
  # som ett sf-objekt i SWEREF 99 TM (EPSG:3006).
  #
  # För att kunna använda funktionen krävs att du:
  # 1. Har ett kostnadsfritt API från NOBIL (ansök via https://info.nobil.no/api).
  # 2. Har sparat din API-nyckel i keyring under en service med namnet 
  #    "nobil_laddstationer". Exempel:
  #       keyring::key_set_with_value(
  #         service = "nobil_laddstationer",
  #         username = "<ditt användarnamn>",
  #         password = "<din API-nyckel>"
  #       )
  #
  # Funktionen kontrollerar automatiskt att:
  # - Paketet `keyring` är installerat.
  # - En giltig service "nobil_laddstationer" finns i keyringen.
  #
  # Om något saknas avbryts körningen med ett informativt felmeddelande.
  #
  # Skriptet använder `httr` för API-anrop, `jsonlite` för att läsa in JSON-data 
  # och `sf` för att skapa spatiala objekt.
  #
  # Skriptet är skrivet av Peter Möller, Region Dalarna och används under eget
  # ansvar. Förbättringar eller förslag på förbättringar görs via Analytiker-
  # nätverkets Github här: https://github.com/Analytikernatverket/hamta_data
  #
  # ------------------------------------------------------------------------------
  
  
  # Kontrollera att keyring är installerat ------------------------------
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("Paketet 'keyring' är inte installerat. Installera det med install.packages('keyring').")
  }
  
  # Kontrollera att tjänsten 'nobil_laddstationer' finns ----------------
  services <- keyring::key_list()
  
  if (!"nobil_laddstationer" %in% services$service) {
    stop("Ingen tjänst med namnet 'nobil_laddstationer' hittades i keyring. Du måste ha ett API från Nobil som du kan få kostnadsfritt här: https://info.nobil.no/api. Lägg in användare och API-nyckel som service 'nobil_laddstationer' i keyring-paketet så kommer detta skript att fungera.")
  }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(httr,
         jsonlite,
         keyring)
  
  laddst_sv <- GET(paste0("https://nobil.no/api/server/datadump.php?apikey=", 
                          key_get("nobil_laddstationer", key_list(service = "nobil_laddstationer")$username), 
                          "&countrycode=SWE&fromdate=2012-06-02&format=json&file=false"))
  laddst_sv_resp <- fromJSON(httr::content(laddst_sv, as = "text"), flatten = FALSE)
  laddst_sv_df <- laddst_sv_resp$chargerstations$csmd
  
  #ta bort parenteser
  
  laddst_sv_df$Position <- gsub("[()]", "", as.character(laddst_sv_df$Position))
  
  #splitta kolumnen position
  laddstolpar_sf <- laddst_sv_df %>% separate_wider_delim(Position, ",", names = c("lat", "lon")) #WGS84 Decimal (lat, lon)
  
  laddstolpar_sf = st_as_sf(laddstolpar_sf, coords = c("lon", "lat"), 
                            crs = 4326, agr = "constant") %>% 
    st_transform(crs = 3006)
  
  return(laddstolpar_sf)
  
}
