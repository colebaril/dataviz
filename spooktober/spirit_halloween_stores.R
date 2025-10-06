require(pacman)
p_load(tidyverse, rvest, tidygeocoder, stringdist)

# U.S. state codes
us_states <- c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "fl", "ga", "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me", "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", "wy")

# Canadian province codes
ca_provinces <- c("ab", "bc", "mb", "nb", "nl", "ns", "nt", "nu", "on", "pe", "qc", "sk", "yt")

regions <- c(us_states, ca_provinces)

# Function to extract city codes for a given state/province
get_city_codes <- function(region) {
  url <- paste0("https://stores.spirithalloween.com/", region, "/")
  page <- read_html(url)
  
  city_codes <- page |>
    html_elements(".is-single .gaq-link") |>
    html_attr("href") |>
    str_remove_all("^/|/$") |>
    str_split("/") |>
    sapply(function(x) x[length(x)]) |>
    unique()
   
  return(city_codes)
}

# Apply function to all regions
city_codes_list <- lapply(regions, get_city_codes)
names(city_codes_list) <- regions

# Function to extract store information for a given city
get_stores <- function(region, city) {
  message("Scraping region: ", region, " | city: ", city)  # <-- progress indicator
  
  url <- paste0("https://stores.spirithalloween.com/", region, "/", city, "/")
  page <- read_html(url)
  
  stores <- page |>
    html_elements(".map-list-item-wrap") |>
    html_text2() |>
    str_squish()
  
  if(length(stores) == 0) {
    return(data.frame(
      region = region,
      city = city,
      store = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(
    region = region,
    city = city,
    store = stores,
    stringsAsFactors = FALSE
  )
}


# Scrape stores for each city in each region
all_stores <- bind_rows(
  mapply(function(region, cities) {
    do.call(rbind, lapply(cities, function(city) get_stores(region, city)))
  }, names(city_codes_list), city_codes_list, SIMPLIFY = FALSE)
)

all_stores |> 
  write.csv("spirit_halloween_stores.csv", row.names = FALSE)

# Use city name and state/province to retrieve approx lat / lon coordinates 

spirit_halloween <- read_csv(here("spooktober/Data/spirit_halloween_stores.csv")) |> 
  mutate(region = str_to_upper(region),
         city = str_to_title(city)) |> 
  summarise(n = n(),
            .by = c("region", "city"))

geo_results <- spirit_halloween |>
  geocode(city = city, state = region, method = "osm")

geo_results_fail <- geo_results |> 
  filter(is.na(lat)) |> 
  write.csv("failed_cities.csv")

# Interestingly, when scraping the website, some city names are incorrectly spelled.
# I retrieved a list of correct names online, then used stringdist to match the
# names with errors to the list of correct names. 

# Clean up city names ----

## Clean Cities List ----
clean_cities <- c(
  "Pell City", "Spanish Fort", "Apache Junction", "Bullhead City", "Casa Grande",
  "Green Valley", "Oro Valley", "Show Low", "Sierra Vista", "Fort Smith",
  "Little Rock", "North Little Rock", "Apple Valley", "Bakersfield", "Baldwin Park",
  "Buena Park", "Cathedral City", "Chino Hills", "Chula Vista", "Citrus Heights",
  "Costa Mesa", "Culver City", "Daly City", "El Cajon", "El Centro",
  "El Cerrito", "Elk Grove", "Fountain Valley", "Jurupa Valley", "La Habra",
  "La Mesa", "La Verne", "La Dera Ranch", "Lake Elsinore", "Long Beach",
  "Los Angeles", "Marina del Rey", "Mission Viejo", "Moreno Valley", "Morgan Hill",
  "National City", "Paso Robles", "Pico Rivera", "Pleasant Hill", "Rancho Cucamonga",
  "Redwood City", "Rohnert Park", "Rolling Hills Estates", "San Bernardino", "San Clemente",
  "San Jose", "San Luis Obispo", "San Mateo", "San Rafael", "San Ramon",
  "Sand City", "Santa Ana", "Santa Clarita", "Santa Cruz", "Santa Maria",
  "Santa Rosa", "Simi Valley", "Thousand Oaks", "West Covina", "Woodland Hills",
  "Yuba City", "Colorado Springs", "Fort Collins", "Glenwood Springs", "Grand Junction",
  "Altamonte Springs", "Avon Park", "Boca Raton", "Boynton Beach", "Coral Springs",
  "Dade City", "Daytona Beach", "Delray Beach", "Fort Myers", "Fort Walton Beach",
  "Hallandale Beach", "Jacksonville Beach", "Jensen Beach", "Lake City", "Lake Mary",
  "Merritt Island", "North Fort Myers", "Oakland Park", "Orange City", "Orange Park",
  "Palm Beach Gardens", "Palm Harbor", "Panama City", "Panama City Beach", "Pembroke Pines",
  "Plant City", "Port Charlotte", "Port Richey", "Port St. Lucie", "Royal Palm Beach",
  "South Miami", "Spring Hill", "St. Augustine", "St. Cloud", "St. Petersburg",
  "Vero Beach", "West Melbourne", "West Palm Beach", "Winter Garden", "Winter Haven",
  "Warner Robins", "Coeur d'Alene", "Idaho Falls", "Twin Falls", "Arlington Heights",
  "Crystal Lake", "Des Plaines", "Downers Grove", "East Peoria", "Fairview Heights",
  "Granite City", "Loves Park", "Melrose Park", "Orland Park", "Round Lake Beach",
  "South Barrington", "Vernon Hills", "Fort Wayne", "Michigan City", "South Bend",
  "Terre Haute", "Cedar Rapids", "Council Bluffs", "Des Moines", "Iowa City",
  "Sioux City", "West Des Moines", "Garden City", "Bowling Green", "Baton Rouge",
  "Bossier City", "Lake Charles", "New Orleans", "West Monroe", "South Portland",
  "Glen Burnie", "Ocean City", "Owings Mills", "Prince Frederick", "Severna Park",
  "Silver Spring", "East Walpole", "North Attleborough", "North Billerica", "Ann Arbor",
  "Battle Creek", "Bay City", "Benton Harbor", "Big Rapids", "Farmington Hills",
  "Fort Gratiot", "Grand Rapids", "Madison Heights", "Rochester Hills", "Traverse City",
  "Apple Valley", "Brooklyn Park", "Eden Prairie", "Elk River", "Maple Grove",
  "North Branch", "St. Cloud", "Ocean Springs", "Cape Girardeau", "Crystal City",
  "Des Peres", "Jefferson City", "Kansas City", "Lake Saint Louis", "Lee's Summit",
  "Saint Joseph", "Saint Peters", "St. Louis", "Great Falls", "Grand Island",
  "Carson City", "Las Vegas", "North Las Vegas", "West Lebanon", "Barnegat Township",
  "Cherry Hill", "Cherry Hill Township", "East Brunswick", "Egg Harbor Township", "Lawrence Township",
  "May's Landing", "Mt. Laurel", "North Brunswick", "Old Bridge", "Parsippany-Troy Hills",
  "Point Pleasant", "Rio Grande", "Tinton Falls", "Toms River", "Woodbridge Township",
  "Woodland Park", "Las Cruces", "Santa Fe", "Clifton Park", "College Point",
  "East Meadow", "Huntington Station", "Johnson City", "New Hartford", "New York",
  "Niagara Falls", "North Babylon", "North Syracuse", "Orchard Park", "Saratoga Springs",
  "Staten Island", "The Bronx", "Valley Stream", "White Plains", "Yorktown Heights",
  "Fuquay Varina", "New Bern", "Winston Salem", "Grand Forks", "Middleburg Heights",
  "New Philadelphia", "North Canton", "North Olmsted", "Rocky River", "Saint Clairsville",
  "Upper Arlington", "Del City", "Midwest City", "Oklahoma City", "Grants Pass",
  "Bethel Park", "Camp Hill", "Cranberry Twp", "Dickson City", "Fairless Hills",
  "Glen Mills", "New Castle", "North Wales", "Plymouth Meeting", "State College",
  "West Mifflin", "Wilkes-Barre Township", "Willow Grove", "North Kingstown", "Myrtle Beach",
  "North Charleston", "Rock Hill", "Rapid City", "Sioux Falls", "Johnson City",
  "Caddo Mills", "Cedar Hill", "Cedar Park", "College Station", "Copperas Cove",
  "Corpus Christi", "Eagle Pass", "Fort Worth", "Grand Prairie", "Highland Village",
  "Lake Jackson", "Lake Worth", "New Braunfels", "Oak Ridge North", "Port Arthur",
  "Round Rock", "San Angelo", "San Marcos", "Texas City", "Universal City",
  "Wichita Falls", "American Fork", "Salt Lake City", "Spanish Fork", "West Jordan",
  "West Valley City", "South Burlington", "Cave Spring", "Colonial Heights", "Falls Church",
  "Newport News", "Seven Corners", "Virginia Beach", "East Wenatchee", "Federal Way",
  "Moses Lake", "Port Angeles", "Spokane Valley", "Union Gap", "Beaver Dam",
  "Fond du Lac", "Green Bay", "Stevens Point", "Sheboygan Falls", "West Bend",
  "Wisconsin Rapids", "Rock Springs", "Fort McMurray", "Grande Prairie", "Medicine Hat",
  "Red Deer", "Sherwood Park", "Langley Township", "North Vancouver", "Saint John",
  "Corner Brook", "St. Johns", "Sydney River", "Greater Sudbury", "Kawartha Lakes",
  "Niagara Falls", "North Bay", "Sault Ste. Marie", "St. Catharines", "Thunder Bay",
  "Pointe Claire", "Quebec City"
)

## Cleaning Function ----

# Function to map each messy name to closest clean name
clean_city_name <- function(city, clean_vector) {
  distances <- stringdist(tolower(city), tolower(clean_vector), method = "osa")
  clean_vector[which.min(distances)]
}

df_clean <- geo_results_fail |>
  rowwise() |>
  mutate(city_clean = clean_city_name(city, clean_cities)) |>
  ungroup() |> 
  mutate(dist = stringdist(city, city_clean, method = "osa")) |> 
  select(region, city_clean, n) |> 
  rename(city = city_clean)

geo_results <- geo_results |> 
  filter(!is.na(lat)) 

geo_results_repeats <- df_clean |>
  geocode(city = city, state = region, method = "osm")

geo_results_rep2 <- geo_results_repeats |> 
  filter(is.na(lat)) |> 
  select(-lat, -long) |> 
  mutate(city = str_replace(city, "May's Landing", "Mays Landing"),
         city = str_replace(city, "St. Johns", "St. John's"),
         city = str_replace(city, "La Dera Ranch", "Ladera Ranch")
         ) |> 
  geocode(city = city, state = region, method = "osm")

geo_results_final <- geo_results |> 
  rbind(geo_results_repeats |> filter(!is.na(lat)), geo_results_rep2)

geo_results_final |> 
  write.csv("spirit_halloween_stores_geocoded.csv")


