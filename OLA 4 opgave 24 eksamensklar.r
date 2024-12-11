library(httr)
library(rvest)

# Funktion til at hente data baseret på lokation
hent_data <- function(location, type) {
  base_url <- "https://envs2.au.dk/Luftdata/Presentation/table"
  url <- paste0(base_url, "/", type, "/", location)
  
  # Hent siden for at få CSRF-token
  res <- GET(url)
  html <- content(res, as = "text")
  csrf_token <- read_html(html) %>%
    html_node("input[name='__RequestVerificationToken']") %>%
    html_attr("value")
  
  # Send POST-anmodning med CSRF-token
  post_url <- paste0(base_url, "/MainTable/", type, "/", location)
  res_post <- POST(
    url = post_url,
    body = list("__RequestVerificationToken" = csrf_token),
    encode = "form"
  )
  
  # Parse og returner tabellen
  data <- read_html(content(res_post, as = "text")) %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  
  return(data)
}

# Eksempler på brug af funktionen:
# HC Andersens Boulevard
hcab_data <- hent_data("HCAB", "Copenhagen")

# Anholt
anholt_data <- hent_data("ANHO", "Rural")

# Banegårdsgade (Aarhus)
aarh3_data <- hent_data("AARH3", "Aarhus")

# Risø
risoe_data <- hent_data("RISOE", "Rural")

# Udskriv data for at se resultatet
print(hcab_data)
print(anholt_data)
print(aarh3_data)
print(risoe_data)
