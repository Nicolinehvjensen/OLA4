#Opgave 2
library(DBI)
library(RMySQL)

{
# Fjern alt undtagen tal i 'price' kolonnen
colldf$price <- gsub("[^0-9]", "", colldf$price)

# Konverter til numerisk
colldf$price <- as.numeric(colldf$price)

# Opret en unik identifikator baseret på location og dealerLogo
colldf$dealerIdentifier <- paste(colldf$location, colldf$dealerLogo)

# Generer et unikt forhandlerID startende fra 1
colldf$dealerID <- as.numeric(factor(colldf$dealerIdentifier))

# Fjern midlertidige kolonner, hvis de ikke er nødvendige
colldf$dealerIdentifier <- NULL

# Tjek resultaterne for at sikre korrekt generering
head(colldf[, c("location", "dealerLogo", "dealerID", "price")])

# Opret forbindelse til MySQL-serveren
con <- dbConnect(
  RMySQL::MySQL(),
  dbname = "car_database",
  host = "localhost",    # Korrekt
  port = 3306,           # Angiv porten separat
  user = "root",
  password = "Jakelong99"
)

# Hent unikke værdier for Dealer
dealers <- unique(colldf[, c("dealerLogo", "location")])

# Indsæt i tabellen
dbWriteTable(
  con,
  name = "Dealer",
  value = dealers,
  append = TRUE,
  row.names = FALSE
)


# Fjern overflødige kolonner fra colldf
cars <- colldf[, c("carid", "makemodel", "details", "description", "location", "link")]

# Indsæt i tabellen
dbWriteTable(
  con,
  name = "Car",
  value = cars,
  append = TRUE,
  row.names = FALSE
)

# Opret Price-dataframe
prices <- colldf[, c("carid", "price", "scrapedate")]
  
# Tilføj 'sold' kolonne som FALSE
prices$sold <- FALSE
  
# Indsæt i Price-tabellen i databasen
dbWriteTable(
  con,
  name = "Price",        # Navnet på tabellen i MySQL
  value = prices,        # Dataframe med price-data
  append = TRUE,         # Tilføj data uden at overskrive eksisterende data
  row.names = FALSE      # Undgå række-navne
)

colSums(is.na(colldf))


}

#Opgave 2.3 

{
# --- Opret og forbered ny data ---
Opgave_1.3 <- colldf

# 1. Fjern de første 5 rækker
Opgave_1.3 <- Opgave_1.3[-(1:5), ]

# 2. Opdater scrapedate (tilføj én dag)
Opgave_1.3$scrapedate <- as.Date(Opgave_1.3$scrapedate) + 1

# 3. Opdater priser for de første 3 rækker
Opgave_1.3[1:3, "price"] <- c("231438", "234498", "224196")

# 4. Tilføj nye biler
new_rows <- data.frame(
  price = c("199995", "149995"),
  props = NA,
  makemodel = c("Model 1", "Model 2"), # Tilføj en standard værdi
  details = NA,
  description = NA,
  location = "Unknown",  # Tilføj en standard lokation
  dealerLogo = NA,
  link = NA,
  carid = c("7000001", "7000002"),
  scrapedate = as.Date(Sys.Date()) + 1,
  dealer_id = NA
)

# Ændr kolonnenavnet fra "dealerID" til "dealer_id"
colnames(Opgave_1.3)[colnames(Opgave_1.3) == "dealerID"] <- "dealer_id"

# Kontrollér kolonnenavne og match
colnames(new_rows) <- colnames(Opgave_1.3)
Opgave_1.3 <- rbind(Opgave_1.3, new_rows)

# Konverter pris til numerisk format
Opgave_1.3$price <- as.numeric(gsub("[^0-9.]", "", Opgave_1.3$price))

# Sørg for korrekt datatyper
Opgave_1.3$carid <- as.character(Opgave_1.3$carid)
Opgave_1.3$scrapedate <- as.Date(Opgave_1.3$scrapedate)

# --- Hent eksisterende data ---
existing_data <- dbReadTable(con, "Price")
existing_cars <- dbReadTable(con, "Car")

# --- Find manglende biler i Car-tabellen ---
missing_cars <- Opgave_1.3[!Opgave_1.3$carid %in% existing_cars$carid, c("carid", "makemodel", "details", "description", "location", "dealer_id", "link")]


# Hvis der er manglende biler, indsæt dem i Car-tabellen
if (nrow(missing_cars) > 0) {
  dbWriteTable(
    con,
    name = "Car",
    value = missing_cars,
    append = TRUE,
    row.names = FALSE
  )
}

# --- Find nye records i Price-tabellen ---
new_records <- Opgave_1.3[!Opgave_1.3$carid %in% existing_data$carid, ]

# Hvis der er nye records, indsæt dem i Price-tabellen
if (nrow(new_records) > 0) {
  new_prices <- new_records[, c("carid", "price", "scrapedate")]
  new_prices$sold <- FALSE  # Tilføj sold som FALSE
  
  dbWriteTable(
    con,
    name = "Price",
    value = new_prices,
    append = TRUE,
    row.names = FALSE
  )
}

# --- Find ændrede priser ---
merged_data <- merge(Opgave_1.3, existing_data, by = "carid", suffixes = c("_new", "_old"))
changed_records <- merged_data[merged_data$price_new != merged_data$price_old, ]

# Opdater ændrede priser i SQL
# Step 1: Find ændrede records
merged_data <- merge(Opgave_1.3, existing_data, by = "carid", suffixes = c("_new", "_old"))

# Filtrer for ændrede priser
changed_records <- merged_data[merged_data$price_new != merged_data$price_old, ]

# Step 2: Tilføj nye rækker for opdaterede priser
if (nrow(changed_records) > 0) {
  # Forbered nye rækker med opdaterede priser
  new_prices <- data.frame(
    carid = changed_records$carid,
    price = changed_records$price_new,
    scrapedate = changed_records$scrapedate_new,
    sold = FALSE # Nye opdaterede rækker skal ikke markeres som solgte
  )
  
  # Indsæt nye rækker i databasen
  dbWriteTable(
    con,
    name = "Price",
    value = new_prices,
    append = TRUE,
    row.names = FALSE
  )
}


# --- Markér solgte biler ---
missing_records <- existing_data[!existing_data$carid %in% Opgave_1.3$carid, ]
if (nrow(missing_records) > 0) {
  for (i in 1:nrow(missing_records)) {
    dbExecute(
      con,
      paste0(
        "UPDATE Price SET sold = TRUE WHERE carid = '", missing_records$carid[i], "';"
      )
    )
  }
}
}
