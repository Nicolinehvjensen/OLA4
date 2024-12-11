###OLA 3 OPG 5

library(httr)
library(jsonlite)


###5.2 - Find 05727

# API endpoint og nøgle til station metadata
base_url <- "https://dmigw.govcloud.dk/v2/metObs/collections/station/items"
api_key <- "71a66ef4-f152-408f-849f-fa4db138d690"  # Indsæt din API nøgle her

# Hent station metadata
response <- GET(base_url, add_headers("X-Gravitee-Api-Key" = api_key))
stations_data <- fromJSON(content(response, "text", encoding = "UTF-8"))

# Konverter dataene til en data frame for nemmere håndtering
stations_df <- as.data.frame(stations_data$features$properties)

#Grepl efter 05272
station_05272 <- subset(stations_df, stationId == "05272")

### 5.3 

# 1. Angiv base URL og API-nøgle
collections <- "https://dmigw.govcloud.dk/v1/forecastdata/collections"
api_key <- "71a66ef4-f152-408f-849f-fa4db138d690"  # Udskift med din egen API-nøgle

# 2. Send GET-anmodning til API'et
response <- GET(
  url = collections,
  add_headers(`X-Gravitee-Api-Key` = api_key)
)

# 3. Hent API-indhold som tekst
collectiondata <- content(
  response,          # Objektet med API-svaret
  as = "text",       # Specificerer tekstformat
  encoding = "UTF-8" # Angiver encoding
)

# 4. Parse JSON-indholdet til en R-struktur
collectiondataparsed <- fromJSON(collectiondata, flatten = TRUE)

# 5. Konverter collections til en data frame
collectionsdf <- as.data.frame(collectiondataparsed$collections)

# 6. Hvor mange collections er der?
numcollections <- nrow(collectionsdf)
print(numcollections)

# 7. Find ID og navn på Lillebælt
print(collectionsdf)

lillebaelt_id <- collectionsdf[grepl("dkss_lb", collectionsdf$id, ignore.case = TRUE), "id"]

print(lillebaelt_id)

#Filformat

content_type <- headers(response)$`content-type`

print(content_type)

### 5.4

##Til at duplikere den første graf
# Indlæs nødvendige biblioteker
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)

# Opsæt API og parametre
base_url <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items"
api_key <- "f1f5621b-d579-474b-a371-47a267df3e2f"  # Indsæt din API-nøgle her
station_ids <- c("06070" = "Aarhus Lufthavn", "06079" = "Anholt Havn")
start_date <- "2022-11-14T00:00:00Z"
end_date <- "2022-11-21T23:59:59Z"

# Funktion til at hente og kombinere data for vindhastighed og vindretning
fetch_data <- function(station_id, station_name) {
  params <- c("wind_speed", "wind_dir")  # De parametre, vi ønsker
  data_list <- lapply(params, function(param) {
    url <- paste0(base_url, "?stationId=", station_id, "&datetime=", start_date, "/", end_date, 
                  "&parameterId=", param, "&limit=1000")
    response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
    if (status_code(response) != 200) return(NULL)
    fromJSON(content(response, "text", encoding = "UTF-8"))$features
  })
  
  if (any(sapply(data_list, is.null))) return(NULL)  # Hvis data mangler
  data.frame(
    time = as.POSIXct(data_list[[1]]$properties$observed, format="%Y-%m-%dT%H:%M:%SZ"),
    speed = data_list[[1]]$properties$value,
    direction = data_list[[2]]$properties$value,
    station = station_name
  )
}

# Hent data for alle stationer
data <- do.call(rbind, lapply(names(station_ids), function(id) fetch_data(id, station_ids[id])))

# Tjek data for fejl
if (is.null(data) || nrow(data) == 0) stop("Ingen gyldige data tilgængelige for plotting.")
print(unique(data$station))  # Debug: Tjek stationer

# Rens data for ukendte eller forkerte stationer
data <- data %>% filter(station %in% c("Aarhus Lufthavn", "Anholt Havn"))

# Konverter retning (grader) til kardinalpunkter
get_wind_direction <- function(angle) {
  directions <- c("N", "NØ", "Ø", "SØ", "S", "SV", "V", "NV", "N")
  if (is.na(angle) || angle < 0 || angle > 360) return(NA)  # Håndter ugyldige værdier
  directions[round(angle / 45) + 1]
}
data <- data %>%
  mutate(direction_label = sapply(direction, get_wind_direction)) %>%
  filter(!is.na(direction_label))  # Fjern eventuelle ugyldige etiketter

# Reducér antallet af labels
data_reduced <- data %>% filter(row_number() %% 6 == 1)

# Plot vindhastighed og retning
ggplot(data, aes(x = time, y = speed, color = station)) +
  geom_line(size = 1) +
  geom_text(data = data_reduced, aes(label = direction_label), vjust = -1, size = 3) +
  scale_color_manual(
    values = c("Aarhus Lufthavn" = "blue", "Anholt Havn" = "red"),
    breaks = c("Aarhus Lufthavn", "Anholt Havn"),
    labels = c("Aarhus Lufthavn", "Anholt Havn")
  ) +
  guides(color = guide_legend(title = "Station")) +
  labs(
    title = "Vindhastighed og retning under stormen i November 2022",
    x = "Tid", y = "Vindstyrke (m/s)", color = "Station"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Forbedring for stormen i november
#####
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)

# Opsæt API og parametre
base_url <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items"
api_key <- "f1f5621b-d579-474b-a371-47a267df3e2f"  # Indsæt din API-nøgle her
station_ids <- c("06070" = "Aarhus Lufthavn", "06079" = "Anholt Havn")
start_date <- "2022-11-14T00:00:00Z"
end_date <- "2022-11-21T23:59:59Z"

# Funktion til at hente og kombinere data for vindhastighed og vindretning
fetch_data <- function(station_id, station_name) {
  params <- c("wind_speed", "wind_dir")  # De parametre, vi ønsker
  data_list <- lapply(params, function(param) {
    url <- paste0(base_url, "?stationId=", station_id, "&datetime=", start_date, "/", end_date, 
                  "&parameterId=", param, "&limit=1000")
    response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
    if (status_code(response) != 200) return(NULL)
    fromJSON(content(response, "text", encoding = "UTF-8"))$features
  })
  
  if (any(sapply(data_list, is.null))) return(NULL)
  
  data.frame(
    time = as.POSIXct(data_list[[1]]$properties$observed, format = "%Y-%m-%dT%H:%M:%SZ"),
    speed = data_list[[1]]$properties$value,
    direction = data_list[[2]]$properties$value,
    station = station_name
  )
}

# Hent data for begge stationer
data <- do.call(rbind, lapply(names(station_ids), function(id) fetch_data(id, station_ids[id])))

# Fejlhåndtering
if (is.null(data) || nrow(data) == 0) stop("Ingen gyldige data hentet")

# Konverter retning (grader) til kardinalpunkter
get_wind_direction <- function(angle) {
  directions <- c("N", "NØ", "Ø", "SØ", "S", "SV", "V", "NV", "N")
  if (is.na(angle) || angle < 0 || angle > 360) return(NA)
  directions[round(angle / 45) + 1]
}

data <- data %>%
  mutate(direction_label = sapply(direction, get_wind_direction)) %>%
  filter(!is.na(direction_label))

# Reducér antallet af labels for pile
data_reduced <- data %>%
  filter(row_number() %% 12 == 1) %>%
  mutate(
    xend = time + 0.001 * cos((direction - 90) * pi / 180),  # Pilens x-retning tæt på punkt
    yend = speed + 0.2 * sin((direction - 90) * pi / 180)    # Pilens y-retning tæt på punkt
  )

# Visualisering
ggplot(data, aes(x = time, y = speed, color = station)) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs"), size = 1.2) +
  geom_segment(
    data = data_reduced,
    aes(x = time, y = speed, xend = xend, yend = yend, color = station),
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    size = 0.8
  ) +
  geom_text(
    data = data_reduced,
    aes(label = direction_label),
    vjust = -1, size = 3, color = "black", check_overlap = TRUE
  ) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2022-11-14 06:00"), as.POSIXct("2022-11-21 18:00"), by = "12 hours"),
    date_labels = "%b %d %H:%M"
  ) +
  scale_y_continuous(
    breaks = seq(0, 15, by = 3),
    limits = c(0, 15)
  ) +
  scale_color_manual(
    values = c("Aarhus Lufthavn" = "blue", "Anholt Havn" = "red"),
    labels = c("Aarhus Lufthavn", "Anholt Havn")
  ) +
  labs(
    title = "Vindretning og styrke under stormen i november 2022",
    x = "Tid",
    y = "Vindstyrke (m/s)",
    color = "Station"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

####Stormen oktober 2023
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)

# Opsæt API og parametre
base_url <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items"
api_key <- "f1f5621b-d579-474b-a371-47a267df3e2f"  # Indsæt din API-nøgle her
station_ids <- c("06070" = "Aarhus Lufthavn", "06079" = "Anholt Havn")
start_date <- "2023-10-20T00:00:00Z"
end_date <- "2023-10-22T23:59:59Z"

# Funktion til at hente og kombinere data for vindhastighed og vindretning
fetch_data <- function(station_id, station_name) {
  params <- c("wind_speed", "wind_dir")  # De parametre, vi ønsker
  data_list <- lapply(params, function(param) {
    url <- paste0(base_url, "?stationId=", station_id, "&datetime=", start_date, "/", end_date, 
                  "&parameterId=", param, "&limit=1000")
    response <- GET(url, add_headers("X-Gravitee-Api-Key" = api_key))
    if (status_code(response) != 200) return(NULL)
    fromJSON(content(response, "text", encoding = "UTF-8"))$features
  })
  
  if (any(sapply(data_list, is.null))) return(NULL)
  
  data.frame(
    time = as.POSIXct(data_list[[1]]$properties$observed, format = "%Y-%m-%dT%H:%M:%SZ"),
    speed = data_list[[1]]$properties$value,
    direction = data_list[[2]]$properties$value,
    station = station_name
  )
}

# Hent data for begge stationer
data <- do.call(rbind, lapply(names(station_ids), function(id) fetch_data(id, station_ids[id])))

# Fejlhåndtering
if (is.null(data) || nrow(data) == 0) stop("Ingen gyldige data hentet")

# Konverter retning (grader) til kardinalpunkter
get_wind_direction <- function(angle) {
  directions <- c("N", "NØ", "Ø", "SØ", "S", "SV", "V", "NV", "N")
  if (is.na(angle) || angle < 0 || angle > 360) return(NA)
  directions[round(angle / 45) + 1]
}

data <- data %>%
  mutate(direction_label = sapply(direction, get_wind_direction)) %>%
  filter(!is.na(direction_label))

# Reducér antallet af labels for pile
data_reduced <- data %>%
  filter(row_number() %% 12 == 1) %>%
  mutate(
    xend = time + 0.001 * cos((direction - 90) * pi / 180),  # Pilens x-retning tæt på punkt
    yend = speed + 0.2 * sin((direction - 90) * pi / 180)    # Pilens y-retning tæt på punkt
  )

# Visualisering
ggplot(data, aes(x = time, y = speed, color = station)) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs"), size = 1.2) +
  geom_segment(
    data = data_reduced,
    aes(x = time, y = speed, xend = xend, yend = yend, color = station),
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    size = 0.8
  ) +
  geom_text(
    data = data_reduced,
    aes(label = direction_label),
    vjust = -1, size = 3, color = "black", check_overlap = TRUE
  ) +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2023-10-20 06:00"), as.POSIXct("2023-10-22 18:00"), by = "12 hours"),
    date_labels = "%b %d %H:%M"
  ) +
  scale_y_continuous(
    breaks = seq(0, 15, by = 3),
    limits = c(0, 15)
  ) +
  scale_color_manual(
    values = c("Aarhus Lufthavn" = "blue", "Anholt Havn" = "red"),
    labels = c("Aarhus Lufthavn", "Anholt Havn")
  ) +
  labs(
    title = "Vindretning og styrke under stormen i oktober 2023",
    x = "Tid",
    y = "Vindstyrke (m/s)",
    color = "Station"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

