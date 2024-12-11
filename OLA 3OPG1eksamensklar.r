library(dkstat)
library(tidyr)
library(dplyr)
library(lubridate)
#install.packages("pls")
#library(pls)
####### Opgave 1.1: Lav alle kombinationer af de 12 spørgsmål i forbrugerundersøgelsen fra 1. kvartal 2000 til 4. kvartal 2024

# Dataindhentning fra DST API
alltables <- dst_get_tables(lang = "da")                     # Hent liste af tabeller fra DST API
dst_search("Forbrugerforventninger")                         # Søg efter tabeller relateret til forbrugerforventninger
FORV1.meta <- dst_meta(table = "FORV1", lang = "da")         # Hent metadata for tabellen FORV1

# Definer filterliste for at hente relevant data
FORV1.filter <- list(
  INDIKATOR = "*",                                           # Alle indikatorer
  Tid = "*"                                                  # Alle tidsperioder
)

# Hent data baseret på filteret og omform til bredt format
Forbrugertillid <- dst_get_data(table = "FORV1", query = FORV1.filter, lang = "da")
Forbrugertillidwide <- pivot_wider(Forbrugertillid, names_from = INDIKATOR, values_from = value)

# Datarensning og transformation til kvartalsvis data
Forbrugertillid2024kvartal <- Forbrugertillidwide %>%
  filter(as.Date(TID) >= as.Date("2000-01-01")) %>%          # Filtrer data fra 2000 og frem
  select(-2) %>%                                             # Fjern den samlede FTI (anden kolonne)
  mutate(Quarter = paste0(year(as.Date(TID)), "-Q", quarter(as.Date(TID)))) %>%  # Opret kvartal-kolonne
  group_by(Quarter) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))   # Beregn gennemsnit for hver kvartal

# Konverter tibble til en data frame for at kunne bruge rownames
Forbrugertillid2024kvartal <- as.data.frame(Forbrugertillid2024kvartal)

#retter negativ korrelationsfejl på spørgsmål 6 og 8 

Forbrugertillid2024kvartal$`Priser i dag, sammenlignet med for et år siden` <- Forbrugertillid2024kvartal$`Priser i dag, sammenlignet med for et år siden`*-1
Forbrugertillid2024kvartal$`Arbejdsløsheden om et år, sammenlignet med i dag` <- Forbrugertillid2024kvartal$`Arbejdsløsheden om et år, sammenlignet med i dag`*-1

# Konverter første kolonne ('Quarter') til rækkenavne
rownames(Forbrugertillid2024kvartal) <- Forbrugertillid2024kvartal$Quarter

# Fjern den første kolonne ('Quarter'), da den nu er rækkenavne
Forbrugertillid2024kvartal <- Forbrugertillid2024kvartal[, -1]

###Kombinationsalgoritme

# Udvælg og filtrer kun numeriske kolonner fra 1 til 12
Forbrugertillid2024kvartal_numeric <- Forbrugertillid2024kvartal[, sapply(Forbrugertillid2024kvartal[, 1:12], is.numeric)]

# Opret en tom liste til at gemme resultaterne
kombinationresultat <- list()

# Beregn alle kombinationer og gem gennemsnit
n <- ncol(Forbrugertillid2024kvartal_numeric)
for (k in 1:n) {
  combs <- combn(n, k, simplify = FALSE)
  for (idx in combs) {
    avg_combination <- rowMeans(Forbrugertillid2024kvartal_numeric[, idx, drop = FALSE], na.rm = TRUE)
    comb_name <- paste("Comb", paste(idx, collapse = "-"), sep = "_")
    kombinationresultat[[comb_name]] <- avg_combination
  }
}

# Konverter resultaterne til en dataframe for bedre overskuelighed
results_df <- as.data.frame(kombinationresultat)

######## Opgave 1.2 – R2 og forbrugertillidsindikatorer

# Hent metadata og opsæt filter
Forbrug.meta <- dst_meta(table = "NKHC021", lang = "da")  # Hent metadata for tabellen NKHC021 fra DST

Forbrug.filter <- list(  # Definer et filter for at hente totalforbrug, 2020-priser og sæsonkorrigerede data
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

# Hent data baseret på filteret
Forbrugdata <- dst_get_data(table = "NKHC021", query = Forbrug.filter)  
Forbrugdata <- Forbrugdata[, -c(1:3)]  # Fjern de tre første kolonner med metadata

# Beregning af realvækst
Forbrugdata$Realvækst <- (Forbrugdata$value / dplyr::lag(Forbrugdata$value, 4) - 1) * 100  # Beregn realvækst år-over-år

# Udvælg relevant periode og kolonner (fra 2000Q1 til 2024Q2)
Forbrugdata2024 <- as.data.frame(Forbrugdata[c(41:139), c(1, 3)])  # Vælg rækkerne 41-138 (2000Q1-2024Q2) og kolonnerne TID og Realvækst

# Datoformatering
Forbrugdata2024$TID <- as.Date(Forbrugdata2024$TID)  # Konverter TID til datoformat

# Udtræk år og kvartal fra datoformatet
Forbrugdata2024$TID <- paste0(year(Forbrugdata2024$TID), "-Q", quarter(Forbrugdata2024$TID))  # Opret en 'År-Q'-format kolonne

# Konverter til data frame hvis det er en tibble
Forbrugdata2024 <- as.data.frame(Forbrugdata2024)

# Sæt værdierne i kolonne 1 (TID) som rækkenavne
rownames(Forbrugdata2024) <- Forbrugdata2024$TID

# Fjern den første kolonne (TID) forsigtigt
Forbrugdata2024 <- Forbrugdata2024[, -1, drop = FALSE]  # drop=FALSE sikrer, at vi ikke fjerner hele strukturen

#Lineær regression 

kombinationer2024Q2 <- results_df[1:(nrow(results_df) - 1), ]

combined_data <- cbind(Forbrugdata2024, kombinationer2024Q2)

# Antag, at din dataframe hedder 'combined_data'
realvaekst <- combined_data$Realvækst  # Afhængig variabel

# Fjern 'Realvækst'-kolonnen for at kun have kombinationerne
indicators <- combined_data[, -which(names(combined_data) == "Realvækst")]

# Opret en tom vektor til at gemme R²-værdierne
r2_values <- numeric(ncol(indicators))

# Loop igennem hver kombination og kør regression
for (i in 1:ncol(indicators)) {
  indicator <- indicators[, i]  # Vælg den i'te indikator
  model <- lm(realvaekst ~ indicator)  # Udfør regressionen
  r2_values[i] <- summary(model)$r.squared  # Gem R²-værdien
}

# Lav en tabel med kombinationer og deres tilsvarende R²-værdier
results <- data.frame(Indicator = names(indicators), R2 = r2_values)

# Sorter resultaterne efter R²-værdien
sorted_results <- results[order(-results$R2), ]

# Vis de bedste resultater
print(head(sorted_results))

###### Opgave 1.4 – Forudsig udviklingen i husholdningernes forbrugsudgift i 3. og 4. kvartal 2024. (Hint: brug jeres svar i opgave 2 fra OLA 2 sammen med jeres svar fra de forrige opgaver. Vær opmærksom på, om tallene fra forbrugerundersøgelsen for oktober er udgivet og I har den første måned i 4. kvartal)

#Slår bedste kombination med højeste r2 værdi sammen til en ny indikator 

forbrugertillid2024Q3 <- head(Forbrugertillid2024kvartal,-1) #Trækker to sidste rækker fra for at få fra 2000q1 til 2024q2

nyindikator <- rowMeans(forbrugertillid2024Q3[, c(3,7,9,11,12)], na.rm = TRUE) #Opretter den nye indikator ved at tage gennemsnit af de spørgsmål der havde kombination af højest r2 

realvdata <- Forbrugdata2024 #duplikerer min dataframe med realvækst 

samlet <- cbind(realvdata, nyindikator) #samler realvækst og den nye indikator i en dataframe 

model <- lm(Realvækst ~ nyindikator, data = samlet)          #Kører lineær regression med realvækst som y og nyindikator som x variabel 

summary(model)

###Forudsigelse

nyindikator_q4 <- rowMeans(Forbrugertillid2024kvartal[nrow(Forbrugertillid2024kvartal), c(3,7,9,11,12)], na.rm = TRUE)

# Opret et data frame med de beregnede nyindikator-værdier for Q3 og Q4
future_data <- data.frame(nyindikator = nyindikator_q4)

# Brug den eksisterende lineære model til at forudsige realvæksten for 2024Q3 og 2024Q4
predictions <- predict(model, newdata = future_data)

# Vis forudsigelserne for realvækst
print(predictions)

##Sammenligning med tidligere kvartaler 
# Beregn intervallet
mean_q4 <- mean(q4_x_values, na.rm = TRUE)
sd_q4 <- sd(q4_x_values, na.rm = TRUE)
lower_bound <- mean_q4 - sd_q4
upper_bound <- mean_q4 + sd_q4

# Plot med gennemsnitslinje og ±1 SD
plot(q4_x_values, type = "b", col = "darkgreen", lwd = 2,
     main = "X-værdi for 2024Q4 er ikke en ekstrem afvigelse ift. tidligere Q4 kvartaler",
     xlab = "År", ylab = "Nyindikator (x)", xaxt = "n")
axis(1, at = 1:length(q4_years), labels = q4_years, las = 2, cex.axis = 0.8)

# Tilføj gennemsnitslinje
abline(h = mean_q4, col = "green", lty = 2, lwd = 2)
# Tilføj ±1 SD bånd
abline(h = lower_bound, col = "orange", lty = 3, lwd = 2)
abline(h = upper_bound, col = "orange", lty = 3, lwd = 2)

# Marker 2024Q4
points(length(q4_x_values), 11.9, col = "red", pch = 19, cex = 1.5)

# Forklaring
legend("topright", legend = c("Nyindikator (Q4)", "2024Q4", "Gennemsnit", "±1 SD"),
       col = c("darkgreen", "red", "green", "orange"), lty = c(1, NA, 2, 3), pch = c(NA, 19, NA, NA), lwd = c(2, NA, 2, 2))

##Alle kvartaler 

# Data for alle kvartaler
all_quarters_values <- rowMeans(Forbrugertillid2024kvartal[, c(3, 7, 9, 11, 12)], na.rm = TRUE)

# Gennemsnit og standardafvigelse for alle kvartaler
mean_all <- mean(all_quarters_values, na.rm = TRUE)
sd_all <- sd(all_quarters_values, na.rm = TRUE)

cat("Gennemsnit for alle kvartaler:", mean_all, "\n")
cat("Standardafvigelse for alle kvartaler:", sd_all, "\n")

# Plot udviklingen for alle kvartaler
plot(all_quarters_values, type = "l", col = "blue", lwd = 2, 
     main = "Udvikling i Nyindikator for alle kvartaler (2000-2024)",
     xlab = "Kvartaler (Tidsrækkefølge)", ylab = "Nyindikator (x)")
abline(h = mean_all, col = "green", lty = 2, lwd = 2)  # Gennemsnit
abline(h = mean_all + sd_all, col = "orange", lty = 2)  # +1 SD
abline(h = mean_all - sd_all, col = "orange", lty = 2)  # -1 SD

# Marker 2024-Q4
points(length(all_quarters_values), 11.86, col = "red", pch = 19, cex = 1.5)

# Forklaring
legend("topright", legend = c("Nyindikator (Alle kvartaler)", "Gennemsnit", "+/-1 SD", "2024-Q4"),
       col = c("blue", "green", "orange", "red"), lty = c(1, 2, 2, NA), pch = c(16, NA, NA, 19), lwd = c(2, 2, 1, NA))



#######Opgave 1.5 – Sammenlign med en mikroøkonomisk indikator
#Find den bedste indikator, der alene består af mikroøkonomiske spørgsmål i forbrugertillidsundersøgelsen og sammenlign indikatoren med jeres tidligere svar i opgave 1.

# Opret en ny data frame med mikroøkonomiske spørgsmål 
Forbrugertillid_mikro <- Forbrugertillid2024kvartal[, c(1,2,9,11,12)]

#Kombinationsalgortime for mikrospørgsmål 

# Filtrer kun de numeriske kolonner (hvis nogle kolonner ikke er numeriske)
Forbrugertillid_mikro_numeric <- Forbrugertillid_mikro %>%
  select(where(is.numeric))

Forbrugertillid_mikro_numeric <- Forbrugertillid_mikro_numeric[1:(nrow(Forbrugertillid_mikro_numeric)), ]

# Opret en tom liste til at gemme resultaterne
mikro_combinations_results <- list() # Nyt navn for resultaterne

# Beregn antallet af kolonner (numeriske)
n <- ncol(Forbrugertillid_mikro_numeric)

# Loop over alle mulige kombinationer (fra 1 til n spørgsmål)
for (k in 1:n) {  # Starter fra 1 til n for at inkludere alle mulige kombinationer
# Generer alle kombinationer af k spørgsmål
  combs <- combn(n, k)
  
 # For hver kombination, beregn gennemsnittet af de udvalgte kolonner
  for (i in 1:ncol(combs)) {
    selected_columns <- Forbrugertillid_mikro_numeric[, combs[, i], drop = FALSE]
    avg_combination <- rowMeans(selected_columns, na.rm = TRUE)
    
# Gem resultatet i listen med en beskrivelse af kombinationen
    comb_name <- paste("Mikro_Comb", paste(combs[, i], collapse = "-"), sep = "_")
    mikro_combinations_results[[comb_name]] <- avg_combination
  }
}

 # Konverter resultaterne til en dataframe for bedre overskuelighed
mikro_results_df <- as.data.frame(mikro_combinations_results)  # Nyt navn til dataframen

# Tjek resultaterne
print(head(mikro_results_df))


##### R2 værdi 

mikro_results_df <- mikro_results_df[-nrow(mikro_results_df), ]  # Fjern sidste række
combined_mikro_data <- cbind(
  Forbrugdata2024,
  mikro_results_df - as.vector(mikro_results_df[nrow(mikro_results_df), ])
)

# Definer afhængig variabel (realvækst)
realvaekst2 <- combined_mikro_data$Realvækst  

# Fjern 'Realvækst'-kolonnen for at kun have mikroindikatorerne
mikro_indicators <- combined_mikro_data[, -which(names(combined_mikro_data) == "Realvækst")]

# Opret en tom vektor til at gemme R²-værdierne
mikro_r2_values <- numeric(ncol(mikro_indicators))

# Loop igennem hver mikroøkonomiske kombination og kør regression
for (i in 1:ncol(mikro_indicators)) {
  mikro_indicator <- mikro_indicators[, i] # Vælg den i'te mikroindikator
  model <- lm(realvaekst2 ~ mikro_indicator) # Udfør regressionen
  mikro_r2_values[i] <- summary(model)$r.squared # Gem R²-værdien
}

# Lav en tabel med mikroindikator-kombinationer og deres tilsvarende R²-værdier
mikro_results <- data.frame(Indicator = names(mikro_indicators), R2 = mikro_r2_values)

# Sorter resultaterne efter R²-værdien
sorted_mikro_results <- mikro_results[order(-mikro_results$R2), ]

# Vis de bedste resultater
print(head(sorted_mikro_results))


# Fjern de to sidste rækker for at få data til og med 2024Q3
forbrugertillid_mikro_2024Q3 <- head(Forbrugertillid_mikro, -1)

# Beregn ny mikroøkonomisk indikator med den bedste mikroøkonomiske kombination (fx kolonne 3)
ny_mikro_indikator <- forbrugertillid_mikro_2024Q3[, 3]

# Kombiner mikroindikator med realvækstdatasættet
samlet_mikro <- cbind(realvdata, ny_mikro_indikator)

# Udfør lineær regression med mikroindikatoren
model_mikro <- lm(Realvækst ~ ny_mikro_indikator, data = samlet_mikro)

# Se resultaterne af regressionen
summary(model_mikro)

# Beregn værdien af mikroindikatoren for 2024Q3 og 2024Q4
ny_mikro_indikator_q4 <- Forbrugertillid_mikro[nrow(Forbrugertillid_mikro), 3]

# Opret en data frame med de beregnede ny_mikro_indikator-værdier for Q3 og Q4
future_data_mikro <- data.frame(ny_mikro_indikator = ny_mikro_indikator_q4)

# Brug modellen til at forudsige realvæksten for 2024Q3 og 2024Q4
predictions_mikro <- predict(model_mikro, newdata = future_data_mikro)

 # Vis forudsigelserne for realvækst baseret på mikroindikatoren
print(predictions_mikro)


