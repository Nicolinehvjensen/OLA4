###Opgave 3###

library(dkstat)
library(tidyr)
library(dplyr)
library(lubridate)
library(pROC)

##Datindhentning og rensing af forbrugertillid for 2000Q1-2024Q4

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

##Dataindhentning og rensing af husholdningers forbrugsudgifter på dansk område 

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
Forbrugdata2024 <- as.data.frame(Forbrugdata[c(41:139), c(1, 3)])  # Vælg rækkerne 41-138 (2000Q1-2024Q3) og kolonnerne TID og Realvækst

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

###Lav en Machine Learning model, der med afsæt i DST’s forbrugertillidsindikator, kan forudsige om julehandlen i 2024 er større end i 2023.

## Dummy variabel 

Forbrugdata2024$Dummy <- ifelse(Forbrugdata2024$Realvækst > 0, 1, 0) 
table(Forbrugdata2024$Dummy)

#22 gange går den ned, 77 gange går ned op i perioden 2000Q1 - 2024Q3

#Kombinerer datasæt 
combined_data <- merge(Forbrugertillid2024kvartal, Forbrugdata2024, by = "row.names")
rownames(combined_data) <- combined_data$Row.names
combined_data <- combined_data[, -1]  # Fjern Row.names-kolonnen

#Logistisk regression 

# Træner modellen
julemodel <- glm(
  formula = Dummy ~ Forbrugertillidsindikatoren,
  data = combined_data,
  family = "binomial"
)

summary(julemodel)

###Predict 

# Opret en ny dataframe for Q4 2024 baseret på række 81
Q4_2024_data <- data.frame(Forbrugertillidsindikatoren = Forbrugertillid2024kvartal$Forbrugertillidsindikatoren[100])

print(Q4_2024_data)

#Forudsigelse

Q4_2024_prob <- predict(julemodel, newdata = Q4_2024_data, type = "response")

print(Q4_2024_prob)

### Konfusionsmatrix 

predicted_probs <- predict(julemodel, type = "response")

predicted_classes <- ifelse(predicted_probs > 0.7, 1, 0)

table(Predicted = predicted_classes, Actual = combined_data$Dummy)

# ROC Kurve

roc_curve <- roc(combined_data$Dummy, predicted_probs)

plot(roc_curve, col = "blue", main = "ROC-kurve")

auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))


summary(predicted_probs)
mean(Forbrugertillid2024kvartal$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)


