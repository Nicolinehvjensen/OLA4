# Video fra Wulf Uge 46

# Opgave 1.1 – Hente data fra Bilbasen
# 

# min første kode
{

library(httr)
library(rvest)
library(dplyr)
library(stringr)

# Første side        
startlink <- "https://www.bilbasen.dk/brugt/bil/renault?fuel=3&includeengroscvr=true&includeleasing=false"
rawres <- GET(url=startlink)
rawres$status_code 
rawcontent <- httr::content(rawres,as="text")

# Transformer text til html-nodes
page <- read_html(rawcontent)

# Hent bil-elementer fra startside
carlist <- page %>% html_elements("article")

# tag-liste
ptag <- ".Listing_price__6B3kE"
proptag <- ".Listing_properties___ptWv"
mmtag <- ".Listing_makeModel__7yqgs"
dettag <- "[class^='Listing-details']"
dettagitem <- "[class^='ListingDetails_listItem']"
desctag <- "[class^='Listing_description']"
loctag <- ".Listing_location__nKGQz"


# Dataframe til opsamling
cn <- c("price,","properties","model", "detailitems","description","description","location","link","carid","scrapedate")
colldf <- as.data.frame(matrix(data=NA,nrow = 0,ncol = 10))
colnames(colldf)=cn

# Løkke gennem bil-elementer og udtræk data
for (car in carlist) {
  price <- tryCatch(car %>% html_element(ptag) %>% html_text(trim = TRUE), error = function(e) NA)
  props <- tryCatch(car %>% html_element(proptag) %>% html_text(trim = TRUE), error = function(e) NA)
  makemodel <- tryCatch(car %>% html_element(mmtag) %>% html_text(trim = TRUE), error = function(e) NA)
  details <- tryCatch(car %>% html_elements(dettagitem) %>% html_text(trim = TRUE) %>% paste0(collapse = "_"), error = function(e) NA)
  description <- tryCatch(car %>% html_element(desctag) %>% html_text(trim = TRUE), error = function(e) NA)
  location <- tryCatch(car %>% html_element(loctag) %>% html_text(trim = TRUE), error = function(e) NA)
  link <- tryCatch(car %>% html_element("a") %>% html_attr("href"), error = function(e) NA)
  carid <- tryCatch(str_extract(link, "[0-9]{7}"), error = function(e) NA)
  scrapedate <- Sys.time()
  
  # Midlertidig data frame til indsamlede data
  tmpdf <- data.frame(price, props, makemodel, details, description, location, link, carid, scrapedate, stringsAsFactors = FALSE)
  
  # Tilføj til hoved dataframen
  colldf <- rbind(colldf, tmpdf)
}


print(colldf)
}

# Indhenting af resterende sider
# Loop for at hente de resterende sider

for (page_num in 2:13) {
  # Dynamisk URL for hver side
  url <- paste0("https://www.bilbasen.dk/brugt/bil/renault?fuel=3&includeengroscvr=true&includeleasing=false&page=", page_num)
  
  # Hent sideindhold
  rawres <- GET(url = url)
  rawcontent <- httr::content(rawres, as = "text")
  
  # Transformer text til html-nodes
  page <- read_html(rawcontent)
  
  # Hent bil-elementer fra nuværende side
  carlist <- page %>% html_elements("article")
  
  # tag-liste
  ptag <- ".Listing_price__6B3kE"
  proptag <- ".Listing_properties___ptWv"
  mmtag <- ".Listing_makeModel__7yqgs"
  dettagitem <- "[class^='ListingDetails_listItem']"
  desctag <- "[class^='Listing_description']"
  loctag <- ".Listing_location__nKGQz"
  
  # Løkke gennem bil-elementer og udtræk data
  for (car in carlist) {
    price <- tryCatch(car %>% html_element(ptag) %>% html_text(trim = TRUE), error = function(e) NA)
    props <- tryCatch(car %>% html_element(proptag) %>% html_text(trim = TRUE), error = function(e) NA)
    makemodel <- tryCatch(car %>% html_element(mmtag) %>% html_text(trim = TRUE), error = function(e) NA)
    details <- tryCatch(car %>% html_elements(dettagitem) %>% html_text(trim = TRUE) %>% paste0(collapse = "_"), error = function(e) NA)
    description <- tryCatch(car %>% html_element(desctag) %>% html_text(trim = TRUE), error = function(e) NA)
    location <- tryCatch(car %>% html_element(loctag) %>% html_text(trim = TRUE), error = function(e) NA)
    link <- tryCatch(car %>% html_element("a") %>% html_attr("href"), error = function(e) NA)
    carid <- tryCatch(str_extract(link, "[0-9]{7}"), error = function(e) NA)
    scrapedate <- Sys.time()
    
    # Midlertidig data frame til indsamlede data
    tmpdf <- data.frame(price, props, makemodel, details, description, location, link, carid, scrapedate, stringsAsFactors = FALSE)
    
    # Tilføj til hoved dataframen
    colldf <- rbind(colldf, tmpdf)
  }
  # Vent 3 sekunder for ikke at overbelaste serveren
  Sys.sleep(3)
}

# Print det samlede dataframe
print(colldf)
colldf <- Opgave_1.1
# Opgave 1.2 – Rense data
{
# Skateboard

# Forsøg1
{
# Hent teksten fra colldf[1,5]
text <- colldf[1,5]

# Fjern specialtegn, behold bogstaver (inkl. æ,ø,å), tal, punktum og komma
text <- gsub("[^a-zA-Z0-9æøåÆØÅ., \\n]", "", text)

# Erstat newline med ". "
text <- gsub("\\n+", ". ", text)

# Fjern ekstra mellemrum
text <- gsub("\\s+", " ", text)

# Fjern mellemrum før punktummer
text <- gsub("\\s+\\.", ".", text)

# Sørg for korrekt mellemrum efter punktummer
text <- gsub("\\.\\s+", ". ", text)

# Print den færdige tekst
cat(text)
}
cat(cleaned_text)

# Forsøg 2 // Bedst i test
{
# Hent teksten fra colldf[1,5]
text2 <- colldf[1,5]

# Fjern specialtegn, behold bogstaver (inkl. æ,ø,å), tal, punktum og komma
text2 <- gsub("[^a-zA-Z0-9æøåÆØÅ., \\n]", "", text2)

# Erstat newline med ". "
text2 <- gsub("\\n+", ". ", text2)

# Fjern ekstra mellemrum
text2 <- gsub("\\s+", " ", text2)

# Fjern mellemrum før punktummer
text2 <- gsub("\\s+\\.", ".", text2)

# Sørg for korrekt mellemrum efter punktummer
text2 <- gsub("\\.\\s+", ". ", text2)

# Fjern sekvenser af flere punktummer og mellemrum som ".. ." -> ". "
text2 <- gsub("\\.\\.+\\s*\\.?", ". ", text2)
}
# Print den færdige tekst
cat(text2)

# Forsøg 3
{
  # Hent tekst fra colldf[1,5]
  tekst <- colldf[1,5]
  
  # Fjern tegn som ikke er punktum eller komma og erstat linjeskift med ". "
  renset_tekst <- gsub("[^a-zA-Z0-9.,æøåÆØÅ ]", "", tekst)  # Fjern alle tegn undtagen bogstaver, tal, komma, punktum og mellemrum
  renset_tekst <- gsub("\n", ". ", renset_tekst)  # Erstat linjeskift med ". "
  renset_tekst <- gsub(" +", " ", renset_tekst)  # Erstat flere mellemrum med ét mellemrum
  renset_tekst <- trimws(renset_tekst)  # Fjern ledende og efterfølgende mellemrum

}
# Vis den rensede tekst
print(renset_tekst)
}

# Opgave 1.3 - Hente nye data - simuleret
{
# Ny df til 1.3
Opgave_1.3 <- colldf

# Fjerner de 5 første rækker
Opgave_1.3 <- Opgave_1.3[-(1:5),]

# Ændre datoen for webscraping
Opgave_1.3[,9] <- Opgave_1.3[,9]+86400

# Ændre prisen i 3 første rækker
Opgave_1.3[1:3,1] <- NA
# 226900*1.02 = 231.438 kr.
Opgave_1.3[1,1] <- "231.438 kr."
# 229900*1.02 = 234.498 kr.
Opgave_1.3[2,1] <- "234.498 kr."
# 219800*1.02 = 224.196 kr.
Opgave_1.3[3,1] <- "224.196 kr."

# Oprettelse af 2 nye rækker
# !!MANGLER!!
}

# Opgave 1.4 - Hente tyske data
library(httr)
library(rvest)
library(dplyr)
library(stringr)

# Første side        
startlink2 <- "https://www.12gebrauchtwagen.de/suchen?s%5Bmk%5D=63&s%5Bmd%5D=&s%5By_min%5D=&s%5By_max%5D=&s%5Bm_min%5D=&s%5Bm_max%5D=&s%5Bprice_or_rate%5D=price&s%5Bpr_min%5D=&s%5Bpr_max%5D=&s%5Brate_from%5D=&s%5Brate_to%5D=&s%5Bzip%5D=24103&s%5Brad%5D=100&s%5Bt%5D=&s%5Bg%5D=a&s%5Bpw_min%5D=&s%5Bpw_max%5D=&s%5Bsince%5D=&s%5Bfuel%5D%5B%5D=7&s%5Bcu%5D=&button="
rawres2 <- GET(url=startlink2)
rawres2$status_code 
rawcontent2 <- httr::content(rawres2,as="text")

# Transformer text til html-nodes
page2 <- read_html(rawcontent2)

# Hent bil-elementer fra startside
carlist2 <- page2 %>% html_elements("article")

# tag-liste
Modeltag <- ".provider-link click-out offer-click-out offer-click-out-carwow_4789825029"
Pricetag <- ".purchase-price ml-3 h1"
Producedtag <- ".column medium-4 text-md mt-half reg_year"
KMdriventag <- ".column medium-4 text-md mt-half mileage"
Topspeedtag <- ".column medium-4 text-md mt-half power"
Consumptiontag <-".column medium-4 text-md mt-half consumption"
Locationtag <- ".column medium-4 text-md mt-half location"


# Dataframe til opsamling
cn2 <- c("price,","Produced","model", "KMdriven","Topspeed","Consumption","location","link","carid","scrapedate")
colldf2 <- as.data.frame(matrix(data=NA,nrow = 0,ncol = 10))
colnames(colldf2)=cn

# Løkke gennem bil-elementer og udtræk data
for (car in carlist) {
  price <- tryCatch(car %>% html_element(ptag) %>% html_text(trim = TRUE), error = function(e) NA)
  props <- tryCatch(car %>% html_element(proptag) %>% html_text(trim = TRUE), error = function(e) NA)
  makemodel <- tryCatch(car %>% html_element(mmtag) %>% html_text(trim = TRUE), error = function(e) NA)
  details <- tryCatch(car %>% html_elements(dettagitem) %>% html_text(trim = TRUE) %>% paste0(collapse = "_"), error = function(e) NA)
  description <- tryCatch(car %>% html_element(desctag) %>% html_text(trim = TRUE), error = function(e) NA)
  location <- tryCatch(car %>% html_element(loctag) %>% html_text(trim = TRUE), error = function(e) NA)
  link <- tryCatch(car %>% html_element("a") %>% html_attr("href"), error = function(e) NA)
  carid <- tryCatch(str_extract(link, "[0-9]{7}"), error = function(e) NA)
  scrapedate <- Sys.time()
  
  # Midlertidig data frame til indsamlede data
  tmpdf <- data.frame(price, props, makemodel, details, description, location, link, carid, scrapedate, stringsAsFactors = FALSE)
  
  # Tilføj til hoved dataframen
  colldf <- rbind(colldf, tmpdf)
}


print(colldf)

# Chattens hjælp
library(httr)
library(rvest)
library(dplyr)
library(stringr)

# Første side
startlink2 <- "https://www.12gebrauchtwagen.de/suchen?s%5Bmk%5D=63&s%5Bmd%5D=&s%5By_min%5D=&s%5By_max%5D=&s%5Bm_min%5D=&s%5Bm_max%5D=&s%5Bprice_or_rate%5D=price&s%5Bpr_min%5D=&s%5Bpr_max%5D=&s%5Brate_from%5D=&s%5Brate_to%5D=&s%5Bzip%5D=24103&s%5Brad%5D=100&s%5Bt%5D=&s%5Bg%5D=a&s%5Bpw_min%5D=&s%5Bpw_max%5D=&s%5Bsince%5D=&s%5Bfuel%5D%5B%5D=7&s%5Bcu%5D=&button="
rawres2 <- GET(url = startlink2)
rawres2$status_code
rawcontent2 <- httr::content(rawres2, as = "text")

# Transformer text til html-nodes
page2 <- read_html(rawcontent2)

# Hent bil-elementer fra startside
carlist2 <- page2 %>% html_elements("article")

# tag-liste
Modeltag2 <- ".provider-link.click-out.offer-click-out.offer-click-out-carwow_4789825029"
Pricetag2 <- ".purchase-price.ml-3.h1"
Producedtag2 <- ".column.medium-4.text-md.mt-half.reg_year"
KMdriventag2 <- ".column.medium-4.text-md.mt-half.mileage"
Topspeedtag2 <- ".column.medium-4.text-md.mt-half.power"
Consumptiontag2 <- ".column.medium-4.text-md.mt-half.consumption"
Locationtag2 <- ".column.medium-4.text-md.mt-half.location"

# Dataframe til opsamling
cn2 <- c("price", "produced", "model", "KMdriven", "topspeed", "consumption", "location", "link", "carid", "scrapedate")
colldf2 <- as.data.frame(matrix(data = NA, nrow = 0, ncol = length(cn2)))
colnames(colldf2) <- cn2

# Løkke gennem bil-elementer og udtræk data
for (car2 in carlist2) {
  price2 <- tryCatch(car2 %>% html_element(Pricetag2) %>% html_text(trim = TRUE), error = function(e) NA)
  produced2 <- tryCatch(car2 %>% html_element(Producedtag2) %>% html_text(trim = TRUE), error = function(e) NA)
  model2 <- tryCatch(car2 %>% html_element(Modeltag2) %>% html_text(trim = TRUE), error = function(e) NA)
  KMdriven2 <- tryCatch(car2 %>% html_element(KMdriventag2) %>% html_text(trim = TRUE), error = function(e) NA)
  topspeed2 <- tryCatch(car2 %>% html_element(Topspeedtag2) %>% html_text(trim = TRUE), error = function(e) NA)
  consumption2 <- tryCatch(car2 %>% html_element(Consumptiontag2) %>% html_text(trim = TRUE), error = function(e) NA)
  location2 <- tryCatch(car2 %>% html_element(Locationtag2) %>% html_text(trim = TRUE), error = function(e) NA)
  link2 <- tryCatch(car2 %>% html_element("a") %>% html_attr("href"), error = function(e) NA)
  carid2 <- tryCatch(str_extract(link2, "[0-9]{7}"), error = function(e) NA)
  scrapedate2 <- Sys.time()
  
  # Midlertidig data frame til indsamlede data
  tmpdf2 <- data.frame(price2, produced2, model2, KMdriven2, topspeed2, consumption2, location2, link2, carid2, scrapedate2, stringsAsFactors = FALSE)
  
  # Tilføj til hoved dataframen
  colldf2 <- rbind(colldf2, tmpdf2)
}

# Print resultatet
print(colldf2)


