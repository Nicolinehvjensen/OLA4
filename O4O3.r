###Opgave 3 OLA 4

##Indhentning af accesslogs 

log0 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log")
log1 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.1")
log2 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.2")
log2_2 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.2 2")
log3 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.3")
log4 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.4")
log5 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.5")
log6 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.6")
log7 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.7")
log8 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.8")
log9 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.9")
log10 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.10")
log11 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.11")
log12 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.12")
log13 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.13")
log14 <- read.table("/Users/nicolinehoffmannvoghjensen/Downloads/log 4/access.log.14")

#Liste over samlede logs
log_list <- list(log0, log1, log2, log2_2, log3, log4, log5, log6, log7, log8, log9, log10, log11, log12, log13, log14)

#Omdanner listen af logs til dataframe 
combinedlogs <- do.call(rbind, log_list)

#Laver kolonnenavne til df
colnames(combinedlogs) <- c("IP", "Identity", "User", "Time", "Timezone", "Request", "Status", "Bytes", "HTTP referrer","Useragent")

#Gemmer logs som CSV for lettere at kunne dele 
write.csv(combinedlogs, "combinedlogs.csv", row.names = FALSE)

# Beregn startdato, slutdato og varighed
start_date <- as.Date(min(combinedlogs$Date), origin = "1970-01-01")
end_date <- as.Date(max(combinedlogs$Date), origin = "1970-01-01")
log_period_length <- as.numeric(end_date - start_date + 1)

#367 dage 

##Optælling af mest aktive IP-adresser 

#Regex til at finde dato - Matcher formatet [dd/mmm/yyyy:hh:mm:ss] og beholder kun dd/mmm/yyyy
combinedlogs$Date <- sub("\\[(\\d+/\\w+/\\d+):.*", "\\1", combinedlogs$Time)

#Omdanner til datoformat
combinedlogs$Date <- as.Date(combinedlogs$Date, format = "%d/%b/%Y")

#Tabel over IP adresser og datoer
ipcounts <- as.data.frame(table(combinedlogs$IP, combinedlogs$Date))

#Omdøber kolonner 
colnames(ipcounts) <- c("IP", "Date", "Requests")

#IP 5.179.80.205 har været mest aktiv 31/10/23 med 1147 requests

system("whois 5.179.80.205")

#AArhus Business College netværk 

##Optælling af unikke IP'er pr døgn

# Tæl unikke IP'er pr. dato
aktivip <- aggregate(IP ~ Date, data = combinedlogs, FUN = function(x) length(unique(x)))

# Omdøb kolonnerne
colnames(aktivip) <- c("Date", "IP")

#Der er flest aktive IP'er den 02/11/23 - 246 

###Plots

##Plot over optælling af antal aktive IP'er pr døgn 
# Sørg for, at 'Date' er i datoformat
aktivip$Date <- as.Date(aktivip$Date, format = "%Y-%m-%d")

aktivip <- aktivip[order(aktivip$Date), ]

# Søjlediagram
barplot(aktivip$IP,
        names.arg = aktivip$Date,
        las = 2,                     # Roter x-aksens tekst for bedre læsbarhed
        cex.names = 0.8,             # Reducér størrelsen på datoerne
        col = "darkseagreen",
        main = "Flest aktive IP'er den 02.11.2023",  # Hovedtitel
        ylab = "Antal aktive IP'er pr. døgn",
        ylim = c(0, max(aktivip$IP) + 50))    # Udvid y-aksen lidt over maks

# Tilføj en undertitel
mtext("Kilde: Logdata analyseret fra webserver", side = 3, line = 0, adj = 0.5, cex = 0.9)

##Plot over de mest aktive IP'er pr logperiode

# Optæl det samlede antal requests pr. IP
ip_total <- aggregate(Requests ~ IP, data = ipcounts, sum)

# Sortér efter antal requests i faldende rækkefølge
ip_total <- ip_total[order(-ip_total$Requests), ]

# Vælg de 10 mest aktive IP'er
top_ips <- ip_total[1:10, ]

# Justér y-aksen for at sikre, at den går hele vejen op
y_limit <- max(top_ips$Requests) * 1.1  # Sæt y-aksen til at gå 10% over den højeste værdi

# Lav søjlediagram med forbedret y-akse
barplot_values <- barplot(top_ips$Requests,
                          names.arg = top_ips$IP,
                          las = 2,                     # Roter IP-adresserne
                          col = "darkseagreen",             # Farve på søjler
                          main = "10 mest aktive IP'er i logperioden",
                          ylim = c(0, y_limit),        # Y-aksen går op til højeste værdi + 10%
                          border = "black",            # Tydelige kanter
                          cex.axis = 0.8)              # Juster tekststørrelse på aksen

# Ryk y-aksens beskrivelse lidt til venstre
mtext("Antal requests", side = 2, line = 3, adj = 0.5, cex = 0.9)  # Juster position af ylab
mtext("IP -adresse 192.0.102.40 er mest aktiv med 4145 requests henover perioden", side = 3, line = 0, adj = 0.5, cex = 1.0)
mtext("Kilde: Logdata analyseret fra webserver", side = 3, line = -2, adj = 0.5, cex = 0.9)


# Tilføj værdier over søjlerne
text(x = barplot_values, 
     y = top_ips$Requests, 
     labels = top_ips$Requests, 
     pos = 3, cex = 0.8, col = "darkgreen")  # Tekst over søjler

###WHOIS INFO

#WHOIS info på mest aktive IP henover perioden 
# Sum requests pr. IP over hele perioden
ipcounts <- aggregate(Request ~ IP, data = combinedlogs, FUN = length)

# Find den mest aktive IP (med flest requests samlet over hele perioden)
most_active_ip <- ipcounts[which.max(ipcounts$Request), "IP"]

# Print den mest aktive IP
print(paste("Most active IP: ", most_active_ip))

# Kør WHOIS-kommandoen på den mest aktive IP
system(paste("whois", most_active_ip))

###Gruppering af 404 fejl 

#Filtrerer efter 404 fejl i status 
error404 <- combinedlogs[grepl("404", combinedlogs$Status), ]

#Grupperer og tæller 404 fejl pr ip adresse 
top404ip <- sort(table(error404$IP), decreasing = TRUE)[1:10]

print(top404ip)

system("whois 93.162.98.150")


#Filtrer på specifikt IP og statuskode 
specific_ip_error404 <- combinedlogs %>%
  filter(Status == "404", IP == "93.162.98.150")











