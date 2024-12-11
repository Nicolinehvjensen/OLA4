# Initialiser en liste til at gemme de gennemsnitlige R² værdier og standardafvigelser for hver iteration
combined_data2 <- combined_data
r_squared_summary <- list()

# Gentag processen, indtil der er 59 rækker tilbage
while (nrow(combined_data2) > 59) {
  
  # Initialiser en vektor til at gemme R² værdier for den aktuelle iteration
  opt_r_squared_values <- numeric(ncol(combined_data2)-1)
  combination_names <- names(combined_data2)[-1]                      # Navne på kombinationerne
  
  # Loop gennem hver kombination (starter fra kolonne 2, da kolonne 1 er Realvækst)
  for (i in 2:ncol(combined_data2)) {
    # Skab en lineær model med Realvækst som afhængig variabel og hver kolonne som uafhængig variabel
    opt_model <- lm(combined_data2$Realvækst ~ combined_data2[, i])
    
    # Gem R²-værdien i vektoren på index i-1 (for at tilpasse indekseringen)
    opt_r_squared_values[i-1] <- summary(opt_model)$r.squared
  }
  
  # Tilføj R²-værdierne for den aktuelle iteration til listen
  r_squared_summary[[length(r_squared_summary) + 1]] <- opt_r_squared_values
  
  # Fjern den sidste række fra combined_data til næste iteration
  combined_data2 <- combined_data2[-nrow(combined_data2), ]
}

# Saml alle iterationers R² statistikker i en enkelt dataframe
r_squared_summary_df <- as.data.frame(r_squared_summary)
row.names(r_squared_summary_df) <- combination_names
numn_columns <- ncol(r_squared_summary_df)
newcolumnnames <- paste0("kvartal", seq(from=99, by=-1, length.out = numn_columns))
colnames(r_squared_summary_df) <- newcolumnnames

# Udskriv det endelige datasæt
print(r_squared_summary_df)

r_squared_summary_df$Mean_R_squared <- rowMeans(r_squared_summary_df, na.rm = TRUE)
r_squared_summary_df$SD_R_squared <- apply(r_squared_summary_df, 1, sd, na.rm = TRUE)
r_squared_summary_df$Index_Rank <- r_squared_summary_df$Mean_R_squared / r_squared_summary_df$SD_R_squared
