###Endnu en test men bagfra

subset_data <- combined_data[1:41, ]  

# Start med subset_data, som allerede indeholder 41 rækker (første kolonne er Realvækst)
rolling_subset_new <- subset_data  # Nyt navn for datasættet
r_squared_summary_new <- list()   # Nyt navn for listen til R²-værdier

# Gentag processen, indtil der er 2 rækker tilbage (stopper før én række)
iteration_count <- 1  # Debugging tæller
while (nrow(rolling_subset_new) > 2) {
  
  # Debug: Print iterationens størrelse
  cat("Iteration:", iteration_count, "Antal rækker:", nrow(rolling_subset_new), "\n")
  
  # Initialiser en vektor til at gemme R² værdier for den aktuelle iteration
  r_squared_values_iteration <- numeric(ncol(rolling_subset_new) - 1)
  combination_names_new <- names(rolling_subset_new)[-1]  # Navne på kombinationerne (spring Realvækst over)
  
  # Loop gennem hver kolonne undtagen kolonne 1 (Realvækst)
  for (i in 2:ncol(rolling_subset_new)) {
    # Debug: Print hvilken kolonne vi arbejder med
    cat("Kolonne:", i, "Navn:", names(rolling_subset_new)[i], "\n")
    
    # Skab en lineær model med Realvækst som afhængig variabel og kolonne i som uafhængig variabel
    model_iteration <- lm(rolling_subset_new$Realvækst ~ rolling_subset_new[, i])
    
    # Debug: Tjek om modellen opfører sig normalt
    r_squared_debug <- summary(model_iteration)$r.squared
    cat("R² værdi:", r_squared_debug, "\n")
    
    # Gem R²-værdien i vektoren på index i-1 (for at tilpasse indekseringen)
    r_squared_values_iteration[i - 1] <- r_squared_debug
  }
  
  # Tilføj R²-værdierne for den aktuelle iteration til listen
  r_squared_summary_new[[length(r_squared_summary_new) + 1]] <- r_squared_values_iteration
  
  # Fjern den sidste række fra rolling_subset_new til næste iteration
  rolling_subset_new <- rolling_subset_new[-nrow(rolling_subset_new), ]
  iteration_count <- iteration_count + 1  # Opdater debugging tæller
}

# Saml alle iterationers R² statistikker i en enkelt dataframe
r_squared_summary_df_new <- as.data.frame(r_squared_summary_new)
row.names(r_squared_summary_df_new) <- combination_names_new

# Omdøb kolonnerne til at angive det antal rækker, der blev analyseret
numn_columns_new <- ncol(r_squared_summary_df_new)
new_column_names <- paste0("kvartal_", seq(from=nrow(subset_data), by=-1, length.out=numn_columns_new))
colnames(r_squared_summary_df_new) <- new_column_names

# Beregn gennemsnit, standardafvigelse og indeksrangering
r_squared_summary_df_new$Mean_R_squared <- rowMeans(r_squared_summary_df_new, na.rm = TRUE)
r_squared_summary_df_new$SD_R_squared <- apply(r_squared_summary_df_new, 1, sd, na.rm = TRUE)
r_squared_summary_df_new$Index_Rank <- r_squared_summary_df_new$Mean_R_squared / r_squared_summary_df_new$SD_R_squared

# Udskriv det endelige datasæt
print(r_squared_summary_df_new)
