WP<- read.csv("data_output/dates_merged/WP_middle")
WP<- read.csv("data_output/dates_merged/WP_final")
licor <-read.csv("data_output/dates_merged/licor_middle")
licor<-licor <-read.csv("data_output/dates_merged/licor_final")
seg<- read.csv("data_output/segmentation/segmentation_data.csv")
geo_df <- read.csv("data_output/Geo_Data/bio_table1")
geo_df$Species[geo_df $Genotype == "TXNM0821"] <- "hybrid"

geo_df<- geo_df %>% 
          select(!c("Sampling.Source"))

  
WP_mean<- WP  %>% 
            select(!c("ID", "LWP.area", "SWP.area", "leafwp_predawn", "SWP", "PD.area" )) %>% 
            group_by(Treatment, Species, Genotype) %>%
            summarize(LWP = mean(LWP, na.rm = TRUE), PD = mean(PD, na.rm = TRUE))


licor_mean<- licor %>% 
               select(!c("rep", "Tleaf"))%>% 
                group_by(Treatment, Species, Genotype) %>%
                summarize(A = mean(A, na.rm = TRUE), Ci = mean(Ci, na.rm = TRUE), gsw = mean(gsw, na.rm = TRUE), 
                          E = mean(E, na.rm = TRUE), intrWUE_AE = mean(intrWUE_AE, na.rm = TRUE), 
                          instWUE_AGs = mean(instWUE_AGs, na.rm = TRUE))

seg_mean<- seg%>% 
              select(!c("species_geno", "mean_Porosity", "sem_Porosity", "mean_SpongyV_TotalMesophyllV",
                            "sem_SpongyV_TotalMesophyllV", "mean_PalisadeV_TotalMesophyllV", 
                            "sem_PalisadeV_TotalMesophyllV", "mean_SpongyV_PalisadeV", 
                            "sem_SpongyV_PalisadeV", "mean_Leaf_Width_um", "sem_Leaf_Width_um",
                            "mean_Mesophyll_Width_um",  
                            "sem_Mesophyll_Width_um", "p_value_Porosity", 
                            "p_value_SpongyV_TotalMesophyllV",   
                            "p_value_PalisadeV_TotalMesophyllV", "p_value_SpongyV_PalisadeV",
                            "p_value_Leaf_Width_um","p_value_Mesophyll_Width_um" ))%>% 
              group_by(Treatment, Species, Genotype) %>%
              summarize(Porosity = mean(Porosity, na.rm = TRUE), 
                        SpongyV_TotalMesophyllV = mean(SpongyV_TotalMesophyllV, na.rm = TRUE), 
                        PalisadeV_TotalMesophyllV = mean(PalisadeV_TotalMesophyllV, na.rm = TRUE), 
                        SpongyV_PalisadeV = mean(SpongyV_PalisadeV, na.rm = TRUE), 
                        Leaf_Width_um = mean(Leaf_Width_um, na.rm = TRUE), 
                        Mesophyll_Width_um = mean(Mesophyll_Width_um, na.rm = TRUE))

merged_df <- merge(merge(seg_mean, licor_mean, by = c("Species", "Genotype", "Treatment")),
                   WP_mean, by = c("Species", "Genotype", "Treatment"))

merged_df <- merge(merged_df, geo_df, by = c("Species", "Genotype"))

drought_df <- merged_df %>%
              filter(Treatment == "Drought")%>% 
              select(!c(Treatment, Species, Genotype))
control_df <- merged_df %>%
              filter(Treatment == "Control")%>% 
              select(!c(Treatment, Species, Genotype))


# new ---------------------------------------------------------------------

library(pander)


# Calculate the correlation matrix
correlation_matrix <- cor(drought_df, method = "pearson")

# Get the variable names
variable_names <- colnames(correlation_matrix)

# Initialize an empty matrix for storing p-values
p_values <- matrix(NA, ncol(correlation_matrix), ncol(correlation_matrix))

# Loop through columns and calculate p-values
for (i in 1:ncol(correlation_matrix)) {
  for (j in 1:ncol(correlation_matrix)) {
    if (i != j) {
      p_values[i, j] <- cor.test(drought_df[, i], drought_df[, j])$p.value
    }
  }
}

# Initialize an empty matrix for formatted output
formatted_output <- matrix("", ncol(correlation_matrix), ncol(correlation_matrix))

# Loop through the correlation matrix and p-values matrix
for (i in 1:ncol(correlation_matrix)) {
  for (j in 1:ncol(correlation_matrix)) {
    if (i != j) {
      formatted_output[i, j] <- ifelse(p_values[i, j] < 0.05,
                                       paste0("**", round(correlation_matrix[i, j], 2), "**"),
                                       round(correlation_matrix[i, j], 2))
    }
  }
}

# Convert the matrix to a data frame
formatted_output_df <- as.data.frame(formatted_output)
rownames(formatted_output_df) <- variable_names
colnames(formatted_output_df) <- variable_names

# Display the formatted output
print(formatted_output_df)
# 
# 
# write.csv(formatted_output_df,
#           "correlation_drought_middle.csv",
#           row.names = TRUE)
# write.csv(formatted_output_df,
#           "correlation_drought_final.csv",
#           row.names = TRUE)