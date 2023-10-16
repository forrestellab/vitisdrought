drought_df<-read.csv("DROUGHT_CORR_mean.csv")

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

 write.csv(formatted_output_df,
          "data_output/correlation_drought_final_mean.csv",
           row.names = TRUE)

 
 

# control -----------------------------------------------------------------

 
 
 control_df<-read.csv("CONTROL_COR_mean.csv")
 
 correlation_matrix <- cor(control_df, method = "pearson")
 
 
 # Get the variable names
 variable_names <- colnames(correlation_matrix)
 
 # Initialize an empty matrix for storing p-values
 p_values <- matrix(NA, ncol(correlation_matrix), ncol(correlation_matrix))
 
 # Loop through columns and calculate p-values
 for (i in 1:ncol(correlation_matrix)) {
   for (j in 1:ncol(correlation_matrix)) {
     if (i != j) {
       p_values[i, j] <- cor.test( control_df[, i],  control_df[, j])$p.value
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
 
 write.csv(formatted_output_df,
           "data_output/correlation_control_final_mean.csv",
           row.names = TRUE)
 
 