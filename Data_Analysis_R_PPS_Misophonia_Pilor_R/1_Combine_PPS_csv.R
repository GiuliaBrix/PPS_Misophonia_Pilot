library(dplyr)
library(purrr)

# Set the path to your folder containing CSV files
folder_path <- "Misophonia_PPS_data"

# Get list of all CSV files in the folder
csv_files <- list.files(path = folder_path, 
                        pattern = "*.csv", 
                        recursive = TRUE,
                        full.names = TRUE)

# Function to process a single file
process_file <- function(path) {
  # Extract information from filenames
  file_name <- basename(path)
  group <- ifelse(grepl("HC", file_name), "HC", "MS")
  
  # Modified ID extraction for format like "HC_FABBRI60_TRIGGER_2.csv"
  id <- sub("^[A-Z]+_([A-Z0-9]+)_(TRIGGER|NEUTRAL).*", "\\1", file_name)
  
  stimulus <- ifelse(grepl("TRIGGER", file_name), "TRIGGER", "NEUTRAL")
  block <- ifelse(grepl("_2", file_name), 2, 1)
  
  # Read and process the file
  tryCatch({
    df <- read.csv2(path, dec = ".", header = TRUE, sep = ",")
    
    # Convert reaction.time to numeric, setting strings to NA
    df$reaction.time <- suppressWarnings(as.numeric(df$reaction.time))
    
    # Convert angle to numeric, setting strings to NA
    df$angle <- suppressWarnings(as.numeric(df$angle))
    
    # Add metadata columns
    df <- df %>%
      mutate(Group = group,
             ID = id,
             Stimulus = stimulus,
             Block = block,
             FileName = file_name)
    
    return(df)
  }, error = function(e) {
    warning(paste("Error processing file:", file_name, "\nError:", e))
    return(NULL)
  })
}

# Process all files and combine into one dataset
combined_df <- csv_files %>%
  map_df(process_file, .id = "file_index")

# Verify that "condition" does not contains numbers
print("Unique values in 'condition' column:")
print(unique(combined_df$condition))

#How may rows for each participant I have?
combined_df %>%
  group_by(ID) %>%
  summarise(n = n())

# Display summary of NA values
na_rt_count <- sum(is.na(combined_df$reaction.time))
print(paste("Number of NA values in reaction time:", na_rt_count))

# Check unique IDs to verify extraction
print("Unique IDs found:")
print(unique(combined_df$ID))

# Save the combined dataset
write.csv(combined_df, 
          file = paste0(folder_path, "/Final_PPS_df.csv"), 
          row.names = FALSE)

# Display summary of the combined dataset
print(paste("Total number of files processed:", length(csv_files)))

#names of the variables
names(combined_df)

# Select relevant columns for PPS analysis
df_PPS <- combined_df %>%
  select(ID, Group, Stimulus, Block, condition, reaction.time, trigger.time)

#Save final dataset df_PPS 
write.csv(df_PPS, 
          file = paste0(folder_path, "/Final_PPS_df.csv"), 
          row.names = FALSE)


