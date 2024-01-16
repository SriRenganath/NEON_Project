# Load required libraries
library(allodb)
library(dplyr)
library(tidyr)
library(stringr)

csv_folder <- "G:/Waveform/All_Sites/Final_GTD/"
output_folder <- "G:/Waveform/All_Sites/Biomass/"

csv_files <- list.files(csv_folder, pattern = "*.csv", full.names = TRUE)

combined_results <- data.frame(Full_Name = character(), 
                               Total_Biomass = numeric(),
                               Dead_Biomass = numeric(),
                               Live_Biomass = numeric(),  # New column
                               stringsAsFactors = FALSE)

for (csv_file in csv_files) {
  data <- read.csv(csv_file)
  
  year <- str_extract(basename(csv_file), "\\d+")
  
  # Calculate Total Biomass
  total_results <- data %>%
    group_by(Full_Name = tools::file_path_sans_ext(basename(csv_file))) %>%
    summarise(
      Total_Biomass = sum(get_biomass(dbh = stemDiameter, genus = Genus, species = Species, coords = c(46.0, -89.0)), na.rm = TRUE)
    ) %>%
    na.omit() 
  
  # Calculate Dead Biomass
  dead_results <- data %>%
    group_by(Full_Name = tools::file_path_sans_ext(basename(csv_file))) %>%
    summarise(
      Dead_Biomass = sum(case_when(
        grepl("Dead, broken bole|Standing dead", plantStatus, ignore.case = TRUE) ~
          get_biomass(dbh = stemDiameter, genus = Genus, species = Species, coords = c(46.0, -89.0)),
        TRUE ~ 0
      ))
    ) %>%
    na.omit() 
  
  # Calculate Live Biomass by subtracting Dead Biomass from Total Biomass
  live_results <- total_results %>%
    left_join(dead_results, by = "Full_Name") %>%
    mutate(
      Live_Biomass = Total_Biomass - Dead_Biomass
    )
  
  # Combine results for each CSV file
  combined_results <- bind_rows(combined_results, live_results)
  
  cat("Processed", csv_file, "\n")
}

combined_results <- combined_results %>%
  group_by(Full_Name) %>%
  summarise(
    Total_Biomass = sum(Total_Biomass, na.rm = TRUE),
    Dead_Biomass = sum(Dead_Biomass, na.rm = TRUE),
    Live_Biomass = sum(Live_Biomass, na.rm = TRUE)  # Sum the Live_Biomass
  ) %>%
  na.omit()

combined_results <- combined_results[order(combined_results$Full_Name), ]

output_file <- file.path(output_folder, "Final_results.csv")
write.csv(combined_results, file = output_file, row.names = FALSE)

cat("Combined results saved in", output_file, "\n")
