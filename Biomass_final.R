# Load required libraries
library(allodb)
library(dplyr)
library(tidyr)
library(stringr)

csv_folder <- "G:/Waveform/All_Sites/Sites/New folder/"
output_folder <- "G:/Waveform/All_Sites/Sites/Output/"

csv_files <- list.files(csv_folder, pattern = "*.csv", full.names = TRUE)

combined_results <- data.frame(plotID = character(), Total_Biomass = numeric(),
                               Dead_Biomass = numeric(), Live_Biomass = numeric(),
                               Year = character(), Full_Name = character(),
                               stringsAsFactors = FALSE)

for (csv_file in csv_files) {
  data <- read.csv(csv_file)
  
  year <- str_extract(basename(csv_file), "\\d+")
  
  results <- data %>%
    group_by(plotID) %>%
    mutate(
      Biomass = get_biomass(dbh = stemDiameter, genus = Genus, species = Species, coords = c(46.0, -89.0)),
      Dead_Biomass = sum(ifelse(grepl("Dead", plantStatus, ignore.case = TRUE), Biomass, 0), na.rm = TRUE),
      Live_Biomass = sum(ifelse(!grepl("Dead", plantStatus, ignore.case = TRUE), Biomass, 0), na.rm = TRUE)
    ) %>%
    summarise(
      Total_Biomass = sum(Biomass, na.rm = TRUE),
      Dead_Biomass = sum(Dead_Biomass, na.rm = TRUE),
      Live_Biomass = sum(Live_Biomass, na.rm = TRUE)
    ) %>%
    na.omit() 
  
  results <- results %>%
    mutate(
      Year = year,
      Full_Name = paste(plotID, year, sep = "_")
    )
  
  combined_results <- bind_rows(combined_results, results)
  
  cat("Processed", csv_file, "\n")
}

combined_results <- combined_results[order(combined_results$plotID), ]

output_file <- paste0(output_folder, "combined_results_live_Dead.csv")
write.csv(combined_results, file = output_file, row.names = FALSE)

cat("Combined results saved in", output_file, "\n")







# Load required libraries
library(allodb)
library(dplyr)
library(tidyr)
library(stringr)

csv_folder <- "G:/Waveform/All_Sites/Sites/"
output_folder <- "G:/Waveform/All_Sites/Sites/Output/"

csv_files <- list.files(csv_folder, pattern = "*.csv", full.names = TRUE)

combined_results <- data.frame(plotID = character(), Total_Biomass = numeric(),
                               Dead_Biomass = numeric(), Live_Biomass = numeric(),
                               Year = character(), Full_Name = character(),
                               stringsAsFactors = FALSE)

for (csv_file in csv_files) {
  data <- read.csv(csv_file)
  
  year <- str_extract(basename(csv_file), "\\d+")
  
  results <- data %>%
    group_by(plotID) %>%
    mutate(
      Biomass = get_biomass(dbh = stemDiameter, genus = Genus, species = Species, coords = c(46.0, -89.0)),
      Dead_Biomass = sum(ifelse(grepl("Dead", plantStatus, ignore.case = TRUE), Biomass, 0), na.rm = TRUE)
    ) %>%
    summarise(
      Total_Biomass = sum(Biomass, na.rm = TRUE),
      Dead_Biomass = sum(Dead_Biomass, na.rm = TRUE),
      Live_Biomass = Total_Biomass - Dead_Biomass
    ) %>%
    na.omit() 
  
  results <- results %>%
    mutate(
      Year = year,
      Full_Name = paste(plotID, year, sep = "_")
    )
  
  combined_results <- bind_rows(combined_results, results)
  
  cat("Processed", csv_file, "\n")
}

combined_results <- combined_results[order(combined_results$plotID), ]

output_file <- paste0(output_folder, "combined_results_live_Dead_Final.csv")
write.csv(combined_results, file = output_file, row.names = FALSE)

cat("Combined results saved in", output_file, "\n")
