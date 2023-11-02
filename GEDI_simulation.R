# Load required libraries
library(rGEDI)
library(lidR)

# Function to simulate and save waveform data for a given LAS file
process_las_file <- function(las_file_path, output_folder) {
  # Read LAS file
  las_amazon <- readLAS(las_file_path)
  
  # Extract plot center geolocations
  xcenter_amazon <- mean(bbox(las_amazon)[1,])
  ycenter_amazon <- mean(bbox(las_amazon)[2,])
  
  # Define the output HDF5 file path (different for each LAS file)
  output_file <- file.path(output_folder, paste0(basename(las_file_path), ".h5"))
  
  # Simulate waveform
  wf_amazon <- gediWFSimulator(input = las_file_path, 
                               output = output_file,
                               coords = c(xcenter_amazon, ycenter_amazon))
  
  # Simulated waveforms: shot_number is incremental beginning from 0
  shot_number <- 0
  simulated_waveform_amazon <- getLevel1BWF(wf_amazon, shot_number)
  
  # Extracting time (rxwaveform) and elevation values using the @ operator
  rxwaveform <- simulated_waveform_amazon@dt$rxwaveform
  elevation <- simulated_waveform_amazon@dt$elevation
  
  # Creating a data frame to hold the extracted data
  simulated_data <- data.frame(time = rxwaveform, elevation = elevation)
  
  # Define the filename for the CSV file (same as the LAS file, but with a .csv extension)
  csv_file_name <- paste0(basename(las_file_path), ".csv")
  csv_file_path <- file.path(output_folder, csv_file_name)
  
  # Save the data frame to a CSV file
  write.csv(simulated_data, file = csv_file_path, row.names = FALSE)
  
  # Optional: Print a message to indicate the successful saving of data
  cat("Simulated waveform data for", las_file_path, "has been saved to", csv_file_path, "\n")
}

# Function to process all LAS files in a given folder and its subfolders
process_las_files_in_folder <- function(folder_path, output_folder) {
  las_files <- list.files(path = folder_path, pattern = "\\.las$", full.names = TRUE, recursive = TRUE)
  if (length(las_files) == 0) {
    cat("No LAS files found in", folder_path, "\n")
  } else {
    cat("Processing LAS files in", folder_path, "\n")
    for (las_file in las_files) {
      process_las_file(las_file, output_folder)
    }
  }
}

# Parent folder containing the subfolders with LAS files
parent_folder <- "G:/Waveform/3D_clouds/Lidar"

# Output folder where the CSV files will be saved
output_folder <- "G:/Waveform/3D_clouds/Waveform"

# Create the output folder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

# Get the list of subfolders (LiDAR_Square_plot folders) in the parent folder
subfolders <- list.dirs(parent_folder, recursive = FALSE)

# Process LAS files in each subfolder and save the results in the output_folder
for (subfolder in subfolders) {
  folder_path <- file.path(subfolder, "LiDAR_Square_plot")
  process_las_files_in_folder(folder_path, output_folder)
}
