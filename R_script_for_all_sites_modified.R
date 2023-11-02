library(neonUtilities)
library(neonOS)
library(geoNEON)
library(tidyr)
library(openxlsx)

# Define the sites and years
site_years <- list(
  DELA = c(2015, 2016, 2017, 2018, 2019, 2021),
  LENO = c(2015, 2016, 2017, 2018, 2019, 2021),
  TALL = c(2015, 2016, 2017, 2018, 2019, 2021),
  BONA = c(2017, 2021),
  DEJU = c(2017, 2019, 2021),
  HEAL = c(2017, 2021),
  SRER = c(2017, 2019, 2021),
  SJER = c(2019),
  SOAP = c(2019, 2021),
  TEAK = c(2021),
  CPER = c(2017),
  NIWO = c(2019, 2020),
  RMNP = c(2017, 2018, 2020, 2022),
  DSNY = c(2018),  #(All years 2014, 2016, 2017, 2018)
  #JERC = c(2020), (All years 2014, 2016, 2017, 2018, 2019, 2021)
  BART = c(2016, 2017, 2022), #(All years 2014, 2016, 2017, 2018, 2019, 2022)
  OSBS = c(2014, 2016, 2017, 2018, 2019, 2021),
  PUUM = c(2019, 2020),
  KONZ = c(2016, 2017, 2018, 2019, 2020),
  UKFS = c(2016, 2017, 2018, 2019, 2020),
  SERC = c(2016, 2017, 2019, 2021, 2022),
  HARV = c(2014, 2016, 2017, 2018, 2019, 2022),
  UNDE = c(2016, 2017, 2019, 2020, 2022),
  JORN = c(2018, 2019, 2021),
  GUAN = c(2018),
  GRSM = c(2015, 2016, 2017, 2018, 2021, 2022),
  ORNL = c(2015, 2016, 2017, 2018),
  CLBJ = c(2016, 2017, 2018, 2019, 2021, 2022),
  MOAB = c(2018, 2021, 2022),
  ONAQ = c(2017, 2019, 2022),
  BLAN = c(2016, 2017, 2019, 2021, 2022),
  MLBS = c(2015, 2017, 2018, 2021),
  SCBI = c(2016, 2017, 2019, 2021, 2022),
  ABBY = c(2017, 2018, 2019, 2021),
  WREF = c(2017, 2019, 2021),
  STEI = c(2016, 2017, 2019, 2020, 2022),
  TREE = c(2016, 2017, 2019, 2020, 2022),
  YELL = c(2018, 2019, 2020)
)

for (site in names(site_years)) {
  years <- site_years[[site]]
  
  for (year in years) {
    veglist <- loadByProduct(dpID = "DP1.10098.001",
                             site = site,
                             startdate = paste0(year, "-01"),
                             enddate = paste0(year, "-12"),
                             package = "basic",
                             check.size = FALSE)
    
    # Calculate individual tree locations (Link: https://data.neonscience.org/data-products/DP1.10098.001; NEON User Guide to the Vegetation Structure Data Product (NEON.DP1.10098.001); Page 15 of 31)
    vst.loc <- getLocTOS(data = veglist$vst_mappingandtagging,
                         dataProd = "vst_mappingandtagging")
    
    veg <- joinTableNEON(veglist$vst_apparentindividual,
                         vst.loc,
                         name1 = "vst_apparentindividual",
                         name2 = "vst_mappingandtagging")
    
    specific_columns <- veg[c("individualID", "scientificName", "date.x", "adjDecimalLatitude", "adjDecimalLongitude", "stemDiameter", "date.y", "eventID.y", "namedLocation", "plantStatus")]
    
    veg_filtered <- specific_columns[complete.cases(specific_columns$adjDecimalLatitude, specific_columns$adjDecimalLongitude, specific_columns$stemDiameter), ]
    
    separated_names <- separate(veg_filtered, scientificName, into = c("Genus", "Species"), sep = " ")
    
    veg_filtered <- cbind(separated_names[, c("Genus", "Species")], veg_filtered[, c("individualID", "date.x", "adjDecimalLatitude", "adjDecimalLongitude", "stemDiameter", "date.y", "eventID.y", "namedLocation", "plantStatus")])
    
    colnames(veg_filtered)[colnames(veg_filtered) == "date.x"] <- "Date_coordinates"
    colnames(veg_filtered)[colnames(veg_filtered) == "date.y"] <- "Date_vegstructure"
    colnames(veg_filtered)[colnames(veg_filtered) == "eventID.y"] <- "eventID"
    
    data_perplot <- veglist$vst_perplotperyear
    
    merged_data <- merge(veg_filtered, data_perplot[, c("eventID", "plotType")], by = "eventID", all.x = TRUE)
    
    merged_data <- unique(merged_data)
    
    Final_filtered <- merged_data[complete.cases(merged_data$plotType), ]
    Final_filtered <- Final_filtered[, !(names(Final_filtered) %in% "eventID")]
    
    Final_filtered <- Final_filtered[!duplicated(Final_filtered$individualID), ]
    
    Final_filtered$namedLocation <- gsub("\\.basePlot\\.vst", "", Final_filtered$namedLocation)
    colnames(Final_filtered)[colnames(Final_filtered) == "namedLocation"] <- "plotID"
    
    file_name <- paste(site, year, sep = "_")
    file_path <- paste("G:/Waveform/All_Sites/Sites/New folder", file_name, ".csv", sep = "")
    
    write.csv(Final_filtered, file_path, row.names = FALSE)
  }
}
