library(lubridate) # Date manipulation
library(tidyverse) # Collection of packages, including dplyr, purrr, ggplot2, and stringr, used for data manipulation and plotting; my most heavily used package
library(stringr) # Used for string manipulation (e.g., str_detect(), str_replace())
library(janitor) # Cleaning column names and removing empty rows/columns
library(readxl) # Used for importing Excel files (read_excel)

library(adehabitatHR)
library(raster)
library(dplyr)

#Kaeli's mac path
Chew_card_data <- read_excel("/Users/kaeliswift/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Rat Chew Card Study/Data/Chew card data.xlsx")


#CHECKING CAMERA TRAP DATA
CT_data <- Chew_card_data %>%
  dplyr::filter(`Camera trap deployed?` == "TRUE") %>%
  dplyr::filter(Site %in% c("B", "D", "E", "F")) %>%
  dplyr::select(Site, `Site name`, Detected, `Date of first contact on CT`)

#35 Camera trap cards were depredated. 32 of those camereas captures rats
