library(lubridate) # Date manipulation
library(tidyverse) # Collection of packages, including dplyr, purrr, ggplot2, and stringr, used for data manipulation and plotting; my most heavily used package
library(stringr) # Used for string manipulation (e.g., str_detect(), str_replace())
library(janitor) # Cleaning column names and removing empty rows/columns
library(readxl) # Used for importing Excel files (read_excel)

library(adehabitatHR)
library(raster)
library(dplyr)

#Read in the data. Needs to be specific to file pathway (i.e may change with device)
#Kaeli's mac path
Chew_card_data <- read_excel("/Users/kaeliswift/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Rat Chew Card Study/Data/Chew card data.xlsx")
#Chew_card_data <- read_excel("C:/Beth/Students/Kaeli/Chew card data.xlsx")

#Create a summary table by site 
#step 1...turn yes/no/unknown into integer format
Chew_card_data <- Chew_card_data %>%
  mutate(across(c(`C1 Rat detected`, `C2 Rat detected`, `T Rat detected`), 
                ~ case_when(
                  . == "Yes"     ~ 1,
                  . == "No"      ~ 0,
                  . == "Unknown" ~ NA_real_,
                  TRUE           ~ NA_real_
                )))

#step 2
#Generating an overall detection columns 
Chew_card_data <- Chew_card_data %>%
  mutate(S1 = case_when(
    # Sites with daily checks (3 detection columns)
    Site %in% c("D", "E", "F") ~ as.integer(`C1 Rat detected`),
    
    # Sites with only terminal check (1 detection column)
    Site %in% c("A", "B") ~ as.integer(`T Rat detected`),
    
    # Default case (e.g., missing site code)
    TRUE ~ NA_integer_
  ))  %>%
  mutate(S2 = case_when(
    # Sites with daily checks (3 detection columns)
    Site %in% c("D", "E", "F") ~ as.integer(`C2 Rat detected`),
    
    # Default case (e.g., missing site code)
    TRUE ~ NA_integer_
  )) %>%  
  
  mutate(S3 = case_when(
    # Sites with daily checks (3 detection columns)
    Site %in% c("D", "E", "F") ~ as.integer(`T Rat detected`),
    
    # Default case (e.g., missing site code)
    TRUE ~ NA_integer_
  ))




#step 3
#Generating effort data
Chew_card_data <- Chew_card_data %>%
  mutate(E1 = case_when(
    # Sites with daily checks (3 detection columns)
    Site %in% c("D", "E", "F") ~ 1,
    
    # Sites with only terminal check (1 detection column)
    Site %in% c("A") ~ 5,
    Site %in% c("B") ~ 3,
    # Default case (e.g., missing site code)
    TRUE ~ NA_integer_
  ))  %>%
  mutate(E2 = case_when(
    # Sites with daily checks (3 detection columns)
    Site %in% c("D", "E", "F") ~ 1,
    
    # Default case (e.g., missing site code)
    TRUE ~ NA_integer_
  )) %>%  
  
  mutate(E3 = case_when(
    # Sites with daily checks (3 detection columns)
    Site %in% c("D", "E", "F") ~ 1,
    
    # Default case (e.g., missing site code)
    TRUE ~ NA_integer_
  ))





#Step 3
#Creating a new column that groups sites by habitat type 
Chew_card_data <- Chew_card_data %>%
  mutate(Habitat_Type = case_when(
    Site %in% c("A", "E") ~ "Secondary\nforest",
    Site %in% c("B", "F") ~ "Tangan\nforest",
    Site == c("D") ~ "Native\nforest",
    TRUE                  ~ NA_character_  # For any unexpected site codes
  ))

#checks that that worked
table(Chew_card_data$Habitat_Type)

library(dplyr)
library(unmarked)


# Establish maximum number of surveys = 3 (C1, C2, T)
#n_surveys <- 3

det_hist <- as.matrix(cbind(Chew_card_data$S1, Chew_card_data$S2, Chew_card_data$S3))
effort <- as.matrix(cbind(Chew_card_data$E1, Chew_card_data$E2, Chew_card_data$E3))

rowSums(det_hist, na.rm=T)

effort_s=matrix(scale(c(effort)), ncol=3)

habitat <- as.vector(as.factor(Chew_card_data$Habitat_Type))


#Create unmarkedFrameOccu object
umf <- unmarkedFrameOccu(y = det_hist, siteCovs = data.frame(habitat=habitat),obsCovs = list(effort = effort_s) )

#Fit null occupancy model
null <- occu(~ 1 ~ 1, data = umf)

# Step 6: Summary of results
summary(null)


#Fit Effort on occupancy model
EffDetc <- occu(~ effort ~ 1, data = umf)

# Step 6: Summary of results
summary(EffDetc)

#Fit effort+habitat on occupancy model
EffHabDect<- occu(~ effort +habitat ~ 1, data = umf)

# Step 6: Summary of results
summary(EffHabDect)

#Fit effort+habitat occupancy and habitat on detection model
EffHabxHab <- occu(~ effort + habitat ~ habitat, data = umf)

# Step 6: Summary of results
summary(EffHabxHab)


#Compare models 
library(MuMIn)
ms <- model.sel(null, EffOccu, EffHabOccu, EffHabxHab)

ms_out <- ms[, c("df", "AICc", "delta", "weight")]
round(ms_out, 3)


#####Create a 2 day encounter history to mimic earlier estimates

Chew_card_data <- Chew_card_data %>% 
  filter(Site %in% c("D", "E", "F"))


det_hist <- as.matrix(cbind(Chew_card_data$S1, Chew_card_data$S2))
table(rowSums(det_hist))
habitat <- as.vector(as.factor(Chew_card_data$Habitat_Type))
umf <- unmarkedFrameOccu(y = det_hist, siteCovs = data.frame(habitat=habitat) )

#Fit null occupancy model
model.test <- occu(~ habitat ~ habitat, data = umf)

