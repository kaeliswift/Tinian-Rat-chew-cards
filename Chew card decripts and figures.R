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
#Quick check to see if that worked. Since site "A" is still in the mix which was only a single check, C1 and C2 fields will be NA
head(Chew_card_data[, c("Site", "C1 Rat detected", "C2 Rat detected", "T Rat detected")])

#step 2
#Generating an overall detection column 
Chew_card_data <- Chew_card_data %>%
  mutate(Detected = case_when(
    # Sites with daily checks (3 detection columns)
    Site %in% c("D", "E", "F") ~ as.integer(
      (`C1 Rat detected` == 1 | `C2 Rat detected` == 1 | `T Rat detected` == 1)
    ),
    
    # Sites with only terminal check (1 detection column)
    Site %in% c("A", "B") ~ as.integer(`T Rat detected` == 1),
    
    # Default case (e.g., missing site code)
    TRUE ~ NA_integer_
  ))

#Step 3 generate output summary table by site 
#Create output summary with overall detection (=occupancy) rate by habitat type
summarySite_df <- Chew_card_data %>%
  group_by(Site) %>%
  dplyr::summarize(
    Sites = n(),
    Detections = sum(Detected, na.rm = TRUE),
    Detection_Rate = mean(Detected, na.rm = TRUE),
    Occupied_Sites = sum(Detections),
    Occupancy_Rate = (Detections/Sites)
  )

#run summary output
summarySite_df


#Creating a new column that groups sites by habitat type 
Chew_card_data <- Chew_card_data %>%
  mutate(Habitat_Type = case_when(
    Site %in% c("E") ~ "Secondary\nforest",
    Site %in% c("B", "F") ~ "Tangan\nforest",
    Site == c("D") ~ "Native\nforest",
    TRUE                  ~ NA_character_  # For any unexpected site codes
  ))



#GENERAL DESCRIPTIVES FOR All sites (can uncomment subsequent lines for all but A)
#OMIT SITE A since it's check schedule was 5 days instead of 3. 
#Chew_card_data <- Chew_card_data %>% filter(Site != "A")

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
unique(Chew_card_data$Habitat_Type)

#Step 4
# Create summary table
summary_df <- Chew_card_data %>%
  group_by(Habitat_Type) %>%
  summarize(
    Sites = n(),
    Detections = sum(Detected, na.rm = TRUE),
    Detection_Rate = mean(Detected, na.rm = TRUE),
    Occupied_Sites = sum(Detections),
    Occupancy_Rate = (Detections/Sites)
  )

summary_df 

# Plot detection rate
ggplot(summary_df, aes(x = Habitat_Type, y = Detection_Rate)) +
  geom_col(fill = "darkseagreen4") +
  ylim(0, 1) +
  labs(
    title = "Overall Detection Rate by Habitat Type",
    x = "Habitat Type",
    y = "Detection Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    text = element_text(family = "Times New Roman")  # Optional: use your desired font
  )


#Plot naive Occupancy rate for all sites surveyed 
ggplot(summary_df, aes(x = Habitat_Type, y = Occupancy_Rate)) +
  geom_col(fill = "darkseagreen4") +
  ylim(0, 1) +
  labs(
    title = "Naive Occupancy Rate by Habitat Type",
    x = "Habitat Type",
    y = "Occupancy Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    text = element_text(family = "Times New Roman")  # Optional: use your desired font
  )