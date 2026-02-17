library(lubridate) # Date manipulation
library(tidyverse) # Collection of packages, including dplyr, purrr, ggplot2, and stringr, used for data manipulation and plotting; my most heavily used package
library(stringr) # Used for string manipulation (e.g., str_detect(), str_replace())
library(janitor) # Cleaning column names and removing empty rows/columns
library(readxl) # Used for importing Excel files (read_excel)

library(adehabitatHR)
library(raster)
library(dplyr)

Chew_card_data <- read_excel("~/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Rat Chew Card Study/Data/Raw data/Chew card data.xlsx")

#Subset data to include on the desired columns and values. In this case I only wanted the `Site`, `Site name`, `C1 Rat detected`, `C2 Rat detected`, `T Rat detected`, `Deployment tree`, `Deployment date`
#columns and I only wanted it to show me that information from sites D,E anf F, which are the only ones we did daily checks for. 
#Use %in% to filter for multiple values of a column name such as "Site"
ChewSimple_subset <- Chew_card_data %>%
  dplyr::filter(Site %in% c("D", "E", "F")) %>%
  dplyr::select(`Site`, `Site name`, `C1 Rat detected`, `C2 Rat detected`, `T Rat detected`, `Deployment tree`, `Deployment date`)

#preview the new dataframe to make sure it's good.  
head(ChewSimple_subset)

#Now I want to swap all the "Yes", "No" and "Unknown" entries that were assigned in Access to a binary format. This is what it looks like for jsut one column
ChewSimple2 <- ChewSimple_subset %>%
  mutate(across(c(`C1 Rat detected`, `C2 Rat detected`, `T Rat detected`), 
                ~ case_when(
                  as.character(.) == "Yes"     ~ 1,
                  as.character(.) == "No"      ~ 0,
                  as.character(.) == "Unknown" ~ NA_real_,
                  TRUE                         ~ NA_real_
                )))

head(ChewSimple2)


#Here is what it looks like to change the yes, no, unknown, for all three detection columns
ChewSimple2 %>%
  summarise(
    C1_ones    = sum(`C1 Rat detected` == 1, na.rm = TRUE),
    C1_zeros   = sum(`C1 Rat detected` == 0, na.rm = TRUE),
    C1_missing = sum(is.na(`C1 Rat detected`)),
    
    C2_ones    = sum(`C2 Rat detected` == 1, na.rm = TRUE),
    C2_zeros   = sum(`C2 Rat detected` == 0, na.rm = TRUE),
    C2_missing = sum(is.na(`C2 Rat detected`)),
    
    T_ones     = sum(`T Rat detected` == 1, na.rm = TRUE),
    T_zeros    = sum(`T Rat detected` == 0, na.rm = TRUE),
    T_missing  = sum(is.na(`T Rat detected`))
  )

head(ChewSimple2)

#Now I want an output that shows me, by site, how many times the first detection (1) happened on C1, C2 or T
ChewSimple2 %>%
  # Step 1: Pivot detection columns to long format
  pivot_longer(cols = c(`C1 Rat detected`, `C2 Rat detected`, `T Rat detected`),
               names_to = "Card", values_to = "Detection") %>%
  
  # Step 2: Filter for rows where detection == 1
  filter(Detection == 1) %>%
  
  # Step 3: Order cards so C1 < C2 < T, then get first detection per Site name
  mutate(Card = factor(Card, levels = c("C1 Rat detected", "C2 Rat detected", "T Rat detected"))) %>%
  arrange(`Site name`, Card) %>%
  group_by(`Site name`) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  
  # Step 4: Count how many first detections per Card per Site
  count(Site, Card) %>%
  pivot_wider(names_from = Card, values_from = n, values_fill = 0)

ChewNull<-ChewSimple_subset %>%
              dplyr::filter(Site %in% c("D", "E", "F")) %>%
              dplyr::select(`C1 Rat detected`, `C2 Rat detected`, `T Rat detected`)

#install unmarked package 

library(unmarked)

chew_null <- occu(~1 ~ 1, ChewNull)

