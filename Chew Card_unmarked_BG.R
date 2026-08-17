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
#Chew_card_data <- read_excel("Data/Chew card data.xlsx")

#####Pre-processing steps#####
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

#####Calculate Naive occupancy#####

library(dplyr)
library(ggplot2)

occupancy_by_habitat <- Chew_card_data %>%
  group_by(Habitat_Type) %>%
  summarise(
    total_sites = n(),
    occupied_sites = sum(`Overall: Rat Detected` == "1", na.rm = TRUE),
    naive_occupancy = occupied_sites / total_sites
  )

occupancy_by_habitat

##### Figure: naive occupancy ####
ggplot(occupancy_by_habitat,
       aes(x = Habitat_Type, y = naive_occupancy)) +
  geom_col() +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent
  ) +
  labs(
    x = "Forest type",
    y = "Naive occupancy",
    title = "Naive rat occupancy by forest type"
  ) +
  theme_classic()

#step 3
######Generating effort data######
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




library(dplyr)
library(unmarked)


# Establish maximum number of surveys = 3 (C1, C2, T)
#n_surveys <- 3

det_hist <- as.matrix(cbind(Chew_card_data$S1, Chew_card_data$S2, Chew_card_data$S3))
effort <- as.matrix(cbind(Chew_card_data$E1, Chew_card_data$E2, Chew_card_data$E3))

rowSums(det_hist, na.rm=T)

effort_s=matrix(scale(c(effort)), ncol=3)

habitat <- as.vector(as.factor(Chew_card_data$Habitat_Type))


#####Create unmarkedFrameOccu object#####
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



######Compare models#####
library(MuMIn)
ms <- model.sel(null, EffDetc, EffHabDect, EffHabxHab)

ms_out <- ms[, c("df", "AICc", "delta", "weight")]
round(ms_out, 3)

library(writexl)

# Round values first
ms_out_export <- round(ms_out, 3)

# Export to Excel
write_xlsx(
  ms_out_export,
  "model_comparison.xlsx"
)


##### Predict occupancy for each habitat in top model #####
occupancy_predictions <- predict(
  EffHabxHab,
  type = "state",
  newdata = data.frame(
    habitat = levels(umf@siteCovs$habitat)
  )
) %>%
  mutate(
    Habitat = levels(umf@siteCovs$habitat)
  )

occupancy_predictions


##### Predict detection for each habitat in top model EffHabxHab####
intercept <- 1.512

habitat_effects <- data.frame(
  Habitat = c(
    "Reference habitat",
    "Secondary forest",
    "Tangan forest"
  ),
  beta = c(
    0,
    -0.276,
    0.616
  )
)

# Calculate detection probability
daily_detection <- habitat_effects %>%
  mutate(
    logit_p = intercept + beta,
    Detection_Probability = plogis(logit_p),
    Detection_Percent = Detection_Probability * 100
  )

daily_detection



###Relative abundance
###First calculate the proportion

length(which(rowSums(det_hist, na.rm=T)[1:100] > 0))
length(which(rowSums(det_hist, na.rm=T)[101:200] > 0))
length(which(rowSums(det_hist, na.rm=T)[201:300] > 0))
length(which(rowSums(det_hist, na.rm=T)[301:400] > 0))
length(which(rowSums(det_hist, na.rm=T)[401:500] > 0))


#3 nights - formula from Hanslowe et al 2022
#they did not give the equation, so I had to eye ball the intercept
19.49*.8+2 #(native)
19.49*.84+2 #(secondary)
19.49*.99+2 #tangantangan
19.49*.98+2 #(tangantangan)

#5 nights - formula from Hanslowe et al 2022
#In this equation they set the intercept to be 0
23.51*.98 #(secondary)

#BG: I think this is how you got the relative index before, it is based on the 
#mean number of detections in each forest type for the grids with 3 day checks
tapply(rowSums(det_hist, na.rm=T)[201:500], habitat[201:500], mean)

#####Create a 2 day encounter history to mimic earlier estimates

Chew_card_data <- Chew_card_data %>% 
  filter(Site %in% c("D", "E", "F"))


det_hist <- as.matrix(cbind(Chew_card_data$S1, Chew_card_data$S2))
table(rowSums(det_hist))
habitat <- as.vector(as.factor(Chew_card_data$Habitat_Type))
umf <- unmarkedFrameOccu(y = det_hist, siteCovs = data.frame(habitat=habitat) )

#Fit null occupancy model
model.test <- occu(~ habitat ~ habitat, data = umf)




######ANALYSIS FOR DAILY CHECK SITES ONLY (D,E,F)######
#Subset data to include on the desired columns and values. In this case I only wanted the `Site`, `Site name`, `C1 Rat detected`, `C2 Rat detected`, `T Rat detected`, `Deployment tree`, `Deployment date`
#columns and I only wanted it to show me that information from sites D,E anf F, which are the only ones we did daily checks for. 
#Use %in% to filter for multiple values of a column name such as "Site"
Dailydf <- Chew_card_data %>%
  dplyr::filter(Site %in% c("D", "E", "F")) %>%
  dplyr::select(`Site`, `Site name`, `C1 Rat detected`, `C2 Rat detected`, `T Rat detected`, `Deployment tree`, `Deployment date`, Habitat_Type)

Dailydf <- Dailydf %>%
  dplyr::rename(
    check1 = `C1 Rat detected`,
    check2 = `C2 Rat detected`,
    check3 = `T Rat detected`
  )

#preview the new dataframe to make sure it's good.  
head(Dailydf)

#Now I want an output that shows me, by site, how many times the first detection (1) happened on C1, C2 or T
# Step 1: Pivot detection columns to long format
Dailydf %>%
  pivot_longer(cols = c(check1, check2, check3),
               names_to = "Card", values_to = "Detection") %>%
  
  # Step 2: Filter for rows where detection == 1
  filter(Detection == 1) %>%
  
  # Step 3: Order cards so C1 < C2 < T, then get first detection per Site name
  mutate(Card = factor(Card, levels = c("check1", "check2", "check3"))) %>%
  arrange(`Site name`, Card) %>%
  group_by(`Site name`) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  
  # Step 4: Count how many first detections per Card per Site
  count(Site, Card) %>%
  pivot_wider(names_from = Card, values_from = n, values_fill = 0)

# Now let's run a Chi squared test to look for differences in detections across check days
#Create detection timing summary table

first_detections <- Dailydf %>%
  pivot_longer(cols = c(check1, check2, check3),
               names_to = "Card", values_to = "Detection") %>%
  filter(Detection == 1) %>%
  mutate(Card = factor(Card, levels = c("check1", "check2", "check3"))) %>%
  arrange(`Site name`, Card) %>%
  group_by(`Site name`) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  count(Site, Card) %>%
  pivot_wider(names_from = Card, values_from = n, values_fill = 0)

# Remove Site column and convert to matrix
detection_matrix <- as.matrix(first_detections[,-1])
rownames(detection_matrix) <- first_detections$Site

# Run chi-square test

Dailydf$Site <- recode(Dailydf$Site,
                       "D" = "Native\nforest",
                       "E" = "Secondary\nforest",
                       "F" = "Tangan\nforest")

chisq.test(detection_matrix)

#Rerunning the chisquared test with SITE included
#Create long format dataset for first detections (with site preserved)
first_detection_df <- Dailydf %>%
  pivot_longer(cols = c(check1, check2, check3),
               names_to = "Card", values_to = "Detection") %>%
  filter(Detection == 1) %>%
  mutate(Card = factor(Card, levels = c("check1", "check2", "check3"))) %>%
  arrange(`Site name`, Card) %>%
  group_by(`Site name`) %>%
  slice_head(n = 1) %>%
  ungroup()

#Run the test
chisq.test(table(first_detection_df$Site, first_detection_df$Card))

#run posthoc to exmain for differences within each site 
site_posthoc <- first_detection_df %>%
  group_by(Site) %>%
  summarise(p_value = chisq.test(table(Card))$p.value) %>%
  mutate(adjusted_p = p.adjust(p_value, method = "bonferroni"))

site_posthoc

#visualalize the results of the chi sqaured test for 
library(tidyr)
library(ggplot2)

first_detections_long <- first_detection_df %>%
  count(Site, Card, name = "Count")

# Optional: set factor levels for nicer x-axis order
first_detections_long$Site <- factor(first_detections_long$Site,
                                     levels = c("Native\nforest", "Secondary\nforest", "Tangan\nforest"))

######Figure:First detections by site #####
ggplot(first_detections_long, aes(x = Site, y = Count, fill = Card)) +
  geom_col(position = "dodge") +
  labs(title = "First Detection by Site", y = "Number of First Detections", x = "Site") +
  scale_fill_manual(values = c("check1" = "darkseagreen4", "check2" = "coral3", "check3" = "darkslateblue"),
                    labels = c("Check 1", "Check 2", "Check 3")) +
  guides(fill = guide_legend(title = NULL))+ #this got rid of the title that said "card" over the legend 
  theme_minimal(base_size = 12)


####Calculate the Daily increase in chews per day####

#Since some site names are repeated across habitats, make a new column that makes them all unique 
Dailydf <- Dailydf %>%
  mutate(
    Site_Habitat = paste(`Site name`, Habitat_Type, sep = "_")
  )


# 1. Calculate proportion of cards with chews for each site and check
daily_prop_df <- Dailydf %>%
  pivot_longer(
    cols = c(check1, check2, check3),
    names_to = "Check",
    values_to = "Detection"
  ) %>%
  group_by(`Site_Habitat`, Check) %>%
  summarise(
    Proportion_Chewed = mean(Detection, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Check,
    values_from = Proportion_Chewed
  )
 #check dropped sites
nrow(Dailydf)

daily_long <- Dailydf %>%
  pivot_longer(
    cols = c(check1, check2, check3),
    names_to = "Check",
    values_to = "Detection"
  )

nrow(daily_long)

daily_grouped <- daily_long %>%
  group_by(`Site_Habitat`, Check) %>%
  summarise(
    Proportion_Chewed = mean(Detection, na.rm = TRUE),
    .groups = "drop"
  )

nrow(daily_grouped)

daily_wide <- daily_grouped %>%
  pivot_wider(
    names_from = Check,
    values_from = Proportion_Chewed
  )

nrow(daily_wide)

Dailydf %>%
  count(Site_Habitat) %>%
  filter(n > 1)


# 2. Calculate the increase between consecutive checks for each site
daily_increase_df <- daily_prop_df %>%
  mutate(
    Increase_check2 = check2 - check1,
    Increase_check3 = check3 - check2
  )

# 3. Calculate the average daily increase for each site
daily_increase_df <- daily_increase_df %>%
  mutate(
    Mean_Daily_Increase = rowMeans(
      cbind(Increase_check2, Increase_check3),
      na.rm = TRUE
    )
  )

# 4. Calculate overall mean and SD across sites
overall_daily_increase <- daily_increase_df %>%
  summarise(
    Mean_Increase = mean(Mean_Daily_Increase, na.rm = TRUE),
    SD_Increase = sd(Mean_Daily_Increase, na.rm = TRUE),
    N_sites = sum(!is.na(Mean_Daily_Increase))
  )

overall_daily_increase

