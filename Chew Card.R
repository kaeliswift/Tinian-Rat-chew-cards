library(lubridate) # Date manipulation
library(tidyverse) # Collection of packages, including dplyr, purrr, ggplot2, and stringr, used for data manipulation and plotting; my most heavily used package
library(stringr) # Used for string manipulation (e.g., str_detect(), str_replace())
library(janitor) # Cleaning column names and removing empty rows/columns
library(readxl) # Used for importing Excel files (read_excel)

library(adehabitatHR)
library(raster)
library(dplyr)

#Read in the data. Needs to be specific to file pathway (i.e may change with device)
Chew_card_data <- read_excel("C:/Users/kaelis/OneDrive - UW/Tinian Forest Bird project/Rat Chew Card Study/Data/Raw data/Chew card data.xlsx")

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
  summarize(
    Sites = n(),
    Detections = sum(Detected, na.rm = TRUE),
    Detection_Rate = mean(Detected, na.rm = TRUE),
    Occupied_Sites = sum(Detections),
    Occupancy_Rate = (Detections/Sites)
  )

#run summary output
summarySite_df

#Check how often camera traps detected rats
CT_Data <- dplyr::select(Chew_card_data$`Camera trap deployed?`, `Date of first contact on CT`, `C1 Rat detected`, `C2 Rat detected`, `T Rat detected`)


#OMIT SITE A since it's check schedule was 5 days instead of 3. 
Chew_card_data <- Chew_card_data %>% filter(Site != "A")

#GENERAL DESCRIPTIVES FOR ALL REMAINING SITES
#Step 1, update detection columns from categorical to integer value
Chew_card_data <- Chew_card_data %>%
  mutate(across(c(`C1 Rat detected`, `C2 Rat detected`, `T Rat detected`), 
                ~ case_when(
                  . == "Yes"     ~ 1,
                  . == "No"      ~ 0,
                  . == "Unknown" ~ NA_real_,
                  TRUE           ~ NA_real_
                )))

#step 2
#Generating an overall detection column 
Chew_card_data <- Chew_card_data %>%
  mutate(Detected = case_when(
    # Sites with daily checks (3 detection columns)
    Site %in% c("D", "E", "F") ~ as.integer(
      (`C1 Rat detected` == 1 | `C2 Rat detected` == 1 | `T Rat detected` == 1)
    ),
    
    # Sites with only terminal check (1 detection column)
    Site %in% c("B") ~ as.integer(`T Rat detected` == 1),
    
    # Default case (e.g., missing site code)
    TRUE ~ NA_integer_
  ))

#Quick check to see if that worked
head(Chew_card_data[, c("Site", "C1 Rat detected", "C2 Rat detected", "T Rat detected", "Detected")])

#Step 3
#Creating a new column that groups sites by habitat type 
Chew_card_data <- Chew_card_data %>%
  mutate(Habitat_Type = case_when(
    Site %in% c("E") ~ "Secondary\nforest",
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


#Plot naive Occupancy rate for all sites surveyed after 3 days (n=400)
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


#CHECKING CAMERA TRAP DATA
CT_data <- Chew_card_data %>%
  dplyr::filter(`Camera trap deployed?` == "1") %>%
  dplyr::filter(Site %in% c("B", "D", "E", "F")) %>%
  dplyr::select(Site, `Site name`, Detected, `Date of first contact on CT`)

#Step one (assumed you already have a filtered data set from the steps above)
Chew_card_data <- Chew_card_data %>%
  mutate(across(c(`Camera trap deployed?`), 
                ~ case_when(
                  . == "TRUE"     ~ 1,
                  . == "FALSE"      ~ 0,
                  . == "Unknown" ~ NA_real_,
                  TRUE           ~ NA_real_
                )))

#35 Camera trap cards were depredated. 32 of those camereas captures rats
#COMPARING DAILY VS SINGLE CHECKS 
#Step 1, create the check type column 
Chew_card_data <- Chew_card_data %>%
  mutate(Check_type = case_when(
    Site %in% c("A", "B") ~ "Single",
    Site %in% c("D", "F", "E") ~ "Daily",
    TRUE                  ~ NA_character_  # For any unexpected site codes
  ))
#Checks that worked 
head(Chew_card_data$Check_type)

#subset data for just Tangan plots
Checkdf <- Chew_card_data %>%
  dplyr::filter(Site %in% c("B", "F")) %>%
  dplyr::select(`Site`, Check_type, `C1 Rat detected`, `C2 Rat detected`, `T Rat detected`)

library(dplyr)
library(unmarked)


# Establish maximum number of surveys = 3 (C1, C2, T)
n_surveys <- 3

# Create detection history matrix with rows = sites, cols = surveys
det_hist <- Checkdf %>%
  rowwise() %>%
  mutate(
    det1 = ifelse(Check_type == "Daily", `C1 Rat detected`, NA_real_),
    det2 = ifelse(Check_type == "Daily", `C2 Rat detected`, NA_real_),
    det3 = `T Rat detected`
  ) %>%
  ungroup() %>%
  dplyr::select(det1, det2, det3) %>%
  as.matrix()

#Prepare site-level covariates data frame
site_covs <- Checkdf %>%
  dplyr::select(Check_type) %>%
  mutate(Check_type = factor(Check_type))

#Create unmarkedFrameOccu object
umf <- unmarkedFrameOccu(y = det_hist, siteCovs = site_covs)

#Fit occupancy model with detection probability depending on Check_type
model <- occu(~ Check_type ~ 1, data = umf)

# Step 6: Summary of results
summary(model)

#Generate a quick summary of detections
Checkdf <- Checkdf %>%
  rename(
    check1 = `C1 Rat detected`,
    check2 = `C2 Rat detected`,
    check3 = `T Rat detected`
  )

Checkdf %>%
  group_by(Site) %>%
  summarize(
    N_sites = n(),
    N_detected = sum(check1 == 1 | check2 == 1 | check3 == 1, na.rm = TRUE),
    Detection_rate = mean(check1 == 1 | check2 == 1 | check3 == 1, na.rm = TRUE)
  )

#ANALYSIS FOR DAILY CHECK SITES ONLY (D,E,F)
#Subset data to include on the desired columns and values. In this case I only wanted the `Site`, `Site name`, `C1 Rat detected`, `C2 Rat detected`, `T Rat detected`, `Deployment tree`, `Deployment date`
#columns and I only wanted it to show me that information from sites D,E anf F, which are the only ones we did daily checks for. 
#Use %in% to filter for multiple values of a column name such as "Site"
Chewdf <- Chew_card_data %>%
  dplyr::filter(Site %in% c("D", "E", "F")) %>%
  dplyr::select(`Site`, `Site name`, `C1 Rat detected`, `C2 Rat detected`, `T Rat detected`, `Deployment tree`, `Deployment date`)

#preview the new dataframe to make sure it's good.  
head(Chewdf)

#Change the column names
Chewdf <- Chewdf %>%
  rename(
    check1 = `C1 Rat detected`,
    check2 = `C2 Rat detected`,
    check3 = `T Rat detected`
  )


#Now I want an output that shows me, by site, how many times the first detection (1) happened on C1, C2 or T
# Step 1: Pivot detection columns to long format
Chewdf %>%
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

# Now let's run a Chi squared test to look for differenes in detections across check days
#Create detection timing summary table

first_detections <- Chewdf %>%
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

Chewdf$Site <- recode(Chewdf$Site,
                      "D" = "Native\nforest",
                      "E" = "Secondary\nforest",
                      "F" = "Tangan\nforest")
                      
chisq.test(detection_matrix)

#Rerunning the chisquared test with SITE included
#Create long format dataset for first detections (with site preserved)
first_detection_df <- Chewdf %>%
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

# Plot
ggplot(first_detections_long, aes(x = Site, y = Count, fill = Card)) +
  geom_col(position = "dodge") +
  labs(title = "First Detection by Site", y = "Number of First Detections", x = "Site") +
  scale_fill_manual(values = c("check1" = "darkseagreen4", "check2" = "coral3", "check3" = "darkslateblue"),
                    labels = c("Check 1", "Check 2", "Check 3")) +
  guides(fill = guide_legend(title = NULL))+ #this got rid of the title that said "card" over the legend 
  theme_minimal(base_size = 12)

#Calculate the propotion of additional chews per day (i.e. replicate Hanslowe stat)
# Step 1: Reshape to long format
daily_prop_df <- Chewdf %>%
  pivot_longer(cols = c(check1, check2, check3),
               names_to = "Day", values_to = "Detection") %>%
  group_by(`Site name`, Day) %>%
  summarize(
    Proportion_Chewed = mean(Detection, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Day, values_from = Proportion_Chewed)

# Step 2: Calculate daily increases per site
daily_increase_df <- daily_prop_df %>%
  mutate(
    Increase_day2 = check2 - check1,
    Increase_day3 = check3 - check2
  )

# Step 3: Average daily increases across sites
average_increase <- daily_increase_df %>%
  summarize(
    Avg_Increase_day2 = mean(Increase_day2, na.rm = TRUE),
    Avg_Increase_day3 = mean(Increase_day3, na.rm = TRUE)
  )

average_increase

#Step 4: Generate average across both days combined 
total_average_increase <- daily_increase_df %>%
  summarize(
    Total_Avg_Increase = mean(c(Increase_day2, Increase_day3), na.rm = TRUE)
  )
total_average_increase


#VISUALIZE THE DATA
#first let's count how many detections occured at each site
#Step 1 sum the totals for each row
Chewdf$detections_total <- rowSums(Chewdf[, c("check1", "check2", "check3")], na.rm = TRUE)
#step 2 extract the any time there was at least one detection by site
tapply(Chewdf$detections_total > 0, Chewdf$Site, sum)

#Now I want a box plot that shows the number of detections by site
#step 1 add a totaled column for detections
Chewdf$detections_total <- rowSums(Chewdf[, c("check1", "check2", "check3")], na.rm = TRUE)

#I want to rename the factor levels so that D,E,F are linked to their forest type
#If I had "Native forest" that's how it would appear on the plot, but that takes up a lot of space
#so I have stacked the name by using \n between the first and second word
Chewdf$Site <- recode(Chewdf$Site,
                      "D" = "Native\nforest",
                      "E" = "Secondary\nforest",
                      "F" = "Tangan\nforest"
)
#Create the boxplot
#cex.axis = 0.7 is the font size. You do not need this code but I added it since they didn't all fit otherwise
boxplot(detections_total ~ Site, data = Chewdf,
        main = "Total Detections per Site",
        ylab = "Total Detections (0–3)",
        xlab = "Site",
        col = "darkseagreen4",
        cex.axis = 0.7)

#RUN THE OCCUPANCY MODEL 
#Install unmarked
library(unmarked)
library(terra)

head(Chewdf)

#prepare the unmarked dataframe
y<-as.matrix(Chewdf[, c("check1", "check2", "check3")])
#assign covariate as a factor
Chewdf$Site <- as.factor(Chewdf$Site)
#create an unmarked frame
site_covs <- data.frame(Site = Chewdf$Site)
umf2 <- unmarkedFrameOccu(y = y, siteCovs = site_covs)

#check that the new data frame looks good
umf2

#run the null model without the covarite 
model_null <- occu(~1 ~1, data = umf2)
summary(model_null)

#Now run the model that includes the covariates
model_site <- occu(~1 ~ Site, data = umf2)
summary(model_site)

#Given that the box plot indicated that tangan had much higher detection that the other two, we may not want to assume that detection is the same across habitat types
#The following code treats the detection probability as being different across sites
model_site_full <- occu(~ Site ~ Site, data = umf2)

#Store the model summary
model_summary <- summary(model_site_full)

model_summary <- summary(model_site_full)

#let's look what happens if we only have TWO DAYS
# Detection history using only check1 and check2
y_day2 <- as.matrix(Chewdf[, c("check1", "check2")])

# Make sure Site is still a factor
Chewdf$Site <- as.factor(Chewdf$Site)

# Site-level covariates
site_covs_day2 <- data.frame(Site = Chewdf$Site)

library(unmarked)
umf_day2 <- unmarkedFrameOccu(y = y_day2, siteCovs = site_covs_day2)

# Null model (no covariates)
model_null_day2 <- occu(~1 ~1, data = umf_day2)
summary(model_null_day2)

# Site-only model (occupancy varies by site, constant detection)
model_site_day2 <- occu(~1 ~ Site, data = umf_day2)
summary(model_site_day2)

# Full model: detection and occupancy both vary by site
model_site_full_day2 <- occu(~ Site ~ Site, data = umf_day2)
model_summary_day2 <- summary(model_site_full_day2)

#Attemping to back transform the output into probability scale 
# Occupancy (logit scale)
occ_est <- c(Intercept = 1.01, SiteE = 7.46, SiteF = 1.61)
occ_se  <- c(Intercept = 0.346, SiteE = 37.753, SiteF = 0.891)

# Detection (logit scale)
det_est <- c(Intercept = 0.613, SiteE = -0.900, SiteF = 0.176)
det_se  <- c(Intercept = 0.268, SiteE = 0.304, SiteF = 0.348)

# Function to calculate back-transformed probability and CI
back_transform <- function(base, delta = 0, base_se, delta_se = 0) {
  # Total estimate and SE
  total_est <- base + delta
  total_se <- sqrt(base_se^2 + delta_se^2)
  
  # Back-transform and CI
  prob <- plogis(total_est)
  ci <- plogis(total_est + c(-1.96, 1.96) * total_se)
  
  return(list(prob = prob, lower = ci[1], upper = ci[2]))
}

# Occupancy
occ_A <- back_transform(occ_est["Intercept"], 0, occ_se["Intercept"])
occ_E <- back_transform(occ_est["Intercept"], occ_est["SiteE"], occ_se["Intercept"], occ_se["SiteE"])
occ_F <- back_transform(occ_est["Intercept"], occ_est["SiteF"], occ_se["Intercept"], occ_se["SiteF"])

# Detection
det_A <- back_transform(det_est["Intercept"], 0, det_se["Intercept"])
det_E <- back_transform(det_est["Intercept"], det_est["SiteE"], det_se["Intercept"], det_se["SiteE"])
det_F <- back_transform(det_est["Intercept"], det_est["SiteF"], det_se["Intercept"], det_se["SiteF"])

# Create output data frame
results <- data.frame(
  Parameter = rep(c("Occupancy", "Detection"), each = 3),
  Site = rep(c("A", "E", "F"), times = 2),
  Estimate = c(occ_A$prob, occ_E$prob, occ_F$prob, det_A$prob, det_E$prob, det_F$prob),
  CI_lower = c(occ_A$lower, occ_E$lower, occ_F$lower, det_A$lower, det_E$lower, det_F$lower),
  CI_upper = c(occ_A$upper, occ_E$upper, occ_F$upper, det_A$upper, det_E$upper, det_F$upper)
)

print(results)


#Let's visualize the two-day model
Chewdf <- Chewdf %>%
  mutate(
    Detected_day2 = as.integer(check1 == 1 | check2 == 1)
  )

summary_day2 <- Chewdf %>%
  group_by(Site) %>%
  summarize(
    Sites = n(),
    Detections = sum(Detected_day2, na.rm = TRUE),
    Occupancy_Rate = mean(Detected_day2, na.rm = TRUE)
  )

ggplot(summary_day2, aes(x = Site, y = Occupancy_Rate)) +
  geom_col(fill = "coral3") +
  ylim(0, 1) +
  labs(
    title = "Occupancy Rate by Check 2",
    x = "Habitat Type",
    y = "Occupancy Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#Now let's visualize occupancy at day 2 vs day three 
Chewdf <- Chewdf %>%
  mutate(
    Occupancy_day2 = as.integer(check1 == 1 | check2 == 1),
    Occupancy_day3 = as.integer(check1 == 1 | check2 == 1 | check3 == 1)
  )

library(dplyr)
#generate the summary table for occupancy rate
summary_compare <- Chewdf %>%
  group_by(Site) %>%
  summarize(
    Occupancy_day2 = mean(Occupancy_day2, na.rm = TRUE),
    Occupancy_day3 = mean(Occupancy_day3, na.rm = TRUE),
    .groups = "drop"
  )

#Generate alternaitve summary table for naive occupancy 

library(tidyr)

summary_long <- summary_compare %>%
  pivot_longer(cols = starts_with("Occupancy"), names_to = "Day", values_to = "Occupancy_Rate") %>%
  mutate(
    Day = recode(Day,
                 "Occupancy_day2" = "Day 2",
                 "Occupancy_day3" = "Day 3")
  )

ggplot(summary_long, aes(x = Site, y = Occupancy_Rate, fill = Day)) +
  geom_col(position = position_dodge(), width = 0.7) +
  scale_fill_manual(values = c("darkseagreen3", "darkseagreen4")) +
  ylim(0, 1) +
  labs(
    title = "Occupancy Rate by Habitat Type",
    x = "Habitat Type",
    y = "Occupancy Rate",
    fill = "Survey Duration"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12),
    legend.position = "top"
  )
#Compare the 2 day vs 3 day model 
model_site_full@AIC           # AIC for 3-day model
model_site_full_day2@AIC 

#Getting the relative abundance index 
Chewdf %>%
  mutate(total_detections = rowSums(across(c(check1, check2, check3)), na.rm = TRUE)) %>%
  group_by(Site) %>%
  summarise(
    Mean_RAI = mean(total_detections, na.rm = TRUE),
    SE_RAI = sd(total_detections, na.rm = TRUE) / sqrt(n())
  )

#Find the proportion of new cards chewed per day 
# Step 1: Reshape to long format
daily_prop_df <- Chewdf %>%
  pivot_longer(cols = c(check1, check2, check3),
               names_to = "Day", values_to = "Detection") %>%
  group_by(`Site name`, Day) %>%
  summarize(
    Proportion_Chewed = mean(Detection, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Day, values_from = Proportion_Chewed)

# Step 2: Calculate daily increases per site
daily_increase_df <- daily_prop_df %>%
  mutate(
    Increase_day2 = check2 - check1,
    Increase_day3 = check3 - check2,
    Mean_Increase = rowMeans(across(c(Increase_day2, Increase_day3)), na.rm = TRUE)
  )

# Step 3: Average daily increases across sites
average_increase <- daily_increase_df %>%
  summarize(
    Avg_Increase_day2 = mean(Increase_day2, na.rm = TRUE),
    Avg_Increase_day3 = mean(Increase_day3, na.rm = TRUE)
  )

# Step 4: Total average increase + standard deviation
total_average_increase <- daily_increase_df %>%
  summarize(
    Total_Avg_Increase = mean(Mean_Increase, na.rm = TRUE),
    SD_Total_Avg_Increase = sd(Mean_Increase, na.rm = TRUE)
  )

# Show results
average_increase
total_average_increase
