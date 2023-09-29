library("tidyverse")
library("readxl")
library("stringr")
library(openxlsx)

# Set Working Directory
setwd("/Users/aidanbeilke/Downloads")

#Import 2021 Data
dashboard21 <- read_xlsx("specs2021.xlsx")
hard21 <- read.xlsx("bbe_2021.xlsx") %>% 
  mutate(Year = 2021)
spin21 <- read.csv("active-spin_2021.csv") %>% 
  mutate(Year = 2021)

#Import 2022 Data
dashboard22 <- read_xlsx("specs_2022.xlsx")
hard22 <- read.xlsx("bbe_2022.xlsx") %>% 
  mutate(Year = 2022)
spin22 <- read.csv("active-spin_2022.csv") %>% 
  mutate(Year = 2022)

#Import 2023 Data
dashboard23 <- read_xlsx("specs_2023.xlsx")
hard23 <- read.xlsx("bbe_2023.xlsx") %>% 
  mutate(Year = 2023)
spin23 <- read.csv("active-spin_2023.csv") %>% 
  mutate(Year = 2023)


# List of data frames
df_list <- list(dashboard21 = dashboard21, dashboard22 = dashboard22, dashboard23 = dashboard23)

# Variables you are interested in
selected_vars <- c("First_Name", "Last_Name","Year" ,"Pitch_Type", "MPH", "RPM", "VAA", "Ext (ft)", "H-Rel Pt", "V-Mov", "H-Mov",
                   "Usage.", "wOBA", "Pitches")

# Initialize an empty list to store the new data frames
selected_df_list <- list()

for (name in names(df_list)) {
  # Select the desired variables using select function from dplyr
  assign(paste0(name, "_selected"), df_list[[name]] %>% select(all_of(selected_vars)))
}

#Combine all 3 years in single df
dashboard <- rbind(dashboard21_selected, dashboard22_selected, dashboard23_selected)
hard <- rbind(hard21, hard22, hard23)
spin <- rbind(spin21, spin22, spin23)

# Get adequate sample
dashboard <- dashboard %>% 
  mutate(full_name = paste(First_Name, Last_Name)) %>% 
  select(full_name, everything()) %>% 
  filter(Pitches >= 100 & Usage. >= .10)

#add full name variable
hard <- hard %>% 
  mutate(full_name = paste(First_Name, Last_Name)) %>% 
  select(full_name,Pitch_Type ,Hard., Year)

#Get hard hit data into df
dashboard <- merge(dashboard, hard, by = c("full_name", "Pitch_Type", "Year"))

#Get gyro spin data
spin <- spin %>% 
  mutate(
    full_name = str_split(last_name..first_name, ", ") %>%
      map_chr(~ paste(.[2], .[1]))
  ) %>% 
  select(full_name, everything()) %>% 
  select(-last_name..first_name)

# Start FB Unique Analysis
spinfb <- spin %>% 
  select(full_name, active_spin_fourseam, Year)

fb <- dashboard %>% 
  filter(Pitch_Type == "FF")

allfb <- merge(fb, spinfb, by = c("full_name", "Year"))

#Round Values
allfb$MPH <- round(allfb$MPH,1)
allfb$RPM <- round(allfb$RPM,0)
allfb$VAA <- round(allfb$VAA, 1)
allfb$`Ext (ft)`<- round(allfb$`Ext (ft)`, 1)
allfb$`H-Rel Pt` <- round(allfb$`H-Rel Pt`, 1)
allfb$`V-Mov` <- round(allfb$`V-Mov`, 1)
allfb$`H-Mov` <- abs(allfb$`H-Mov`)
allfb$`H-Mov` <- round(allfb$`H-Mov`,1)

# Select Unique Variables
allfb <- allfb %>% 
  select(full_name, Pitches,Pitch_Type ,MPH, 
         RPM, active_spin_fourseam, `V-Mov`, `H-Mov`,`Ext (ft)`, 
         `H-Rel Pt`, wOBA, VAA, Usage., Hard., Year)

#Find uniqueness by each metric
allfb$vert_norm <- abs((allfb$`V-Mov` - mean(allfb$`V-Mov`)) / sd(allfb$`V-Mov`)) 
allfb$va_norm <- abs((allfb$VAA - mean(allfb$VAA)) / sd(allfb$VAA))
allfb$rpm_norm <- abs((allfb$RPM - mean(allfb$RPM)) / sd(allfb$RPM))
allfb$horiz_norm <- abs((allfb$`H-Mov` - mean(allfb$`H-Mov`)) / sd(allfb$`H-Mov`)) 
allfb$gyro_norm <- abs((allfb$active_spin_fourseam - mean(allfb$active_spin_fourseam, na.rm =T)) / sd(allfb$active_spin_fourseam, na.rm =T))
allfb$mph_norm <- (allfb$MPH - mean(allfb$MPH)) / sd(allfb$MPH)

# Sinker Uniqueness
si <- dashboard %>% 
  filter(Pitch_Type == "SI")

spinsi <- spin %>% 
  select(full_name, active_spin_sinker, Year)

allsi <- merge(si, spinsi, by = c("full_name", "Year"))

allsi$MPH <- round(allsi$MPH,1)
allsi$RPM <- round(allsi$RPM,0)
allsi$VAA <- round(allsi$VAA, 1)
allsi$`Ext (ft)`<- round(allsi$`Ext (ft)`, 1)
allsi$`H-Rel Pt` <- round(allsi$`H-Rel Pt`, 1)
allsi$`V-Mov` <- round(allsi$`V-Mov`, 1)
allsi$`H-Mov` <- abs(allsi$`H-Mov`)
allsi$`H-Mov` <- round(allsi$`H-Mov`,1)

allsi <- allsi %>% 
  select(full_name, Pitches,Pitch_Type ,MPH, 
         RPM, active_spin_sinker, `V-Mov`, `H-Mov`,`Ext (ft)`, 
         `H-Rel Pt`, wOBA, VAA, Usage., Hard., Year)

allsi$vert_norm <- abs((allsi$`V-Mov` - mean(allsi$`V-Mov`)) / sd(allsi$`V-Mov`)) 
allsi$va_norm <- abs((allsi$VAA - mean(allsi$VAA)) / sd(allsi$VAA))
allsi$rpm_norm <- abs((allsi$RPM - mean(allsi$RPM)) / sd(allsi$RPM))
allsi$horiz_norm <- abs((allsi$`H-Mov` - mean(allsi$`H-Mov`)) / sd(allsi$`H-Mov`)) 
allsi$gyro_norm <- abs((allsi$active_spin_sinker - mean(allsi$active_spin_sinker, na.rm =T)) / sd(allsi$active_spin_sinker, na.rm =T))
allsi$mph_norm <- (allsi$MPH - mean(allsi$MPH)) / sd(allsi$MPH)

# Cutter Uniqueness
cu <- dashboard %>% 
  filter(Pitch_Type == "FC")

spincu <- spin %>% 
  select(full_name, active_spin_cutter, Year)

allcu <- merge(cu, spincu, by = c("full_name", "Year"))

allcu$MPH <- round(allcu$MPH, 1)
allcu$RPM <- round(allcu$RPM, 0)
allcu$VAA <- round(allcu$VAA, 1)
allcu$`Ext (ft)` <- round(allcu$`Ext (ft)`, 1)
allcu$`H-Rel Pt` <- round(allcu$`H-Rel Pt`, 1)
allcu$`V-Mov` <- round(allcu$`V-Mov`, 1)
allcu$`H-Mov` <- abs(allcu$`H-Mov`)
allcu$`H-Mov` <- round(allcu$`H-Mov`, 1)


allcu <- allcu %>%
  select(full_name, Pitches,Pitch_Type ,MPH, 
         RPM, active_spin_cutter, `V-Mov`, `H-Mov`,`Ext (ft)`, 
         `H-Rel Pt`, wOBA, VAA, Usage., Hard., Year)

allcu$vert_norm <- abs((allcu$`V-Mov` - mean(allcu$`V-Mov`)) / sd(allcu$`V-Mov`))
allcu$va_norm <- abs((allcu$VAA - mean(allcu$VAA)) / sd(allcu$VAA))
allcu$rpm_norm <- abs((allcu$RPM - mean(allcu$RPM)) / sd(allcu$RPM))
allcu$horiz_norm <- abs((allcu$`H-Mov` - mean(allcu$`H-Mov`)) / sd(allcu$`H-Mov`))
allcu$gyro_norm <- abs((allcu$active_spin_cutter - mean(allcu$active_spin_cutter, na.rm = TRUE)) / sd(allcu$active_spin_cutter, na.rm = TRUE))
allcu$mph_norm <- (allcu$MPH - mean(allcu$MPH)) / sd(allcu$MPH)

#Slider Uniqueness
sl <- dashboard %>% 
  filter(Pitch_Type %in% c("SW", "SL", "SV"))

spinsl <- spin %>% 
  select(full_name, active_spin_slider, Year)

allsl <- merge(sl, spinsl, by = c("full_name", "Year"))

allsl$MPH <- round(allsl$MPH, 1)
allsl$RPM <- round(allsl$RPM, 0)
allsl$VAA <- round(allsl$VAA, 1)
allsl$`Ext (ft)` <- round(allsl$`Ext (ft)`, 1)
allsl$`H-Rel Pt` <- round(allsl$`H-Rel Pt`, 1)
allsl$`V-Mov` <- round(allsl$`V-Mov`, 1)
allsl$`H-Mov` <- abs(allsl$`H-Mov`)
allsl$`H-Mov` <- round(allsl$`H-Mov`, 1)

allsl <- allsl %>%
  select(full_name,Pitch_Type ,Pitches, MPH, RPM, active_spin_slider, 
         `V-Mov`, `H-Mov`, `Ext (ft)`, `H-Rel Pt`,
         wOBA, VAA, Usage., Hard., Year)

allsl$vert_norm <- abs((allsl$`V-Mov` - mean(allsl$`V-Mov`)) / sd(allsl$`V-Mov`))
allsl$va_norm <- abs((allsl$VAA - mean(allsl$VAA)) / sd(allsl$VAA))
allsl$rpm_norm <- abs((allsl$RPM - mean(allsl$RPM)) / sd(allsl$RPM))
allsl$horiz_norm <- abs((allsl$`H-Mov` - mean(allsl$`H-Mov`)) / sd(allsl$`H-Mov`))
allsl$gyro_norm <- abs((allsl$active_spin_slider - mean(allsl$active_spin_slider, na.rm = TRUE)) / sd(allsl$active_spin_slider, na.rm = TRUE))
allsl$mph_norm <- abs((allsl$MPH - mean(allsl$MPH)) / sd(allsl$MPH))

# Changeup Uniqueness
ch <- dashboard %>% 
  filter(Pitch_Type == "CH")

spinch <- spin %>% 
  select(full_name, active_spin_changeup, Year)

allch <- merge(ch, spinch, by = c("full_name", "Year"))

allch$MPH <- round(allch$MPH, 1)
allch$RPM <- round(allch$RPM, 0)
allch$VAA <- round(allch$VAA, 1)
allch$`Ext (ft)` <- round(allch$`Ext (ft)`, 1)
allch$`H-Rel Pt` <- round(allch$`H-Rel Pt`, 1)
allch$`V-Mov` <- round(allch$`V-Mov`, 1)
allch$`H-Mov` <- abs(allch$`H-Mov`)
allch$`H-Mov` <- round(allch$`H-Mov`, 1)

allch <- allch %>%
  select(full_name,Pitch_Type ,Pitches, MPH, RPM, active_spin_changeup, 
         `V-Mov`, `H-Mov`, `Ext (ft)`, `H-Rel Pt`, 
         wOBA, VAA, Usage., Hard., Year)

allch$vert_norm <- abs((allch$`V-Mov` - mean(allch$`V-Mov`)) / sd(allch$`V-Mov`))
allch$va_norm <- abs((allch$VAA - mean(allch$VAA)) / sd(allch$VAA))
allch$rpm_norm <- abs((allch$RPM - mean(allch$RPM)) / sd(allch$RPM))
allch$horiz_norm <- abs((allch$`H-Mov` - mean(allch$`H-Mov`)) / sd(allch$`H-Mov`))
allch$gyro_norm <- abs((allch$active_spin_changeup - mean(allch$active_spin_changeup, na.rm = TRUE)) / sd(allch$active_spin_changeup, na.rm = TRUE))
allch$mph_norm <- abs((allch$MPH - mean(allch$MPH)) / sd(allch$MPH))

#Curveball Uniqueness

cb <- dashboard %>% 
  filter(Pitch_Type == "CU")

spincb <- spin %>% 
  select(full_name, active_spin_curve, Year)

allcb <- merge(cb, spincb, by = c("full_name", "Year"))

allcb$MPH <- round(allcb$MPH, 1)
allcb$RPM <- round(allcb$RPM, 0)
allcb$VAA <- round(allcb$VAA, 1)
allcb$`Ext (ft)` <- round(allcb$`Ext (ft)`, 1)
allcb$`H-Rel Pt` <- round(allcb$`H-Rel Pt`, 1)
allcb$`V-Mov` <- round(allcb$`V-Mov`, 1)
allcb$`H-Mov` <- abs(allcb$`H-Mov`)
allcb$`H-Mov` <- round(allcb$`H-Mov`, 1)

allcb <- allcb %>%
  select(full_name, Pitch_Type ,Pitches, MPH, RPM, active_spin_curve, 
         `V-Mov`, `H-Mov`, `Ext (ft)`, `H-Rel Pt`
         , wOBA, VAA, Usage., Hard., Year)

allcb$vert_norm <- abs((allcb$`V-Mov` - mean(allcb$`V-Mov`)) / sd(allcb$`V-Mov`))
allcb$va_norm <- abs((allcb$VAA - mean(allcb$VAA)) / sd(allcb$VAA))
allcb$rpm_norm <- abs((allcb$RPM - mean(allcb$RPM)) / sd(allcb$RPM))
allcb$horiz_norm <- abs((allcb$`H-Mov` - mean(allcb$`H-Mov`)) / sd(allcb$`H-Mov`))
allcb$gyro_norm <- abs((allcb$active_spin_curve - mean(allcb$active_spin_curve, na.rm = TRUE)) / sd(allcb$active_spin_curve, na.rm = TRUE))
allcb$mph_norm <- abs((allcb$MPH - mean(allcb$MPH)) / sd(allcb$MPH))

# Splitter Uniqueness
sp <- dashboard %>% 
  filter(Pitch_Type == "FS")

allsp <- sp

allsp$MPH <- round(allsp$MPH, 1)
allsp$RPM <- round(allsp$RPM, 0)
allsp$VAA <- round(allsp$VAA, 1)
allsp$`Ext (ft)` <- round(allsp$`Ext (ft)`, 1)
allsp$`H-Rel Pt` <- round(allsp$`H-Rel Pt`, 1)
allsp$`V-Mov` <- round(allsp$`V-Mov`, 1)
allsp$`H-Mov` <- abs(allsp$`H-Mov`)
allsp$`H-Mov` <- round(allsp$`H-Mov`, 1)

allsp <- sp %>%
  select(full_name,Pitch_Type,Pitches, MPH, RPM, `V-Mov`, 
         `H-Mov`, `Ext (ft)`, 
         `H-Rel Pt`, wOBA, VAA, Usage., Hard., Year)

allsp$vert_norm <- abs((allsp$`V-Mov` - mean(allsp$`V-Mov`)) / sd(allsp$`V-Mov`))
allsp$va_norm <- abs((allsp$VAA - mean(allsp$VAA)) / sd(allsp$VAA))
allsp$rpm_norm <- abs((allsp$RPM - mean(allsp$RPM)) / sd(allsp$RPM))
allsp$horiz_norm <- abs((allsp$`H-Mov` - mean(allsp$`H-Mov`)) / sd(allsp$`H-Mov`))
allsp$mph_norm <- abs((allsp$MPH - mean(allsp$MPH)) / sd(allsp$MPH))
allsp$gyro_norm <- .5


# List of data frames
df_list2 <- list(allcb = allcb, allch = allch, allcu = allcu,
                allfb = allfb, allsi = allsi, allsl = allsl, allsp = allsp)

# Variables you are interested in
selected_vars2 <- c("full_name", "Pitch_Type", "Year", "wOBA", "Hard.", "vert_norm", 
                   "va_norm", "rpm_norm", "horiz_norm", "gyro_norm", "mph_norm")

# Initialize an empty list to store the new data frames
selected_df_list2 <- list()

for (name in names(df_list2)) {
  # Select the desired variables using select function from dplyr
  assign(paste0(name, "_selected"), df_list2[[name]] %>% select(all_of(selected_vars2)))
}

# Use dataframe for ML
all <- rbind(allcb_selected, allch_selected, allcu_selected, allfb_selected, allsi_selected,
             allsl_selected, allsp_selected)

# Create Handeness Variable
dashboard$`H-Mov` <- dashboard$`H-Mov`*-1 ## Adjust statcast horizontal movement numbers

rhp <- dashboard %>%
  mutate(hand = ifelse(Pitch_Type %in% c("SL", "SW", "SW") & `H-Mov` < 0, "RHP",
                       ifelse(Pitch_Type == "SI" & `H-Mov` > 0, "RHP",
                              ifelse(Pitch_Type == "CU" & `H-Mov` < 0, "RHP",
                                     ifelse(Pitch_Type == "CH" & `H-Mov` > 0, "RHP", "LHP")
                              )
                       )
  )
  ) %>%
  select(full_name, hand) %>%
  group_by(full_name) %>%
  summarise(hand = max(hand))


rhp$hand[rhp$full_name == "Drew Smyly"] <- "LHP"
dashboard <- merge(dashboard, rhp, by = "full_name")

# Get average movements for plot
avg_movements <- dashboard %>%
  group_by(hand, Pitch_Type) %>%
  summarise(avg_hmov = mean(`H-Mov`),
            avg_vmov = mean(`V-Mov`)) %>% 
  filter(!Pitch_Type %in% c("EP", "Enyel", "Yerry", "s", "FA", "FO", "SC")) %>% 
  na.omit()
