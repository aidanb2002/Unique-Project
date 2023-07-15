library("tidyverse")
library("readxl")
library("stringr")
library(openxlsx)


## Download Data and Set working Directory ##

setwd("/Users/aidanbeilke/Downloads")
dashboard <- read_xlsx("specs (26).xlsx")
hard <- read.xlsx("bbe (5).xlsx")
spin2 <- read.csv("active-spin (3).csv")
spin3 <- read.csv("active-spin (4).csv")

# Set Adequate Sample Size #

dashboard <- dashboard %>% 
  mutate(full_name = paste(First_Name, Last_Name)) %>% 
  select(full_name, everything()) %>% 
  filter(Pitches >= 30 & Usage. >= .05)

hard <- hard %>% 
  mutate(full_name = paste(First_Name, Last_Name)) %>% 
  select(full_name,Pitch_Type ,Hard.)

dashboard <- merge(dashboard, hard, by = c("full_name", "Pitch_Type"))

fb <- dashboard %>% 
  filter(Pitch_Type == "FF")

spin2$full_name <- paste(spin2$first_name, spin2$last_name)
spin3$full_name <- paste(spin3$first_name, spin3$last_name)

spin <- bind_rows(spin2, spin3) %>%
  distinct(full_name, .keep_all = TRUE) %>% 
  mutate(full_name = trimws(full_name))

## Join Tables ##

spinfb <- spin %>% 
  select(full_name, active_spin_fourseam)

fb <- dashboard %>% 
  filter(Pitch_Type == "FF")

allfb <- merge(fb, spinfb, by = "full_name")

## Start Unique Analysis ##
## Fastballs ##

allfb$MPH <- round(allfb$MPH,1)
allfb$RPM <- round(allfb$RPM,0)
allfb$VAA <- round(allfb$VAA, 1)
allfb$`Ext (ft)`<- round(allfb$`Ext (ft)`, 1)
allfb$`H-Rel Pt` <- round(allfb$`H-Rel Pt`, 1)
allfb$`V-Mov` <- round(allfb$`V-Mov`, 1)
allfb$`H-Mov` <- abs(allfb$`H-Mov`)
allfb$`H-Mov` <- round(allfb$`H-Mov`,1)

allfb <- allfb %>% 
  select(full_name, Pitches,Pitch_Type ,MPH, RPM, active_spin_fourseam, `V-Mov`, `H-Mov`,`V-Rel Pt`,`Ext (ft)`, `H-Rel Pt`, wOBA, VAA, Usage., Hard.) %>% 
  filter(`V-Rel Pt` >= 4.4) ## No submarines;

# Normalize the Data #

allfb$vert_norm <- abs((allfb$`V-Mov` - mean(allfb$`V-Mov`)) / sd(allfb$`V-Mov`)) 
allfb$va_norm <- abs((allfb$VAA - mean(allfb$VAA)) / sd(allfb$VAA))
allfb$rpm_norm <- abs((allfb$RPM - mean(allfb$RPM)) / sd(allfb$RPM))
allfb$horiz_norm <- abs((allfb$`H-Mov` - mean(allfb$`H-Mov`)) / sd(allfb$`H-Mov`)) 
allfb$gyro_norm <- abs((allfb$active_spin_fourseam - mean(allfb$active_spin_fourseam, na.rm =T)) / sd(allfb$active_spin_fourseam, na.rm =T))
allfb$mph_norm <- abs((allfb$MPH - mean(allfb$MPH)) / sd(allfb$MPH))


allfb$total <- sqrt(abs(allfb$vert_norm + allfb$horiz_norm + allfb$gyro_norm))
allfb$total <- ifelse(allfb$MPH > 96, allfb$total + (allfb$mph_norm), allfb$total) # Boost for FB Above 96
allfb$total <- ifelse(allfb$RPM >= 2200, allfb$total + (allfb$rpm_norm), allfb$total) # Boost for FB RPM above 2200
allfb$total <- ifelse(allfb$VAA > -5, allfb$total + (allfb$va_norm), allfb$total) # Boost for flat VAA
allfb$percentile <- cut(allfb$total, breaks = quantile(allfb$total, probs = seq(0, 1, by = 0.01), na.rm = TRUE), labels = FALSE)
allfb$plus <- round((allfb$total / mean(allfb$total, na.rm = T))*100)

allfb1 <- allfb %>% 
  select(full_name,Pitch_Type ,Pitches, MPH, RPM, active_spin_fourseam, 
         `V-Mov`, `H-Mov`, VAA, plus, wOBA, Usage., Hard.) %>% 
  mutate(gyro = active_spin_fourseam) %>% 
  select(-active_spin_fourseam)


## Sinkers ##

si <- dashboard %>% 
  filter(Pitch_Type == "SI")

spinsi <- spin %>% 
  select(full_name, active_spin_sinker)

allsi <- merge(si, spinsi, by = "full_name")

allsi$MPH <- round(allsi$MPH,1)
allsi$RPM <- round(allsi$RPM,0)
allsi$VAA <- round(allsi$VAA, 1)
allsi$`Ext (ft)`<- round(allsi$`Ext (ft)`, 1)
allsi$`H-Rel Pt` <- round(allsi$`H-Rel Pt`, 1)
allsi$`V-Mov` <- round(allsi$`V-Mov`, 1)
allsi$`H-Mov` <- abs(allsi$`H-Mov`)
allsi$`H-Mov` <- round(allsi$`H-Mov`,1)

allsi <- allsi %>% 
  select(full_name,Pitch_Type ,Pitches, MPH, RPM, active_spin_sinker, 
         `V-Mov`, `H-Mov`,`V-Rel Pt`,`Ext (ft)`, `H-Rel Pt`, wOBA, VAA, Usage., Hard.) %>% 
  filter(`V-Rel Pt` >= 4.4) ## No submarines; 

# Normalize Data #

allsi$vert_norm <- abs((allsi$`V-Mov` - mean(allsi$`V-Mov`)) / sd(allsi$`V-Mov`)) 
allsi$va_norm <- abs((allsi$VAA - mean(allsi$VAA)) / sd(allsi$VAA))
allsi$rpm_norm <- abs((allsi$RPM - mean(allsi$RPM)) / sd(allsi$RPM))
allsi$horiz_norm <- abs((allsi$`H-Mov` - mean(allsi$`H-Mov`)) / sd(allsi$`H-Mov`)) 
allsi$gyro_norm <- abs((allsi$active_spin_sinker - mean(allsi$active_spin_sinker, na.rm =T)) / sd(allsi$active_spin_sinker, na.rm =T))
allsi$mph_norm <- abs((allsi$MPH - mean(allsi$MPH)) / sd(allsi$MPH))

allsi$total <- sqrt(abs(allsi$vert_norm + allsi$horiz_norm + allsi$gyro_norm + allsi$mph_norm))
allsi$percentile <- cut(allsi$total, breaks = quantile(allsi$total, probs = seq(0, 1, by = 0.01), na.rm = TRUE), labels = FALSE)
allsi$plus <- round((allsi$total / mean(allsi$total, na.rm = T))*100)

allsi1 <- allsi %>% 
  select(full_name,Pitch_Type ,Pitches, MPH, RPM, active_spin_sinker, 
         `V-Mov`, `H-Mov`, VAA, plus, wOBA, Usage., Hard.) %>% 
  mutate(gyro = active_spin_sinker) %>% 
  select(-active_spin_sinker)

## Cutters ##

cu <- dashboard %>% 
  filter(Pitch_Type == "FC")

spincu <- spin %>% 
  select(full_name, active_spin_cutter)

allcu <- merge(cu, spincu, by = "full_name")

allcu$MPH <- round(allcu$MPH, 1)
allcu$RPM <- round(allcu$RPM, 0)
allcu$VAA <- round(allcu$VAA, 1)
allcu$`Ext (ft)` <- round(allcu$`Ext (ft)`, 1)
allcu$`H-Rel Pt` <- round(allcu$`H-Rel Pt`, 1)
allcu$`V-Mov` <- round(allcu$`V-Mov`, 1)
allcu$`H-Mov` <- abs(allcu$`H-Mov`)
allcu$`H-Mov` <- round(allcu$`H-Mov`, 1)


allcu <- allcu %>%
  select(full_name, Pitch_Type ,Pitches, MPH, RPM, active_spin_cutter, `V-Mov`, 
         `H-Mov`, `V-Rel Pt`, `Ext (ft)`, `H-Rel Pt`, wOBA, VAA, Usage., Hard.) %>%
  filter(`V-Rel Pt` >= 4.4) ## No submarines

allcu$vert_norm <- abs((allcu$`V-Mov` - mean(allcu$`V-Mov`)) / sd(allcu$`V-Mov`))
allcu$va_norm <- abs((allcu$VAA - mean(allcu$VAA)) / sd(allcu$VAA))
allcu$rpm_norm <- abs((allcu$RPM - mean(allcu$RPM)) / sd(allcu$RPM))
allcu$horiz_norm <- abs((allcu$`H-Mov` - mean(allcu$`H-Mov`)) / sd(allcu$`H-Mov`))
allcu$gyro_norm <- abs((allcu$active_spin_cutter - mean(allcu$active_spin_cutter, na.rm = TRUE)) / sd(allcu$active_spin_cutter, na.rm = TRUE))
allcu$mph_norm <- abs((allcu$MPH - mean(allcu$MPH)) / sd(allcu$MPH))

allcu$total <- sqrt(abs(allcu$horiz_norm + allcu$rpm_norm + allcu$vert_norm))
allcu$total <- ifelse(allcu$MPH > 91, allcu$total + (allcu$mph_norm), allcu$total)
allcu$total <- ifelse(allcu$VAA > -5.5, allcu$total + (allcu$va_norm), allcu$total)
allcu$percentile <- cut(allcu$total, breaks = quantile(allcu$total, probs = seq(0, 1, by = 0.01), na.rm = TRUE), labels = FALSE)
allcu$plus <- round((allcu$total / mean(allcu$total, na.rm = T))*100)

allcu1 <- allcu %>% 
  select(full_name, Pitch_Type ,Pitches, MPH, RPM, active_spin_cutter, 
         `V-Mov`, `H-Mov`, VAA, plus, wOBA, Usage., Hard.) %>% 
  mutate(gyro = active_spin_cutter) %>% 
  select(-active_spin_cutter)

## Slider ##
sl <- dashboard %>% 
  filter(Pitch_Type %in% c("SW", "SL", "SV"))

spinsl <- spin %>% 
  select(full_name, active_spin_slider)

allsl <- merge(sl, spinsl, by = "full_name")

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
         `V-Mov`, `H-Mov`, `V-Rel Pt`, `Ext (ft)`, `H-Rel Pt`,
         wOBA, VAA, Usage., Hard.) %>%
  filter(`V-Rel Pt` >= 4.4) ## No submarines

allsl$vert_norm <- abs((allsl$`V-Mov` - mean(allsl$`V-Mov`)) / sd(allsl$`V-Mov`))
allsl$va_norm <- abs((allsl$VAA - mean(allsl$VAA)) / sd(allsl$VAA))
allsl$rpm_norm <- abs((allsl$RPM - mean(allsl$RPM)) / sd(allsl$RPM))
allsl$horiz_norm <- abs((allsl$`H-Mov` - mean(allsl$`H-Mov`)) / sd(allsl$`H-Mov`))
allsl$gyro_norm <- abs((allsl$active_spin_slider - mean(allsl$active_spin_slider, na.rm = TRUE)) / sd(allsl$active_spin_sliders, na.rm = TRUE))
allsl$mph_norm <- abs((allsl$MPH - mean(allsl$MPH)) / sd(allsl$MPH))

mean(allsl$`H-Mov`)

allsl$total <- sqrt(abs(allsl$vert_norm + allsl$horiz_norm + allsl$rpm_norm + allsl$mph_norm))
allsl$total <- ifelse(allsl$RPM >= 2480, allsl$total + allsl$rpm_norm, allsl$total)
allsl$percentile <- cut(allsl$total, breaks = quantile(allsl$total, probs = seq(0, 1, by = 0.01), na.rm = TRUE), labels = FALSE)
allsl$plus <- round((allsl$total / mean(allsl$total, na.rm = TRUE)) * 100)

allsl1 <- allsl %>% 
  select(full_name,Pitch_Type ,Pitches, MPH, RPM, active_spin_slider, 
         `V-Mov`, `H-Mov`, VAA, plus, wOBA, Usage., Hard.) %>% 
  mutate(gyro = active_spin_slider) %>% 
  select(-active_spin_slider)

## Changeup ##

ch <- dashboard %>% 
  filter(Pitch_Type == "CH")

spinch <- spin %>% 
  select(full_name, active_spin_changeup)

allch <- merge(ch, spinch, by = "full_name")

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
         `V-Mov`, `H-Mov`, `V-Rel Pt`, `Ext (ft)`, `H-Rel Pt`, 
         wOBA, VAA, Usage., Hard.) %>%
  filter(`V-Rel Pt` >= 4.4) ## No submarines

allch$vert_norm <- abs((allch$`V-Mov` - mean(allch$`V-Mov`)) / sd(allch$`V-Mov`))
allch$va_norm <- abs((allch$VAA - mean(allch$VAA)) / sd(allch$VAA))
allch$rpm_norm <- abs((allch$RPM - mean(allch$RPM)) / sd(allch$RPM))
allch$horiz_norm <- abs((allch$`H-Mov` - mean(allch$`H-Mov`)) / sd(allch$`H-Mov`))
allch$gyro_norm <- abs((allch$active_spin_changeup - mean(allch$active_spin_changeup, na.rm = TRUE)) / sd(allch$active_spin_changeup, na.rm = TRUE))
allch$mph_norm <- abs((allch$MPH - mean(allch$MPH)) / sd(allch$MPH))

allch$total <- sqrt(abs(allch$vert_norm + allch$gyro_norm + allch$horiz_norm + allch$mph_norm))
allch$total <- ifelse(allch$RPM > 2000, allch$total + allch$rpm_norm, allch$total)
allch$percentile <- cut(allch$total, breaks = quantile(allch$total, probs = seq(0, 1, by = 0.01), na.rm = TRUE), labels = FALSE)
allch$plus <- round((allch$total / mean(allch$total, na.rm = TRUE)) * 100)

allch1 <- allch %>% 
  select(full_name,Pitch_Type ,Pitches, MPH, RPM, active_spin_changeup, 
         `V-Mov`, `H-Mov`, VAA, plus, wOBA, Usage., Hard.) %>% 
  mutate(gyro = active_spin_changeup) %>% 
  select(-active_spin_changeup)

## Curveballs ##

cb <- dashboard %>% 
  filter(Pitch_Type == "CU")

spincb <- spin %>% 
  select(full_name, active_spin_curve)

allcb <- merge(cb, spincb, by = "full_name")  
  
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
         `V-Mov`, `H-Mov`, `V-Rel Pt`, `Ext (ft)`, `H-Rel Pt`
         , wOBA, VAA, Usage., Hard.) %>%
  filter(`V-Rel Pt` >= 4.4) ## No submarines

allcb$vert_norm <- abs((allcb$`V-Mov` - mean(allcb$`V-Mov`)) / sd(allcb$`V-Mov`))
allcb$va_norm <- abs((allcb$VAA - mean(allcb$VAA)) / sd(allcb$VAA))
allcb$rpm_norm <- abs((allcb$RPM - mean(allcb$RPM)) / sd(allcb$RPM))
allcb$horiz_norm <- abs((allcb$`H-Mov` - mean(allcb$`H-Mov`)) / sd(allcb$`H-Mov`))
allcb$gyro_norm <- abs((allcb$active_spin_curve - mean(allcb$active_spin_curve, na.rm = TRUE)) / sd(allcb$active_spin_curve, na.rm = TRUE))
allcb$mph_norm <- abs((allcb$MPH - mean(allcb$MPH)) / sd(allcb$MPH))


allcb$total <- sqrt(abs(allcb$vert_norm + allcb$mph_norm + allcb$horiz_norm + allcb$rpm_norm))
allcb$percentile <- cut(allcb$total, breaks = quantile(allcb$total, probs = seq(0, 1, by = 0.01), na.rm = TRUE), labels = FALSE)
allcb$plus <- round((allcb$total / mean(allcb$total, na.rm = TRUE)) * 100)

allcb1 <- allcb %>% 
  select(full_name, Pitch_Type ,Pitches, MPH, RPM, active_spin_curve, 
         `V-Mov`, `H-Mov`, VAA, plus, wOBA, Usage., Hard.) %>% 
  mutate(gyro = active_spin_curve) %>% 
  select(-active_spin_curve)

## Splitter ##

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
         `H-Mov`, `V-Rel Pt`, `Ext (ft)`, 
         `H-Rel Pt`, wOBA, VAA, Usage., Hard.) %>%
  filter(`V-Rel Pt` >= 4.4)

allsp$vert_norm <- abs((allsp$`V-Mov` - mean(allsp$`V-Mov`)) / sd(allsp$`V-Mov`))
allsp$va_norm <- abs((allsp$VAA - mean(allsp$VAA)) / sd(allsp$VAA))
allsp$rpm_norm <- abs((allsp$RPM - mean(allsp$RPM)) / sd(allsp$RPM))
allsp$horiz_norm <- abs((allsp$`H-Mov` - mean(allsp$`H-Mov`)) / sd(allsp$`H-Mov`))
allsp$mph_norm <- abs((allsp$MPH - mean(allsp$MPH)) / sd(allsp$MPH))

allsp$total <- sqrt(abs(allsp$vert_norm + allsp$horiz_norm))
allsp$total <- ifelse(allsp$MPH >= 88, allsp$mph_norm + allsp$total, allsp$total)
allsp$total <- ifelse(allsp$RPM <= 1400, allsp$total + allsp$rpm_norm, allsp$total)
allsp$percentile <- cut(allsp$total, breaks = quantile(allsp$total, probs = seq(0, 1, by = 0.01), na.rm = TRUE), labels = FALSE)
allsp$plus <- round((allsp$total / mean(allsp$total, na.rm = TRUE)) * 100)

allsp1 <- allsp %>% 
  select(full_name, Pitch_Type ,Pitches, MPH, RPM, `V-Mov`, `H-Mov`, VAA, plus, wOBA, Usage., Hard.) %>% 
  mutate(gyro = NA)

# Bind Tables Together #

joined_table <- rbind(allcb1, allfb1, allch1, allsl1, allcu1, allsi1, allsp1)

joined_table <- joined_table %>% 
  arrange(desc(plus))

joined_table$MPH <- round(joined_table$MPH, 1)
joined_table$RPM <- round(joined_table$RPM, 0)
joined_table$`V-Mov` <- round(joined_table$`V-Mov`, 2)
joined_table$`H-Mov` <- round(joined_table$`H-Mov`, 2)
joined_table$VAA <- round(joined_table$VAA, 2)

dashboard$`H-Mov` <- dashboard$`H-Mov`*-1 ## Adjust statcast horizontal movement numbers

# Create handeness Variable #

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




joined_table <- merge(joined_table, rhp, by = "full_name")
dashboard <- merge(dashboard, rhp, by = "full_name")

# Create Average Movement for Shiny Graph #

avg_movements <- dashboard %>%
  group_by(hand, Pitch_Type) %>%
  summarise(avg_hmov = mean(`H-Mov`),
            avg_vmov = mean(`V-Mov`)) %>% 
  filter(!Pitch_Type %in% c("EP", "Enyel", "Yerry", "s", "FA", "FO", "SC")) %>% 
  na.omit()

# Save Files #

setwd("/Users/aidanbeilke/Desktop/Shiny App")

write.xlsx(joined_table, file = "joined_table.xlsx")
write.xlsx(dashboard, file = "dashboard.xlsx")
write.xlsx(avg_movements, file = "avg_movements.xlsx")
