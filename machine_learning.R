library(tidyverse)
library(Boruta)
library(caret)
library(randomForest)

ml <- all %>% 
  select(full_name,Pitch_Type ,vert_norm, va_norm, rpm_norm, horiz_norm, gyro_norm, mph_norm, wOBA, Year) %>% 
  na.omit() %>% 
  group_by(full_name, Pitch_Type, Year) %>% 
  summarise(vert_norm = mean(vert_norm, na.rm = T),
            va_norm = mean(va_norm, na.rm = T),
            rpm_norm = mean(rpm_norm, na.rm = T),
            horiz_norm = mean(horiz_norm, na.rm = T),
            gyro_norm = mean(gyro_norm, na.rm = T),
            mph_norm = mean(mph_norm, na.rm = T),
            wOBA = mean(wOBA, na.rm = T)) 

# Feature Selection
set.seed(123)
boruta <- Boruta(wOBA ~ vert_norm + va_norm + rpm_norm + horiz_norm + gyro_norm + mph_norm, data = ml, doTrace = 2)

plot(boruta, las = 2, cex.axis = 0.5)
plotImpHistory(boruta)
getNonRejectedFormula(boruta)

# Data Partition
set.seed(222)
train <- subset(ml, Year %in% c(2021, 2022))
test <- subset(ml, Year == 2023)

# Random Forest Model
set.seed(333)
rf10 <- randomForest(wOBA ~ vert_norm + va_norm + rpm_norm + horiz_norm + gyro_norm + mph_norm, data = train)

p <- predict(rf10, test)

test$unique_pred <- p

test$unique_pred <- (mean(test$unique_pred) / test$unique_pred) * 100

