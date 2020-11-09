install.packages("tidyverse")
install.packages("readxl")
install.packages("lubridate")
install.packages("purrr")
install.packages("eeptools")
library(tidyverse)
library(readxl)
library(lubridate)
library(purrr)
library(eeptools)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))

df <- list.files(pattern = "*.xlsx")
df <- lapply(df, function(x) read_excel(x, skip = 1))
df <- bind_rows(df, .id = "id")

#import retirement factor lookup key into GE
factorlookup <- read_csv("https://raw.githubusercontent.com/matthanc/successionplanning/main/factorlookup.csv")

#change date formatting using lubridate and purrr libraries 
df <-  df %>% modify_if(is.POSIXct, as.Date)

#calculate age and service years to nearest 0.25
df <- df %>%
  mutate(age = (round((age_calc(df$Birthdate, enddate = today(), units = "years"))*4))/4) %>%
  mutate(service_years = (round((age_calc(df$`Start Date`, enddate = today(), units = "years"))*4))/4)

#apply retirement factor to df and create RetirementFactor variable
df <- df %>%
  left_join(factorlookup, by = c(age = 'key')) %>%
  rename(retirementfactor = value) %>%
  mutate(retirement_formula = pmin((service_years * retirementfactor),0.75))

#import retirement model into environment
load(url("https://github.com/matthanc/successionplanning/blob/main/retirementmodel.rda?raw=true"))

df <- df %>% mutate(retirementmodel = predict(retirementmodel, newdata = df, type = "response"))


df <- df %>%
  mutate("Likelihood to Retire" = ifelse(retirementmodel < 0.001, "Very Low", ifelse(retirementmodel < 0.01, "Low", ifelse(retirementmodel < 0.30, "Moderate", ifelse(retirementmodel < 0.70, "High", ifelse(retirementmodel >= 0.70, "Very High", NA))))))

#write csv file to folder
df <- df %>%
  rename(Age = age) %>%
  rename("Service Years" = service_years) %>%
  rename("Retirement Factor (multiplier)" = retirementfactor) %>%
  rename("Retirement Formula" = retirement_formula) %>%
  rename("Retirement Model Score" = retirementmodel)


write_excel_csv(df,"successionplanninganalysis.csv")
