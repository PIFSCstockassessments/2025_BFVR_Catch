# global.R - Load packages and data that are used across UI and server
# do some pre-processing of dataframes 

library(shiny)
library(bslib)
library(tidyverse)
library(data.table)
library(ggthemes)
library(plotly)

QT_trip <- fread(file.path( "data", "mrip_quantiles.csv")) %>%
        pivot_longer(cols = q90:max, names_to="quantiles", values_to="value")
QT_annual <- fread(file.path("data", "lamson_quantiles.csv")) %>%
        pivot_longer(cols = q90:max, names_to="quantiles", values_to="value")

# Get species codes and names for plots
SP.id <- QT_trip[,1:3]

FC <- fread(file.path("data", "Fisher_counts.csv")) %>%
        filter(cml_registr == "N")

F2 <- fread(file.path("data", "FRS_trips_annon.csv"))

# Read in CML catches and tidy
cml <- fread(file.path("data", "CML_catches.csv"))

cml.all.sp <- cml %>% 
  pivot_longer(cols = d7:s97, names_to = "species", values_to = "catch") %>% 
  mutate(type = "Commercial - CML reported") 

cml.all <- cml %>% 
  mutate(catch = d7,
          type = "Commercial - CML reported") %>% 
  select(c("year", "catch", "type"))

TC <- fread(file.path("data", "Total_catches_2024.csv"))

tc.all <- TC %>% 
  select(c("Year", "d7")) %>% 
  rename("year" = "Year", "catch" = "d7") %>%
  mutate(type = "2024 Assessment total catch",
  catch = as.numeric(catch)) %>%
  filter(year < 2023) 

tc.all.sp <- TC %>% 
  pivot_longer(cols = d7:s97, names_to = "species", values_to = "catch") %>% 
  rename("year" = "Year") %>%
  mutate(type = "2024 Assessment total catch",
  catch = as.numeric(catch)) %>%
  filter(year < 2023 & species != "d7") %>%
  left_join(SP.id, by = c("species" = "sp_frs_id"),
  relationship = "many-to-many") 

