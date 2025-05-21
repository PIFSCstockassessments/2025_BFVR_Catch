# global.R - Load packages and data that are used across UI and server
# do some pre-processing of dataframes 

library(shiny)
library(bslib)
library(tidyverse)
library(data.table)
library(ggthemes)
library(plotly)
library(scales)
library(reactable)
library(shinyjs)
library(ggimage)
library(ggplotify)
library(ggfun)
library(ggimage)
library(patchwork)
library(magick)


#source plotting functions
source(file.path("layered_barplot.r"))
source(file.path("acl_allocation_plot.r"))
source(file.path("old_acl_plot.r"))
source(file.path("new_acl_plot.r"))
source(file.path("legend.r"))

catch_colors <- c("Commercial - CML reported" = "#F09008FF", 
            "Commercial - CML unreported" = "#7868C0FF", 
            "Non-commercial - BFVR approach" = "#488820FF",
            "2024 Assessment total catch" = "grey")

# Load trip-level limits outputted from 02_MRIP_Interview_Catch.qmd
QT_trip <- fread(file.path("data", "mrip_quantiles.csv")) %>%
        pivot_longer(cols = q90:max, names_to="quantiles", values_to="value")
# Load annual-level limits from 03_Lamson_Annual_Catch.qmd
QT_annual <- fread(file.path("data", "lamson_quantiles.csv")) %>%
        pivot_longer(cols = q90:max, names_to="quantiles", values_to="value")

# Get species codes and names for plots
SP.id <- QT_trip[,1:3]

# Load fisher counts outputted from 01_Registered_fisher_list.qmd.
FC <- fread(file.path("data", "Fisher_counts.csv")) %>%
        filter(cml_registr == "N")

# Load FRS trip data outputted from 02_Annual_catch_from_BFVR.qmd
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

# Load the total catches from the 2024 stock assessment
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

