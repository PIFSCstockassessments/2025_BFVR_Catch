require(pacman,quietly=T)
pacman::p_load("tidyverse","data.table","this.path","readxl","ggthemes")
root_dir <- here(..=1)

#============User inputs=================================
# Multiplier on the number of active BFVR non-commercial boats, to take into
# account the percentage of unregistered boats.
# Ranges from 25%-100% in app but for this script range from .25-1
percent_registered <- 90

# Multiplier to BF-registered boats to remove inactive vessels
# (i.e. not catching deep7) in a given year.
# Range 0-100%
percent_inactive <- 10

# FRS fishers to filter out
only_bf_registered  <- "Y"  # Only keep BF-registered fishers
#remove_nod7_catch   <- "Y"  # Only keep fishers that caught a deep7

# MRIP interview and Lamson annual quantile to filter at
selected_quantile <- c("q90","q95","q99","max")[3]

# What percentage of commercial catch is unreported
percent_unreported <- 0

# At which level should catch data be filtered, trip (MRIP), annual (Lamson),
# or both?
which_filter_level <- "Trip"
#=======================================================

#================Load data==============================
# Load trip-level limits outputted from 02_MRIP_Interview_Catch.qmd
# and select the quantile to use.
QT_trip <- fread(fs::path(root_dir,"03_Outputs","mrip_quantiles.csv")) %>% 
  pivot_longer(cols=q90:max,names_to="quantiles",values_to="value") 

# subset columns `sci_name`, `common_name`, and `sp_frs_id`
SP.id <- QT_trip[,1:3]

# Load annual-level limits from 03_Lamson_Annual_Catch.qmd
QT_annual <- fread(fs::path(root_dir,"03_Outputs","lamson_quantiles.csv")) %>% 
  pivot_longer(cols=q90:max,names_to="quantiles",values_to="value") 

# Load fisher counts outputted from 01_Registered_fisher_list.qmd.

FC <- fread(fs::path(root_dir,"03_Outputs", "Fisher_counts.csv")) %>% 
  filter(cml_registr == "N") 

# Load FRS trip data outputted from 02_Annual_catch_from_BFVR.qmd
F2 <- fread(fs::path(root_dir,"03_Outputs","FRS_trips_annon.csv"))

# Load CML_catches
cml <- fread(fs::path(root_dir, "03_Outputs", "CML_catches.csv"))

# Load the total catches from the 2024 stock assessment
TC <- fread(file.path(root_dir, "03_Outputs", "Total_catches_2024.csv"))

#======================Modify the number of active BFVR vessels===========

FC$n_bf_fishers <- FC$n_bf_fishers - (FC$n_bf_fishers * percent_inactive/100)
FC$n_bf_fishers <- FC$n_bf_fishers + FC$n_bf_fishers*(1-percent_registered/100)

#=========================================================================

#===============Do some pre-processing of CML and TC dataframes============
# CML catch by species
cml.all.sp <- cml %>% 
  pivot_longer(cols = d7:s97, names_to = "species", values_to = "catch") %>% 
  mutate(type = "Commercial - CML reported") 

# CML catch aggregated
cml.all <- cml %>% 
  mutate(catch = d7,
          type = "Commercial - CML reported") %>% 
  select(c("year", "catch", "type"))

# 2024 assessment catch aggregated
tc.all <- TC %>% 
  select(c("Year", "d7")) %>% 
  rename("year" = "Year", "catch" = "d7") %>%
  mutate(type = "2024 Assessment total catch",
  catch = as.numeric(catch)) %>%
  filter(year < 2023) 

# 2024 assessment catch by species
tc.all.sp <- TC %>% 
  pivot_longer(cols = d7:s97, names_to = "species", values_to = "catch") %>% 
  rename("year" = "Year") %>%
  mutate(type = "2024 Assessment total catch",
  catch = as.numeric(catch)) %>%
  filter(year < 2023 & species != "d7") %>%
  left_join(SP.id, by = c("species" = "sp_frs_id"),
  relationship = "many-to-many") 
#=========================================================================

#======================Filters================================
# Apply trip-level filters to classify fishers as comm. (1) vs non-comm. (0).
F2$trip_type <- 0

if(which_filter_level == "Trip" | which_filter_level == "Both"){
  QT_trip <- QT_trip %>% 
  filter(quantiles==selected_quantile) %>% 
  as.data.table() 
  F2[d7  > QT_trip[sp_frs_id=="d7"]$value|
    s17 > QT_trip[sp_frs_id=="s17"]$value| 
    s19 > QT_trip[sp_frs_id=="s19"]$value| 
    s21 > QT_trip[sp_frs_id=="s21"]$value|
    s22 > QT_trip[sp_frs_id=="s22"]$value|
    s97 > QT_trip[sp_frs_id=="s97"]$value| 
    s20 > QT_trip[sp_frs_id=="s20"]$value 
    ]$trip_type <- 1
}
# Sum catches from trip-level data to annual-level per fisher.
F3 <- F2 %>% group_by(year, cml_no.fs, bf_registr, cml_registr, county) %>%
  summarize_if(is.numeric, sum)

# Re-classify "trip_type" into "fisher_type".
F3 <- F3 %>% mutate(fisher_type=if_else(trip_type > 0,"Comm","NC")) %>%
  select(-trip_type) %>% as.data.table()

# Apply the annual-level filters to classify fishers as comm. vs non-comm.
F3$annual_type <- "NC"

if(which_filter_level == "Annual" | which_filter_level == "Both"){
  QT_annual <- QT_annual %>% 
      filter(quantiles == selected_quantile) %>% as.data.table()
  F3[d7  > QT_annual[sp_frs_id=="d7"]$value|
    s17 > QT_annual[sp_frs_id=="s17"]$value| 
    s19 > QT_annual[sp_frs_id=="s19"]$value| 
    s21 > QT_annual[sp_frs_id=="s21"]$value|
    s22 > QT_annual[sp_frs_id=="s22"]$value|
    s97 > QT_annual[sp_frs_id=="s97"]$value| 
    s20 > QT_annual[sp_frs_id=="s20"]$value 
    ]$annual_type <- "Comm"
}

F3 <- F3 %>% mutate(fisher_type=if_else(annual_type=="Comm","Comm",fisher_type))

# Apply filters on the fishers.
if(only_bf_registered=="Y"){ F3 <- F3 %>% filter(bf_registr=="Y") }
#if(remove_nod7_catch=="Y") { F3 <- F3 %>% filter(d7!=0) }

F3 <- F3 %>% filter(fisher_type=="NC")
#==========================================================

#======================Sampling the annual catch===========
# Add the # of NC vessels by year x County to the catch data.
F3 <- F3 %>% left_join(FC,by=join_by(year,county), relationship = "many-to-many") %>% 
                relocate(n_bf_fishers,.after=county)

# Sample "n" times from the annual catch data set, where "n" is the number of
# non-commercial BF fishers in a given Year x County.

Results <- list()
set.seed(1234)
for(i in 1:200){
 aSample <- F3 %>% group_by(year,county) %>% 
                    sample_n(n_bf_fishers[1],replace=T) %>% 
                      add_column(iter=i) %>% relocate(iter,.before=year)
 Results <- append(Results,list(aSample))
}

# Bind results together and sum fisher-specific catch by iteration.
Results      <- rbindlist(Results)
Final.County <- Results %>% group_by(iter,year,county) %>%
                 summarize_at(vars(s15:allsp),sum)


#=================Plots=====================================
# Plot County-level results.
ggplot(data=Final.County,aes(x=factor(year),y=d7))+
  geom_boxplot(fill="lightblue",alpha=0.6,outlier.size=0.6)+
  facet_wrap(~county)+theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(x="Year",y="Deep7 non-commercial catch (lbs)")

# Now see global results.
Final.all <- Final.County %>% group_by(iter,year) %>% 
  summarize_at(vars(s15:allsp),sum)

# Some species specific global results.
Final.all.sp <- Final.all %>% pivot_longer(
                cols=s15:s97,names_to="species",values_to="lbs_caught") %>%
                  #filter(species %in% c("s19","s21","s22")) %>%
                     select(iter,year,species,lbs_caught)

non_commercial_data <- Final.all.sp %>%
  group_by(year, species) %>% 
  summarise(catch = quantile(lbs_caught, .5)) %>% 
  mutate(type = "Non-commercial - BFVR approach") %>% 
  filter(year < 2023)

if (percent_unreported > 0) {
  # Create data with both regular CML and unreported CML and NC
  local_cml_all_sp <- cml.all.sp %>%
    mutate(catch = (catch * options$prop_unreported) + (non_commercial_data$catch * options$prop_unreported),
            type = "Commercial - CML unreported") %>% 
    bind_rows(cml.all.sp)
  
  # Combine all data
  plot_data_sp <- bind_rows(non_commercial_data, local_cml_all_sp)
} else {
  # Only include the regular CML data (no unreported)
  plot_data_sp <- bind_rows(non_commercial_data, cml.all.sp)
}

# Set type of catch as a factor to control order of bars in plot
# and set which colors to use for each type
colors <- c("Commercial - CML reported" = "#F09008FF", 
            "Commercial - CML unreported" = "#7868C0FF", 
            "Non-commercial - BFVR approach" = "#488820FF",
            "2024 Assessment total catch" = "grey")

# Get all unique years from the data to use as breaks
all_years <- sort(unique(plot_data_sp$year))

plot_data_sp <- plot_data_sp %>%
  left_join(SP.id, by = c("species" = "sp_frs_id"), 
            relationship = "many-to-many") %>% 
  filter(species != "d7") %>% 
  mutate(type = factor(type, levels = c("Commercial - CML reported", 
  "Commercial - CML unreported", "Non-commercial - BFVR approach"))) %>%
  distinct(year, species, type, .keep_all = TRUE) 

# See 04_Shiny/server.r for plotly style code
ggplot(data = plot_data_sp)+
  geom_bar(aes(x = year, y=catch, group = type, fill = type),
            position="stack", stat="identity") +
  geom_line(data = tc.all.sp, aes(x = year, y = catch, color = type), linewidth = 2) + 
  geom_point(data = tc.all.sp, aes(x = year, y = catch, color = type), size = 3) +
  theme_minimal()+
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) + 
  theme(axis.text.x=element_text(angle=45,hjust=1),
        strip.text = element_text(size = 14))+
  labs(y="Year",x="Catch (lbs)")+
  facet_wrap(~common_name,ncol=3,scales="free_y")

# Deep7 global results.
# See 04_Shiny/server.r for plotly style code
# First create the data for Non-commercial - BFVR approach
non_commercial_data_all <- Final.all %>%
  group_by(year) %>% 
  summarise(catch = quantile(d7, .5)) %>% 
  mutate(type = "Non-commercial - BFVR approach") %>%
  filter(year < 2023)
  
# Decide whether to include unreported CML based on proportion
if (percent_unreported > 0) {
  # Create data with both regular CML and unreported CML and NC
  local_cml_all <- cml.all %>%
  mutate(catch = (catch * options$prop_unreported) + (non_commercial_data_all$catch * options$prop_unreported), 
  type = "Commercial - CML unreported") %>%
    bind_rows(cml.all)
  
  # Combine all data
  plot_data_all <- bind_rows(non_commercial_data_all, local_cml_all)
} else {
  # Only include the regular CML data (no unreported)
  plot_data_all <- bind_rows(non_commercial_data_all, cml.all) 
}

# Set type of catch as a factor to control order of bars in plot
# and set which colors to use for each type
plot_data_all <- plot_data_all %>%
  mutate(type = factor(type, 
  levels = c("Commercial - CML reported", "Commercial - CML unreported",
  "Non-commercial - BFVR approach")))
 
ggplot(data = plot_data_all)+
  geom_bar(aes(x = year, y=catch, group = type, fill = type),
            position="stack", stat="identity") +
  geom_line(data = tc.all, aes(x = year, y = catch, color = type), linewidth = 2) + 
  geom_point(data = tc.all, aes(x = year, y = catch, color = type), size = 3) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        strip.text = element_text(size = 14))+
  labs(y="Year",x="Catch (lbs)")+
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors)
      