require(pacman,quietly=T)
pacman::p_load("tidyverse","data.table","this.path","readxl","ggthemes")
root_dir <- here(..=1)

#============User inputs=================================
# Assumed proportion of BF-registered boats that caught at least one D7 
# in a given year.
prop_caught_d7 <- 1

# FRS fishers to filter out
only_bf_registered  <- "Y"  # Only keep BF-registered fishers
remove_nod7_catch   <- "Y"  # Only keep fishers that caught a deep7
min_d7_catch        <- 0    # Only keep fishers that caught at least this D7 lbs.

# MRIP interview quantile to filter at
selected.quantile <- c("q90","q95","q99","max")[3]
#=======================================================

#================Load data==============================
# Load trip-level limits outputted from 04_MRIP_Interview_Catch.qmd
# and select the quantile to use.
QT <- fread(fs::path(root_dir,"03_Outputs","mrip_quantiles.csv")) %>% 
  pivot_longer(cols=q90:max,names_to="quantiles",values_to="value") %>% 
  filter(quantiles==selected.quantile) %>% as.data.table()

SP.id <- QT[,sci_name:sp_frs_id]

# Load fisher counts outputted from 01_Registered_fisher_list.qmd.
# Appy the correction related to the assume # of BF-registered fishers catching
# at least one deep7 in any given year.
FC <- fread(fs::path(root_dir,"03_Outputs", "Fisher_counts.csv")) %>% 
  filter(cml_registr == "N") %>% 
  mutate(n_bf_fishers=n_bf_fishers*prop_caught_d7)

# Load FRS trip data outputted from 02_Annual_catch_from_BFVR.qmd
F2 <- fread(fs::path(root_dir,"03_Outputs","FRS_trips.csv"))

# Load CML_catches
cml <- fread(fs::path(root_dir, "03_Outputs", "CML_catches.csv"))

#======================Filters================================
# Apply trip-level filters to classify fishers as comm. (1) vs non-comm. (0).
F2$trip_type <- 0

F2[d7  > QT[sp_frs_id=="d7"]$value|
   s17 > QT[sp_frs_id=="s17"]$value| 
   s19 > QT[sp_frs_id=="s19"]$value| 
   s21 > QT[sp_frs_id=="s21"]$value|
   s22 > QT[sp_frs_id=="s22"]$value|
   s97 > QT[sp_frs_id=="s97"]$value| 
   s20 > QT[sp_frs_id=="s20"]$value 
   ]$trip_type <- 1

# Sum catches from trip-level data to annual-level per fisher.
F3 <- F2 %>% group_by(year, cml_no.fs, bf_registr, cml_registr, county) %>%
  summarize_if(is.numeric, sum)

# Re-classify "trip_type" into "fisher_type".
F3 <- F3 %>% mutate(fisher_type=if_else(trip_type > 0,"Comm","NC")) %>%
  select(-trip_type)

# Apply filters on the fishers.
if(only_bf_registered=="Y"){ F3 <- F3 %>% filter(bf_registr=="Y") }
if(remove_nod7_catch=="Y") { F3 <- F3 %>% filter(d7!=0) }

F3 <- F3 %>% filter(d7>=min_d7_catch)
F3 <- F3 %>% filter(fisher_type=="NC")
#==========================================================

#======================Sampling the annual catch===========
# Add the # of NC vessels by year x County to the catch data.
F3 <- F3 %>% left_join(FC,by=join_by(year,county)) %>% 
                relocate(n_bf_fishers,.after=county)

# Sample "n" times from the annual catch data set, where "n" is the number of
# non-commercial BF fishers in a given Year x County.

Results <- list()
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

cml.all.sp <- cml %>% 
pivot_longer(cols = d7:s97, names_to = "species", values_to = "catch") %>% 
mutate(type = "CML")

cml.all <- cml %>% 
mutate(catch = d7,
type = "CML") %>% 
select(c("year", "catch", "type"))

Final.all.sp %>%
group_by(year, species) %>% 
summarise(catch = quantile(lbs_caught, .5)) %>% 
mutate(type = "BFVR Non-Commercial") %>% 
bind_rows(cml.all.sp) %>% 
left_join(SP.id, by = c("species" = "sp_frs_id")) %>% 
filter(species != "d7") %>% 
ggplot()+
      geom_bar(aes(x = year, y=catch, group = type, fill = type), position="stack", stat="identity") +
      theme_minimal()+
      theme(axis.text.x=element_text(angle=45,hjust=1),
            strip.text = element_text(size = 14))+
      labs(y="Year",x="Catch (lbs)")+
      facet_wrap(~common_name,ncol=3)

# Deep7 global results.
Final.all %>%
group_by(year) %>% 
summarise(catch = quantile(d7, .5)) %>% 
mutate(type = "BFVR Non-Commercial") %>% 
bind_rows(cml.all) %>% 
ggplot()+
      geom_bar(aes(x = year, y=catch, group = type, fill = type), position="stack", stat="identity") +
      theme_minimal()+
      theme(axis.text.x=element_text(angle=45,hjust=1),
            strip.text = element_text(size = 14))+
      labs(x="Year",y="Catch (lbs)")
      

