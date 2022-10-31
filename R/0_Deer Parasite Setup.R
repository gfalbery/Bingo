
# Rum red deer ####

library(tidyverse);library(reshape2);library(igraph); library(ggregplot); library(INLA); library(gsheet); library(magrittr)
library(readxl); library(fs)

GregDeer <- read.csv("Data/Deer.csv")

GregDeer %<>% 
  mutate(Date = lubridate::mdy(Date)) %>% 
  filter(!is.na(Date)) %>% 
  bind_rows(GregDeer %>% mutate(Date = lubridate::dmy(Date)) %>% filter(!is.na(Date))) %>% 
  mutate(Month = substr(Date, 6, 7) %>% as.numeric) %>% 
  filter(!((Season == "Summer") & (Month < 7)))

GregDeer %<>% rename(Strongyles.g = Strongylates.g)

GregDeer %<>% 
  mutate(Coccidia.Binary = as.numeric(EBovis.g>0|Eimeriasp.g>0)) %>% 
  mutate(Moniezia.Binary = as.numeric(Moniezia.g>0))

GregDeer %<>% 
  mutate(PelletCount = Pellet.No,
         SampleMass = ifelse(Season.No %in% 1:7, Pellet.Weight*Pellet.No, Pellet.Weight))

# Importing new data ####

"Data/Parasitology/DatabaseParasites" %>% list.files(full.names = T) %>% 
  map(read_xlsx) ->
  ParasiteList

names(ParasiteList) <- "Data/Parasitology/DatabaseParasites" %>% list.files

ParasiteRecode <- c(
  
  "2" = "Strongyles.g",
  "1" = "Nematodirus.g",
  "4" = "Capillaria.g",
  "8" = "Ec.g",
  "9" = "Dictyocaulus.g",
  
  "7" = "Moniezia", 
  # "6" = "Strongyloides",
  # "5" = "Trichuris", 
  "3" = "Coccidia"
  
)

# Strongyles ####

ParasiteList$tblFaecalParasiteCounts.xlsx %>% 
  mutate_at("CountB", ~ifelse(is.na(.x), 0, .x)) %>% 
  mutate(Parasite = recode(Taxon, !!!ParasiteRecode)) %>% 
  mutate(MeanCount = (CountA + CountB)*MultiplicationFactor) %>% 
  dplyr::select(CountRef, Parasite, MeanCount) %>% 
  pivot_wider(names_from = "Parasite", values_from = "MeanCount") %>% 
  left_join(ParasiteList$tblFaecalParasiteCountDetails.xlsx, by = "CountRef") ->
  FECCounts

ParasiteList$tblFaecalParasitePresence.xlsx %>% 
  mutate(Parasite = recode(Taxon, !!!ParasiteRecode)) %>% 
  filter(Taxon %in% names(ParasiteRecode)) %>% 
  dplyr::select(CountRef, Parasite, Present) %>% 
  pivot_wider(names_from = "Parasite", values_from = "Present") %>% 
  rename_at(c("Moniezia", "Coccidia"), ~paste0(.x, ".Binary")) %>% 
  mutate_at("Moniezia.Binary", ~ c(- .x)) %>% 
  left_join(ParasiteList$tblFaecalParasiteCountDetails.xlsx) ->
  FECPresence

ParasiteList$tblFaecalParasiteFlukeCounts.xlsx %>% 
  mutate(Flukes.g = (CountA + CountB)/FlukeCountMass) %>% 
  dplyr::select(FaecalRef, Flukes.g) ->
  FlukeCounts

ParasiteList$tblFaecalParasiteLarvalCounts.xlsx %>% 
  mutate(Parasite = recode(LarvalTaxon, !!!ParasiteRecode)) %>% 
  dplyr::select(LarvalCountRef, Parasite, LarvalCount) %>% 
  left_join(ParasiteList$tblFaecalParasiteLarvalCountDetails.xlsx) %>% 
  mutate_at("LarvalCount", ~.x/LarvalCountMass) %>% 
  pivot_wider(names_from = "Parasite", values_from = "LarvalCount") %>% 
  dplyr::select(FaecalRef, Ec.g, Dictyocaulus.g) ->
  LarvalCounts

list(ParasiteList$tblFaecalSamples.xlsx,
     FECCounts, FECPresence,
     FlukeCounts, LarvalCounts) %>% 
  reduce(full_join) -> 
  
  FullCountDF

FullCountDF %<>% filter(is.na(Counter) | Counter != "GFA")

# Initial phenotyping ####

FullCountDF %>% 
  mutate_at("SampleDate", ~.x %>% str_split(" ") %>% map_chr(1)) %>% 
  separate(SampleDate, "-", into = c("Year", "Month", "Day")) %>% 
  mutate_at(c("Year", "Month", "Day"), as.numeric) %>% 
  mutate(Season = case_when(Month %in% c(4:6) ~ "Spring",
                            Month %in% c(7:9) ~ "Summer",
                            Month %in% c(10:12) ~ "Autumn") %>% 
           factor(levels = c("Summer", "Autumn", "Spring"))) %>% 
  mutate_at("Year", ~ifelse(Season == "Spring", .x - 1, .x)) %>% 
  arrange(Year, Season) %>% #dplyr::select(Season, Year) %>% 
  mutate(Season.No = paste0(Year, Season) %>% 
           factor(levels = unique(paste0(Year, Season))) %>% 
           as.numeric %>% add(max(GregDeer$Season.No, na.rm = T))) ->
  
  FullCountDF

FullCountDF %<>% rename(Name = Code)

FullCountDF %<>% mutate(Date = lubridate::ymd(paste(Year, Month, Day, sep = "-")))

FullCountDF %<>% arrange(FaecalRef)

# Antibodies ####

NewAntibodies <- 
  "Data/Immunology/Antibody Updates/Dave Antibodies November 2021" %>% 
  dir_ls() %>% 
  map(read_xlsx) %>% 
  map(~.x %>% dplyr::select(Name = Animal, TotA = `Total IgA`, 
                            TcA = `T. circ IgA`,
                            TotG = `Total IgG`,
                            TcG = `T. circ IgG`) %>% 
        mutate_at(2:5, as.numeric)) %>% 
  bind_rows(.id = "Season") %>% 
  mutate_at("Season", ~str_split(.x, "/") %>% map_chr(last)) %>% 
  mutate(Year = Season %>% str_split(" ") %>% map_chr(2),
         Season = Season %>% str_split(" ") %>% map_chr(1))

NewAntibodies %<>% 
  mutate_at("Year", ~.x %>% as.numeric() %>% add(2000)) %>% 
  mutate_at("Season", ~.x %>% str_replace_all(c("Apr" = "Spring",
                                                "Aug" = "Summer", 
                                                "Nov" = "Autumn")))

NewAntibodies %<>% mutate_at("Year", ~ifelse(Season == "Spring", .x - 1, .x))

FullCountDF %<>% left_join(NewAntibodies, by = c("Year", "Season", "Name"))

# Joining together ####

GregDeer %>% colnames %>% 
  intersect(colnames(FullCountDF)) %>% c("Sample", "Date", .) %>% 
  dplyr::select(GregDeer, .) %>% 
  bind_rows(FullCountDF) ->
  FullCountDF

FullCountDF %<>% mutate(PelletWeight = SampleMass/PelletCount)

Parasites <- ParasiteRecode[1:3] %>% c("Flukes.g", ParasiteRecode[4:5]) %>% 
  c("Coccidia.Binary", "Moniezia.Binary")

names(Parasites) <- NULL

if(0){
  
  Deer %>% group_by(AgeCat) %>% 
    summarise_at(Parasites, Prev) %>% filter(AgeCat == "A") %>% 
    gather("Parasite", "Prevalence", -AgeCat) %>% 
    filter(Prevalence>0.1) %>% pull(Parasite) -> 
    AdultParasites
  
  Deer %>% group_by(AgeCat) %>% 
    summarise_at(Parasites, Prev) %>% filter(AgeCat == "Y") %>% 
    gather("Parasite", "Prevalence", -AgeCat) %>% 
    filter(Prevalence>0.1) %>% pull(Parasite) -> 
    YearlingParasites
  
  Deer %>% group_by(AgeCat) %>% 
    summarise_at(Parasites, Prev) %>% filter(AgeCat == "C") %>% 
    gather("Parasite", "Prevalence", -AgeCat) %>% 
    filter(Prevalence>0.1) %>% pull(Parasite) -> 
    CalfParasites
  
}

FullCountDF %>% saveRDS("Intermediate/FullCountDF.rds")

