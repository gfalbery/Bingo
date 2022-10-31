
# 1_Deer Phenotyping ####

rm(list = ls())

library(tidyverse);library(reshape2);library(igraph); library(ggregplot); library(INLA); library(gsheet); library(magrittr)
library(readxl); library(cowplot); library(adehabitatHR)

theme_set(theme_cowplot())

"Data/Phenotypes" %>% 
  list.files(full.names = T) %>% 
  map(read_xlsx) ->
  PhenotypeList

names(PhenotypeList) <- "Data/Phenotypes" %>% list.files

# Get these directly from Deer Main access file
Individuals <- PhenotypeList$tblLife.xlsx
Names <- PhenotypeList$tblNames.xlsx

# Get these from queries ####
# Main page queries
HindStatus <- PhenotypeList$sys_HindStatusByYear
LBS <- PhenotypeList$LBS.xlsx

# Auxiliary queries
PopSize <- PhenotypeList$PopSize.xlsx
BirthWts <- PhenotypeList$sys_BirthWt.xlsx

BirthDates <- Individuals %>% 
  mutate_at(vars(BirthDay:DeathYear), 
            ~factor(as.factor(.x),levels = c(0:10000, paste(0, 1:10, sep=""))) %>% 
              str_pad(2, "left", 0)) %>%
  mutate_at(c("BirthYear", "DeathYear"), as.numeric) %>% 
  mutate(BirthDate = glue::glue("{BirthDay}/{BirthMonth}/{BirthYear}") %>% 
           lubridate::dmy()) %>% 
  group_by(BirthYear) %>% 
  mutate(cBirthDate = BirthDate - median(BirthDate, na.rm = T)) %>% 
  ungroup %>% 
  dplyr::select(Code, BirthDate, cBirthDate)

#Individuals %<>% 
#  left_join(Names, by = "Code") %>% 
#  mutate_at(vars(contains("Name")), as.character) %>% 
#  mutate_at("GivenName", ~ifelse(is.na(.x), FamilyName, .x)) %>% 
#  mutate(Animal = GivenName) %>% # Dates
#  mutate_at("Sex", ~cut(.x,
#                        breaks = c(0:3 + 0.5),
#                        labels = c("F","M","3")))

Individuals %<>% rename(DeathStatus = Status)

# Getting every individual deer:year combo ####

DeerNames <- unique(Individuals$Code)

# Censuses <- read.csv(paste0(Root, "/Behaviour/FullCensuses.csv"))
Censuses <- read_xlsx(paste0("Data", "/Behaviour/tblCensus.xlsx"))

Censuses %>% separate(Date, "-", into = c("Year", "Month", "Day")) %>% 
  dplyr::select(c("Day", "Month", "Year")) %>% 
  mutate_all(as.numeric) ->
  Censuses[,c("Day", "Month", "Year")]

Censuses %<>% mutate(DeerYear = ifelse(Month<5, Year - 1, Year))

Censuses %>% 
  filter(Code %in% DeerNames) %>% 
  dplyr::select(Name = Code, Year = DeerYear) %>% unique %>% 
  arrange(Name, Year) -> IDYearDF

IDYearDF %<>% left_join(Individuals, by = c("Name" = "Code"))

IDYearDF %<>% 
  mutate(Age = Year - BirthYear) %>% 
  mutate(Hind = as.numeric(Age>2 & Sex == 1)) %>% 
  mutate(AgeCat = cut(Age, breaks = c(0, 0.5, 1.5, 2.5, Inf), 
                      labels = c("C", "Y", "2Y", "A"), include.lowest = T))

IDYearDF %<>% 
  left_join(Names, by = c("Name" = "Code")) %>% 
  mutate_at(vars(contains("Name")), as.character) %>% 
  mutate(Animal = ifelse((Age<3)|(Sex == 2), FamilyName, GivenName)) %>% 
  mutate_at("Animal", ~ifelse(is.na(.x)&is.na(FamilyName), GivenName, .x))

IDYearDF %<>% 
  mutate_at("Sex", ~cut(.x,
                        breaks = c(0:3 + 0.5),
                        labels = c("F","M","3")))

IDYearDF %<>% filter(Age > -1)

# Reproductive Status ####

HindStatus %<>% arrange(Female, DeerYear) %>%
  mutate(
    
    Reprod = as.numeric(!ReprodStatus %in% c("Naive", "True.Yeld", NA)),
    
    Winter = as.numeric(ReprodStatus %in% c("Winter.Yeld", "Milk"))
    
  ) %>%
  group_by(Female) %>%
  mutate(
    
    CumulativeReprod = cumsum(Reprod),
    CumulativeWinter = cumsum(Winter)
    
  ) %>% ungroup

# Method 1 ####

# HindStatus %>%
#   dplyr::select(Female, ReprodStatus.t0 = ReprodStatus, DeerYear, #Calf, 
#                 CumulativeReprod, CumulativeWinter) %>%
#   mutate_at("ReprodStatus.t0", ~.x %>% str_trim %>%
#               str_replace_all(c(" " = ".",
#                                 "Naïve" = "Naive",
#                                 "yeld" = "Yeld"))) %>%
#   left_join(IDYearDF, ., by = c("Name" = "Female", "Year" = "DeerYear")) ->
#   IDYearDF

# Method 2 ####

# NewStatus <- 
#   Individuals %>% dplyr::select(MumCode, Calf = Code, BirthYear, DeathYear, DeathMonth) %>% 
#   mutate_at(c("BirthYear", "DeathYear", "DeathMonth"), ~as.numeric(as.character(.x))) %>% 
#   mutate(ReprodStatus = case_when(
#     
#     is.na(BirthYear) | is.na(DeathYear) | 
#       DeathYear > (BirthYear + 1) | 
#       (DeathYear == (BirthYear + 1) & DeathMonth > 5) ~ "Milk",
#     
#     (DeathYear == (BirthYear + 1) & DeathMonth < 5) ~ "Winter.Yeld",
#     
#     ((DeathYear == BirthYear) & DeathMonth > 9) ~ "Winter.Yeld",
#     
#     ((DeathYear == BirthYear) & DeathMonth < 10) ~ "Summer.Yeld",
#     
#     
#     TRUE ~ "True.Yeld"
#     
#   )) %>% dplyr::select(-c(DeathYear, DeathMonth))
# 
# IDYearDF <- 
#   IDYearDF %>% 
#   # dplyr::select(Name, Year, ReprodStatus.t0) %>% 
#   left_join(NewStatus, by = c("Name" = "MumCode", "Year" = "BirthYear")) %>% 
#   as.data.frame

# Method 3 ####

HindStatus %>% 
  mutate_at("DeerYear", ~.x - 1) %>% 
  dplyr::select(Female, ReprodStatus, DeerYear) %>%
  mutate_at("ReprodStatus", ~.x %>% str_trim %>%
              str_replace_all(c(" " = ".",
                                "Naïve" = "Naive",
                                "yeld" = "Yeld"))) %>%
  left_join(IDYearDF, ., by = c("Name" = "Female", "Year" = "DeerYear")) ->
  IDYearDF

IDYearDF <- 
  Individuals %>% dplyr::select(Name = MumCode, Calf = Code, Year = BirthYear) %>% 
  left_join(IDYearDF, ., by = c("Name", "Year"))

# Cleaning up ####

IDYearDF %>% 
  filter(ReprodStatus %in% c("Winter.Yeld", "Summer.Yeld", "Milk")) %>% 
  group_by(Name) %>% filter(Age == min(Age)) %>% 
  dplyr::select(Name, AgeAtFirst = Age) %>% 
  left_join(IDYearDF, .) %>% 
  mutate_at("ReprodStatus", ~ifelse(is.na(.x) & Age < AgeAtFirst, "Naive", .x)) %>% 
  mutate_at("ReprodStatus", ~ifelse(is.na(.x), "True.Yeld", .x)) ->
  IDYearDF

IDYearDF %<>%
  mutate(Status = 
           
           case_when(
             
             Age == 0 ~ "Calf",
             Age == 1 ~ "Yearling", 
             Age == 2 ~ "2Y",
             Age>2 & Sex == "M" ~ "Stag", 
             TRUE ~ as.character(ReprodStatus)
             
           )
  )

IDYearDF %>% 
  left_join(PopSize, 
            by = c("Year" = "DeerYear")) %>% 
  left_join(LBS[,c("Code", "LRS","LBS")], by = c("Name" = "Code")) ->
  IDYearDF

IDYearDF %>% 
  dplyr::select(Name, Year, ReprodStatus, Age) %>% 
  mutate_at(c("Year", "Age"), ~.x + 1) %>% 
  left_join(IDYearDF, ., by = c("MumCode" = "Name", "BirthYear" = "Year"),
            suffix = c("", ".Mum")) ->
  IDYearDF

# Calf Traits ####

IDYearDF %<>% 
  left_join(BirthWts %>% dplyr::select(Code, BirthWt), 
            by = c("Name" = "Code"))

IDYearDF %<>% 
  left_join(BirthWts %>% dplyr::select(Code, BirthWt), 
            by = c("Calf" = "Code"),
            suffix = c("", ".Calf"))

IDYearDF %<>% 
  left_join(BirthDates, by = c("Name" = "Code")) %>% 
  left_join(BirthDates, by = c("Calf" = "Code"),
            suffix = c("", ".Calf"))

IDYearDF %<>% 
  left_join(Individuals %>% dplyr::select(Calf = Code, Sex),
            suffix = c("", ".Calf"), by = c("Calf"))

# Subsequent reproduction ####

ReprodCovar <- c("BirthWt", "cBirthDate") %>% paste0(".Calf") %>% c("ReprodStatus")

IDYearDF %>% 
  mutate_at("Year", ~.x - 1) %>% 
  dplyr::select(Name, Year, all_of(ReprodCovar)) %>% 
  left_join(IDYearDF, ., suffix = c("", ".t2"), by = c("Name", "Year")) ->
  IDYearDF

IDYearDF %>% 
  mutate_at("Year", ~.x + 1) %>% 
  dplyr::select(Name, Year, all_of(ReprodCovar)) %>% 
  left_join(IDYearDF, ., suffix = c("", ".t0"), by = c("Name", "Year")) ->
  IDYearDF

CurrentYear <- max(Censuses$DeerYear, na.rm = T)

IDYearDF %<>% 
  mutate(
    Pregnant = as.numeric(Hind == 1 & 
                            ReprodStatus.t2 %in% c("Summer.Yeld", "Winter.Yeld", "Milk"))
  ) %>% 
  mutate_at("Pregnant", ~ifelse(Year == CurrentYear, NA, .x))

IDYearDF %>% 
  mutate_at(vars(contains("ReprodStatus")), 
            ~factor(.x, levels = c("Naive","True.Yeld","Summer.Yeld","Winter.Yeld","Milk"))) ->
  
  IDYearDF

# Survival ####

HindStatus %>% 
  dplyr::select(Name = Calf, CalfSurvival = ReprodStatus) %>% 
  left_join(IDYearDF, .) ->
  IDYearDF

Censuses %>% 
  filter(Code %in% DeerNames) %>% 
  mutate_at("Date", ~lubridate::dmy(.x)) %>% 
  arrange(Date) %>% group_by(Code) %>% 
  slice(n()) %>% 
  dplyr::select(Name = Code, 
                Date, Day, Month, Year, DeerYear) %>% 
  rename_at(c("Date", "Day", "Month", "Year", "DeerYear"), ~paste0("LastSeen.", .x)) -> 
  LastSeenDates

CurrentYear <- max(Censuses$DeerYear, na.rm = T)

LastSeenDates %>% dplyr::select(Name, LastSeen.Date, LastSeen.DeerYear) %>% 
  left_join(IDYearDF, .) %>% 
  mutate(
    Survived0 = case_when(
      
      DeathStatus == "L" & Year == CurrentYear ~ "NA",
      LastSeen.DeerYear == Year ~ "0",
      TRUE ~ "1"
      
    )
  ) %>% 
  mutate(
    Survived1 = case_when(
      
      DeathStatus == "L" & Year == CurrentYear ~ "NA",
      LastSeen.DeerYear > Year ~ "1",
      TRUE ~ "0"
      
    )
  ) %>% 
  mutate(
    Survived2 = case_when(
      
      DeathStatus == "L" & Year >= CurrentYear - 1 ~ "NA",
      LastSeen.DeerYear > Year + 1 ~ "1",
      TRUE ~ "0"
      
    )
  ) ->
  IDYearDF

IDYearDF %<>% 
  mutate(Shot = 1 - as.numeric(DeathType != "S" | is.na(DeathType)))

# Adding Parasites ####

source("R/0_Deer Parasite Setup.R")

FullCountDF %<>% group_by(Year, Season, Name) %>%
  summarise_if(is.numeric, ~mean(.x, na.rm = T))

FullCountDF %>% anti_join(IDYearDF, by = c("Name", "Year")) ->
  NotPresent

FullCountDF %>% full_join(IDYearDF, ., by = c("Name", "Year")) ->
  Deer

Deer %<>% mutate(Survived = ifelse(Season == "Spring", Survived2, Survived1))

# Attaching Social Data ####

Deer %>% arrange(Year) %>% pull(Year) %>% unique %>% as.character %>% na.omit -> FocalYears

FocalYears %<>% c(min(as.numeric(FocalYears))-1, .) %>% as.numeric

FocalYears <- FocalYears[FocalYears > 2013]

source("R/0b_Social Network Setup.R")

Deer %<>% left_join(FullSociality, by = c("Name", "Year"))

# Deer <- IDYearDF %>% left_join(FullSociality, by = c("Name", "Year"))

# Density ####

# Overall Density ####

if(1){
  
  Censuses %>% 
    filter(Year %in% FocalYears) %>% 
    group_by(Code) %>% 
    summarise_at(c("Easting", "Northing"), ~mean(.x, na.rm = T)) ->
    LifetimeCentroids
  
  SPDF <- SpatialPointsDataFrame(data = LifetimeCentroids[,c("Easting", "Northing")], 
                                 coords = LifetimeCentroids[,c("Easting", "Northing")])
  
  LifetimeKUDL <- kernelUD(SPDF, same4all = TRUE, grid = 500)
  
  LifetimeKUDL %>% raster::raster() %>% raster::extract(Deer[,c("E", "N")]) ->
    
    Deer$LifetimeDensity
  
  # Annual ####
  
  Deer %>% arrange(Year) %>% pull(Year) %>% unique %>% as.character %>% na.omit -> FocalYears
  
  # FocalYears %<>% c(min(as.numeric(FocalYears))-1, .)
  
  Censuses %>% 
    filter(Year %in% FocalYears) %>% 
    group_by(Code, Year) %>% 
    summarise_at(c("Easting", "Northing"), 
                 ~mean(.x, na.rm = T)) %>% 
    rename(XCentroidAnnual = Easting, YCentroidAnnual = Northing) -> 
    
    AnnualCentroids
  
  AnnualCentroids %<>% filter(Year %in% FocalYears) %>% 
    na.omit
  
  SPDF <- SpatialPointsDataFrame(data = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual", "Year")], 
                                 coords = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual")])
  
  SPDF <- SPDF[,"Year"]
  
  KUDL <- kernelUD(SPDF, same4all=TRUE, grid=500)
  
  1:length(KUDL) %>% lapply(function(a){
    
    print(names(KUDL)[a])
    
    DF <- Deer %>% filter(Year == names(KUDL)[a])
    
    KUDL2 <- KUDL[[names(KUDL)[a]]]
    
    KUDL2 %>% raster::raster() %>% raster::extract(DF[,c("E", "N")]) ->
      
      DF$AnnualDensity
    
    return(DF)
    
  }) -> DensityList
  
  DensityList %>% bind_rows -> Deer
  
  2:length(names(KUDL)) %>% lapply(function(a){
    
    print(names(KUDL)[a])
    
    DF <- Deer %>% filter(Year == names(KUDL)[a])
    
    KUDL2 <- KUDL[[names(KUDL)[a - 1]]]
    
    KUDL2 %>% raster::raster() %>% 
      raster::extract(DF[,c("E", "N")]) ->
      
      DF$AnnualDensity.t0
    
    return(DF)
    
  }) -> DensityList
  
  DensityList %>% bind_rows -> Deer
  
  #Deer %<>% mutate_at(vars(contains("Density")), ~.x*PopN)
  
  # Deer %<>% 
  #   group_by(Year) %>% 
  #   mutate_at(vars(contains("AnnualDensity")), ~scales::rescale(.x, c(0, 1))) %>% 
  #   ungroup
  
  # Calves ####
  
  Censuses %>% 
    filter(Year %in% FocalYears) %>% 
    filter(Age == 0) %>% 
    group_by(Code) %>% 
    summarise_at(c("Easting", "Northing"), ~mean(.x, na.rm = T)) ->
    LifetimeCentroids
  
  SPDF <- SpatialPointsDataFrame(data = LifetimeCentroids[,c("Easting", "Northing")], 
                                 coords = LifetimeCentroids[,c("Easting", "Northing")])
  
  LifetimeKUDL <- kernelUD(SPDF, same4all = TRUE, grid = 500)
  
  LifetimeKUDL %>% raster::raster() %>% raster::extract(Deer[,c("E", "N")]) ->
    
    Deer$LifetimeCalfDensity
  
  Deer %>% arrange(Year) %>% pull(Year) %>% unique %>% as.character -> FocalYears
  
  # FocalYears %<>% c(min(as.numeric(FocalYears))-1, .)
  
  Censuses %>% 
    filter(Year %in% FocalYears) %>% 
    filter(Age == 0) %>% 
    group_by(Code, Year) %>% 
    summarise_at(c("Easting", "Northing"), 
                 ~mean(.x, na.rm = T)) %>% 
    rename(XCentroidAnnual = Easting, YCentroidAnnual = Northing) -> 
    
    AnnualCentroids
  
  AnnualCentroids %<>% filter(Year %in% FocalYears)
  
  SPDF <- SpatialPointsDataFrame(data = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual", "Year")], 
                                 coords = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual")])
  
  SPDF <- SPDF[,"Year"]
  
  KUDL <- kernelUD(SPDF, same4all=TRUE, grid=500)
  
  1:length(names(KUDL)) %>% lapply(function(a){
    
    print(names(KUDL)[a])
    
    DF <- Deer %>% filter(Year == names(KUDL)[a])
    
    KUDL2 <- KUDL[[names(KUDL)[a]]]
    
    KUDL2 %>% raster::raster() %>% raster::extract(DF[,c("E", "N")]) ->
      
      DF$AnnualCalfDensity
    
    return(DF)
    
  }) -> DensityList
  
  DensityList %>% bind_rows -> Deer
  
  2:length(names(KUDL)) %>% lapply(function(a){
    
    print(names(KUDL)[a])
    
    DF <- Deer %>% filter(Year == names(KUDL)[a])
    
    KUDL2 <- KUDL[[names(KUDL)[a - 1]]]
    
    KUDL2 %>% raster::raster() %>% 
      raster::extract(DF[,c("E", "N")]) ->
      
      DF$AnnualCalfDensityt0
    
    return(DF)
    
  }) -> DensityList
  
  DensityList %>% bind_rows -> Deer
  
  # Deer %<>% mutate_at(vars(contains("CalfDensity")), ~.x*PopN)
  
  Deer %<>% 
    group_by(Year) %>% 
    mutate_at(vars(contains("AnnualCalfDensity")), ~scales::rescale(.x, c(0, 1))) %>% 
    ungroup()
  
}

# Sighting densities ####

if(0){
  
  Censuses2 <- Censuses %>% filter(Year %in% FocalYears)
  
  SPDF <- SpatialPointsDataFrame(data = Censuses2[,c("Easting", "Northing")], 
                                 coords = Censuses2[,c("Easting", "Northing")])
  
  KUDL <- kernelUD(SPDF, same4all = TRUE, grid = 500)
  
  KUDL %>% raster::raster() %>% raster::extract(Deer[,c("E", "N")]) ->
    
    Deer$SightingDensity
  
  SPDF <- SpatialPointsDataFrame(data = Censuses2[,c("Easting", "Northing", "Year")], 
                                 coords = Censuses2[,c("Easting", "Northing")])
  
  SPDF <- SPDF[,"Year"]
  
  KUDL <- kernelUD(SPDF, same4all=TRUE, grid=500)
  
  1:length(FocalYears) %>% lapply(function(a){
    
    print(FocalYears[a])
    
    DF <- Deer %>% filter(Year == FocalYears[a])
    
    KUDL2 <- KUDL[[FocalYears[a]]]
    
    KUDL2 %>% raster::raster() %>% raster::extract(DF[,c("E", "N")]) ->
      
      DF$AnnualSightingDensity
    
    return(DF)
    
  }) -> DensityList
  
  DensityList %>% bind_rows -> Deer
  
}

# Adding centroid distances ####

Censuses %>% 
  group_by(Code) %>% 
  summarise(Lifetime.E = mean(Easting, na.rm = T),
            Lifetime.N = mean(Northing, na.rm = T)) %>% 
  left_join(Deer, ., by = c("Name" = "Code")) %>% 
  left_join(Deer %>% 
              mutate(Year2 = Year + 1) %>% 
              dplyr::select(Name, Year2, E.t0 = E, N.t0 = N) %>% unique, 
            by = c("Name", "Year" = "Year2")) ->
  
  Deer

Censuses %>% ungroup %>% 
  filter(Age<6) %>% 
  group_by(Code) %>% 
  summarise(Early.E = mean(Easting, na.rm = T),
            Early.N = mean(Northing, na.rm = T)) %>% 
  left_join(Deer, ., by = c("Name" = "Code")) -> 
  
  Deer

Pythagoreg <- function(Data, X = c("X1", "X2"), Y = c("Y1", "Y2")){
  
  Data <- as.data.frame(Data)
  
  X <- abs(Data[,X[1]] - Data[,X[2]])
  Y <- abs(Data[,Y[1]] - Data[,Y[2]])
  
  Distance <- (X^2 + Y^2)^0.5
  
  return(Distance)
  
}

Deer %>% Pythagoreg(X = c("E", "E.t0"), Y = c("N", "N.t0")) -> 
  Deer$AnnualDistance

Deer %>% 
  arrange(Name, Year) %>% ungroup %>% 
  group_by(Name) %>% mutate(YearDiff = c(NA, diff(Year))) %>% ungroup ->
  Deer

Deer %<>% 
  mutate_at("AnnualDistance", ~ifelse(is.na(YearDiff)|(YearDiff>1), NA, .x))

Deer %>% Pythagoreg(X = c("E", "Lifetime.E"), Y = c("N", "Lifetime.N")) -> 
  Deer$LifetimeDistance

Deer %>% Pythagoreg(X = c("E", "Early.E"), Y = c("N", "Early.N")) -> 
  Deer$EarlyDistance

# Adding Graze ####

Censuses %>% 
  ungroup %>%  
  # rename(Name = Code) %>% 
  mutate(Year = DeerYear) %>% 
  group_by(Name, Year) %>%
  summarise_at("GrazeType", ~.x %>% str_detect("^G.$") %>% Prev) %>% 
  ungroup -> GrazeDeer

Deer %<>% 
  left_join(GrazeDeer, by = c("Name", "Year")) #%>% 
# left_join(GrazeDeer %>% mutate(Year = Year + 1), 
#           by = c("Name", "Year"), suffix = c("", ".t0"))

if(0){
  
  # Adding Home range models ####
  
  MovementLists <- readRDS("C:/Users/gfalb/Documents/Script/Spocial/Data/MovementLists.rds")
  MCPLists <- readRDS("C:/Users/gfalb/Documents/Script/Spocial/Data/MCPLists.rds")
  
  MovementLists[[1]] %>% bind_rows(.id = "Name") %>% melt %>% 
    filter(!is.na(value)) %>% 
    rename(HRA = value) %>% 
    mutate(Year = variable %>% str_remove("X") %>% as.numeric) %>% 
    dplyr::select(Name, Year, HRA) %>% full_join(
      
      MCPLists[[1]] %>% bind_rows(.id = "Name") %>%
        dplyr::select(Name, Year, MCPArea)) -> Areas
  
  AnnualChange <- function(Matrix){
    
    N <- nrow(Matrix)
    
    Matrix[2:N, 1:(N-1)]
    
    2:N %>% map_dbl(~Matrix[.x, .x-1])
    
  }
  
  MCPLists[[2]][!map_lgl(MCPLists[[2]], ~all(is.na(.x)))] %<>% 
    lapply(function(a){
      
      N <- nrow(a)
      
      New <- a %>% as.numeric %>% matrix(nrow = N, ncol = N)
      
      dimnames(New) <- list(rownames(a), rownames(a))
      
      return(New)
      
    })
  
  MovementLists[[2]][!map_lgl(MovementLists[[2]], ~all(is.na(.x)))] %>% 
    map(~data.frame(HRAShrink = AnnualChange(.x),
                    Year = rownames(.x)[2:nrow(.x)])) %>% 
    bind_rows(.id = "Name") %>% 
    
    full_join(
      
      MCPLists[[2]][!map_lgl(MCPLists[[2]], ~all(is.na(.x)))] %>% 
        map(~data.frame(MCPShrink = AnnualChange(.x),
                        Year = rownames(.x)[2:nrow(.x)] %>% str_remove("X"))) %>%
        bind_rows(.id = "Name")) -> Shrink
  
  Shrink %>% mutate_at("Year", as.numeric) %>% full_join(Areas) %>% 
    left_join(Deer, .) -> Deer
  
  Deer %<>% 
    mutate_at(vars(matches("Shrink")), ~ifelse(is.na(YearDiff)|(YearDiff>1), NA, .x))
  
}

# Adding previous year's measures ####

Deer %>% 
  filter(Survived0 == 1|is.na(Survived0)) %>% 
  #filter(Survived0 == 1) %>% 
  dplyr::select(Name, Year, 
                contains(c("HRA", "HRAShrink", "MCPArea", "MCPShrink")), 
                contains("Distance"),
                contains("Density"), 
                contains(SocResps[1:3]),
                contains("GrazeType")) %>% unique %>% 
  mutate_at("Year", ~.x + 1) %>% 
  left_join(Deer, ., by = c("Name", "Year"), 
            suffix = c("", ".t0")) ->
  
  Deer

SaveDeer <- Deer

# Separating Greg and Moredun samples ####

Deer %<>% mutate(Counter = ifelse(is.na(Counter), "GFA", "DMB"))

Deer %>% saveRDS("ModelDeer.rds")
