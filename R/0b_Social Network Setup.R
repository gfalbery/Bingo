# Subsampling Social and Spatial behvaiour Metrics

# rm(list = ls())

Root <- "Data"

# Initial data import and cleaning ####

library(tidyverse);library(reshape2);library(igraph); library(ggregplot); library(readxl); library(data.table)
library(magrittr); library(tidygraph)

if(!any(str_detect(ls(), "Censuses"))){
  
  Censuses <- read_xlsx(paste0(Root, "/Behaviour/tblCensus.xlsx"))
  
  # Censuses <- fread(paste0(Root, "/Behaviour/tblCensus.xlsx"))
  
  Censuses %>% 
    separate(Date, "-", into = c("Year", "Month", "Day")) %>% 
    dplyr::select(c("Day", "Month", "Year")) %>% 
    mutate_all(as.numeric) ->
    Censuses[,c("Day", "Month", "Year")]
  
}

Censuses %<>% 
  mutate(DeerYear = ifelse(Month < 5, Year - 1, Year)) %>% 
  mutate(Season = ifelse(Year == DeerYear, "Rut", "Spring")) %>% 
  mutate(GroupDate = paste(Date, Group, sep = ",")) %>% 
  filter(!is.na(Code), !Code == "") %>% 
  dplyr::select(c("Date","Code","Easting","Northing","GroupSize",
                  "Year","DeerYear","Season","GroupDate", "GrazeType"))

Individuals <- read_xlsx(paste0(Root, "/Phenotypes/tblLife.xlsx"))
Names <- read_xlsx(paste0(Root, "/Phenotypes/tblNames.xlsx"))

Individuals <- merge(Individuals, Names, by = "Code",all.x=TRUE)

Individuals %<>% mutate_at(c("GivenName", "FamilyName"), as.character)

Individuals[is.na(Individuals$GivenName),"GivenName"] <- 
  Individuals[is.na(Individuals$GivenName),"FamilyName"]

Individuals %<>% 
  mutate(Animal = GivenName)

Individuals$Sex <- cut(Individuals$Sex, 
                       breaks = c(0, 1.5, 2.5, 3.5),
                       labels = c("F", "M", "3"))

Individuals %<>% mutate_at(vars(BirthDay:DeathYear), 
                           function(a) str_pad(a, 2, "left", 0))

Individuals %<>% 
  mutate(Birth.Date = paste(BirthDay, BirthMonth, BirthYear, sep="/"),
         Death.Date = paste(DeathDay, DeathMonth, DeathYear,sep="/")) %>% 
  mutate_at(c("Birth.Date", "Death.Date"), ~ifelse(.x == "NA/NA/NA", NA, .x)) %>% 
  mutate_at(c("BirthYear", "DeathYear"), ~as.numeric(as.character(.x)))

Individuals$Name = Individuals$Code

Censuses <- merge(Censuses, Individuals[,c("Sex", "Name", "BirthYear")], 
                  by.x = c("Code"), by.y = c("Name"))

Censuses %<>% filter(!is.na(Easting), !is.na(Northing))

Censuses %<>% mutate(Age = Year - BirthYear, 
                     Hind = ifelse(Age > 2 & Sex == "F", "Y", "N"))

Censuses %<>% arrange(lubridate::ymd(Date))

Censuses %<>% mutate(Name = Code)

# to make annual records for all sightings ####

SEASON = c("Rut", "Spring")

Records = 5

AMList <- longlist <- NetList <- list()

x <- min(Censuses$DeerYear, na.rm = TRUE)

# FocalYears <- Censuses$DeerYear %>% unique %>% gtools::mixedsort()

for(x in (FocalYears)){
  
  print(x)
  
  Censuses2 <- 
    Censuses %>% 
    filter(DeerYear == x) %>% 
    filter(!is.na(Code)) %>% 
    filter(Season %in% c(SEASON)) # %>% 
  # filter(Hind %in%c("Y"))# %>% 
  # filter(Hind %in%c("Y", "N", NA))
  # filter(!is.na(Hind))
  
  if(nrow(Censuses2)>0){
    
    SocGraph <- Censuses2 %>% dplyr::select(Code, GroupDate) %>% table() %>% 
      graph.incidence(weighted = T) %>% 
      bipartite.projection %>% extract2("proj1") %>% as_tbl_graph()
    
    SocGraph %<>% activate(nodes) %>%
      left_join(Individuals, by = c("name" = "Name"))
    
    AssMat <- 
      SocGraph %>% 
      get.adjacency(attr = "weight", sparse = F)
    
    NObs <- diag(AssMat) <- table(Censuses2$Code)
    
    AM <- ProportionalMatrix(AssMat, NObs)
    
    diag(AssMat) <- diag(AM) <- 0
    
    AMList[[which(FocalYears == x)]] <- AM
    
    DeerGraph <- 
      graph_from_adjacency_matrix(AM, weighted = TRUE, mode = "undirected") %>% 
      as_tbl_graph() # %>% 
    # left_join(Individuals, by = c("name" = "Name"))
    
    Eigens <- data.frame(Name = rownames(AM),
                         Degree = colSums(AM > 0), 
                         Strength = colSums(AM),
                         # Eigenvector = eigen_centrality(DeerGraph, 
                         #                                weights = NA, 
                         #                                scale = TRUE)$vector,
                         # Eigenvector_Weighted = eigen_centrality(DeerGraph, 
                         #                                         scale = TRUE)$vector,
                         # Betweenness = betweenness(DeerGraph),
                         Clustering = transitivity(DeerGraph, type = "local")
    ) %>%
      mutate(Strength_Mean = Strength/Degree) %>%
      mutate(Strength_Mean = ifelse(is.na(Strength_Mean), 0, Strength_Mean)) %>% 
      arrange(Name)
    
    # Hinds only ####
    
    AssMat <- 
      SocGraph %>% 
      mutate(Age = x - BirthYear) %>% 
      filter(Age > 2 & Sex == "F") %>% 
      get.adjacency(attr = "weight", sparse = F)
    
    NObs2 <- diag(AssMat) <- rowSums(AssMat)
    
    AM <- ProportionalMatrix(AssMat, NObs2)
    
    diag(AssMat) <- diag(AM) <- 0
    
    AM[is.na(AM)] <- 0
    
    DeerGraph <- 
      graph_from_adjacency_matrix(AM, weighted = TRUE, mode = "undirected") %>% 
      as_tbl_graph() # %>% 
    # left_join(Individuals, by = c("name" = "Name"))
    
    Eigens2 <- data.frame(Name = rownames(AM),
                          Degree = colSums(AM > 0), 
                          Strength = colSums(AM),
                          # Eigenvector = eigen_centrality(DeerGraph, 
                          #                                weights = NA, 
                          #                                scale = TRUE)$vector,
                          # Eigenvector_Weighted = eigen_centrality(DeerGraph, 
                          #                                         scale = TRUE)$vector,
                          # Betweenness = betweenness(DeerGraph),
                          Clustering = transitivity(DeerGraph, type = "local")
    ) %>%
      mutate(Strength_Mean = Strength/Degree) %>%
      mutate(Strength_Mean = ifelse(is.na(Strength_Mean), 0, Strength_Mean)) %>% 
      arrange(Name)
    
    # Combining ####
    
    Subdf <- 
      Censuses2 %>% 
      dplyr::group_by(Name) %>% 
      dplyr::summarize(E = mean(Easting, na.rm = T), 
                       N = mean(Northing, na.rm = T),
                       GroupSize = mean(Northing, na.rm = T)) %>% 
      mutate(RiverDistance = abs(1363 - E), 
             NObs = c(NObs),
             Reg6 = LocToReg6(E, N)) %>% 
      left_join(Eigens, by = c("Name")) %>% 
      left_join(Eigens2, by = c("Name"), suffix = c("", ".Hind")) %>% 
      data.frame %>% 
      mutate(Year = x)
    
    longlist[[which(FocalYears == x)]] <- Subdf
    
  }
}

FullSociality <- bind_rows(longlist)

FullSociality <- FullSociality %>% filter(!Year == 1992)

SocResps <- c("GroupSize", "Degree",
              "Strength", "Strength_Mean",
              "Eigenvector", "Eigenvector_Weighted",
              "Betweenness", "Clustering")
