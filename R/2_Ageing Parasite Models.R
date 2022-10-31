
# 2_Ageing Parasite Models

{
  
  library(tidyverse);library(reshape2);library(igraph); library(ggregplot); library(INLA); library(gsheet); library(magrittr)
  library(readxl); library(patchwork); library(cowplot); library(colorspace)
  
  theme_set(theme_cowplot())
  
}

# SaveDeer <- Deer
Deer <- readRDS("ModelDeer.rds")

Deer %<>% mutate_at("AnnualDensity.t0", ~scales::rescale(.x, to = c(0, 1)))

ReprodReplace <- c("None", "None", "Summer", "Winter", "Winter")

names(ReprodReplace) <- c("Naive", "True.Yeld", "Summer.Yeld", "Winter.Yeld", "Milk")

Deer %<>% mutate_at(vars(contains("ReprodStatus")), ~.x %>%
                      str_replace_all(c(ReprodReplace)))

AdultParasites <- Parasites[c(1, 6, 4, 5)] %>% c("TotA", "TcA")

Resps <- Parasites %>% c("TotA", "TcA")#[c(1, 4, 5, 6)]

FamilyList <- rep("nbinomial", 6) %>% c(rep("binomial", 2)) %>% c(rep("gaussian", 2))

names(FamilyList) <- Resps

Covar <- c("Year", "Season", "ReprodStatus")

AddCovar <- c("Degree.t0", "AnnualDensity.t0", "Age")

IIDCovar <- c("Name")

IMList <- list()

r <- 1

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(Resps[r] %in% AdultParasites){
    
    Deer %>% 
      filter(Hind == 1) %>% 
      filter(Year > 2015) %>% 
      dplyr::select(IIDCovar, 
                    all_of(Resps[r]), Covar, 
                    AddCovar, 
                    E, N) %>% 
      mutate_at(Resps[r], ~round(.x)) %>% 
      mutate_at("Year", as.factor) %>% 
      na.omit %>% droplevels -> 
      TestDF
    
    TestDF %>% nrow %>% print
    
    IM1 <- INLAModelAdd(Response = Resps[r], 
                        Explanatory = Covar %>% c(#"Season:ReprodStatus", 
                          AddCovar),
                        ScaleVariables = F,
                        # Add = AddCovar,
                        # Random = IIDCovar, RandomModel = "iid",
                        Add = "f(Name, model = 'iid')",
                        Clashes = ClashList,
                        Family = FamilyList[[Resps[r]]], 
                        Data = TestDF,
                        AddSpatial = T,
                        AllModels = T, 
                        Delta = -Inf,
                        Coordinates = c("E", "N"))
    
    IM1$FinalModel %>% Efxplot
    
    IMList[[Resps[r]]] <- IM1
    
  }
}

IMList <- IMList[AdultParasites]

saveRDS(IMList, file = "Output/AdultIMList.rds")

