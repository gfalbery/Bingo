
# 3_Ageing Parasite Survival Models ####

{
  
  library(tidyverse);library(reshape2);library(igraph); library(ggregplot); library(INLA); library(gsheet); library(magrittr)
  library(readxl); library(patchwork); library(cowplot); library(colorspace)
  
  theme_set(theme_cowplot())
  
}

# SaveDeer <- Deer
# Deer <- SaveDeer

Deer <- readRDS("ModelDeer.rds")

ReprodReplace <- c("None", "None", "Summer", "Winter", "Winter")

names(ReprodReplace) <- c("Naive", "True.Yeld", "Summer.Yeld", "Winter.Yeld", "Milk")

Deer %<>% mutate_at(vars(contains("ReprodStatus")), ~.x %>%
                      str_replace_all(c(ReprodReplace)))

AdultParasites <- Parasites[c(1, 4, 5, 6)] %>% c("TotA", "TcA")

Resps <- Parasites %>% c("TotA", "TcA")#[c(1, 4, 5, 6)]

FamilyList <- rep("nbinomial", 6) %>% c(rep("binomial", 2)) %>% c(rep("gaussian", 2))

names(FamilyList) <- Resps

Covar <- c("Year", "Season", "ReprodStatus")

AddCovar <- c("Age")

IIDCovar <- c("Name")

SurvivalIMList <- list()

r <- 1

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(Resps[r] %in% AdultParasites){
    
    Deer %>% 
      filter(Hind == 1) %>% 
      filter(Year > 2015) %>% 
      filter(!Season == "Autumn") %>% 
      dplyr::select(IIDCovar, Survived,
                    all_of(Resps[r]), Covar, 
                    AddCovar, 
                    E, N) %>% 
      mutate_at("Survived", as.numeric) %>% 
      mutate_at("Year", as.factor) %>% 
      na.omit %>% droplevels -> 
      TestDF#
    
    if(FamilyList[[Resps[r]]] == "nbinomial"){
      
      TestDF %<>% mutate_at(Resps[r], ~log(.x + 1))
      
    }
    
    TestDF %>% nrow %>% print
    
    IM1 <- INLAModelAdd(Response = "Survived", 
                        Explanatory = Covar %>% c(#"Season:ReprodStatus", 
                          AddCovar, Resps[r]),
                        ScaleVariables = T,
                        # Add = AddCovar,
                        Random = IIDCovar, RandomModel = "iid",
                        Clashes = ClashList,
                        Family = "binomial", 
                        Data = TestDF,
                        AddSpatial = T,
                        AllModels = F, 
                        Coordinates = c("E", "N"))
    
    # IM1$FinalModel %>% Efxplot
    
    SurvivalIMList[[Resps[r]]] <- IM1
    
  }
}

SurvivalIMList %>% saveRDS(file = "Output/SurvivalIMList.rds")

SurvivalIMList <- SurvivalIMList[AdultParasites]

# Survival Figure

SurvivalEstimates <- 
  SurvivalIMList %>% 
  map("FinalModel") %>% 
  map(~summary(.x)$fixed) %>% 
  map(function(a) a %>% 
        as.data.frame %>% 
        rename(Estimate = mean, Lower = `0.025quant`, Upper = `0.975quant`) %>% 
        rownames_to_column("Factor") %>% 
        mutate_at("Factor", ~str_replace_all(.x, ":", "_") %>% str_remove_all("[(]|[)]")) %>% 
        dplyr::select(Factor, Estimate, Lower, Upper) %>% 
        slice(c(n()-1, n())) %>% 
        mutate(Model = "Base")) %>% 
  bind_rows() %>% 
  bind_rows(
    SurvivalIMList %>% 
      map(c("Spatial", "Model")) %>% 
      map(~summary(.x)$fixed) %>% 
      map(function(a) a %>% 
            as.data.frame %>% 
            rename(Estimate = mean, Lower = `0.025quant`, Upper = `0.975quant`) %>% 
            rownames_to_column("Factor") %>% 
            mutate_at("Factor", ~str_replace_all(.x, ":", "_") %>% str_remove_all("[(]|[)]")) %>% 
            dplyr::select(Factor, Estimate, Lower, Upper) %>% 
            slice(c(n()-1, n())) %>% 
            mutate(Model = "SPDE")) %>% 
      bind_rows()
  )

SurvivalEstimates %>% 
  filter(!Factor == "Age") %>% 
  ggplot(aes(Factor, Estimate, group = Model)) +
  geom_hline(lty = 2, alpha = 0.3, aes(yintercept = 0)) +
  geom_point(colour = "black", position = position_dodge(w = 0.8)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, colour = Model), 
                width = 0.2,
                position = position_dodge(w = 0.8)) +
  geom_point(aes(colour = Model), position = position_dodge(w = 0.8)) +
  coord_flip() + labs(x = NULL) +
  scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[2]]))

ggsave("Figures/SurvivalEstimates.jpeg", units = "mm", width = 200, height = 150, dpi = 300)

# Plots ####

PlotList <- 
  
  SurvivalIMList %>% names %>% map(function(a){
    
    SmoothOutput(Data = SurvivalIMList[[a]]$Data,
                 Model = SurvivalIMList[[a]]$FinalModel,
                 Covariates = c(Covar, AddCovar, "Season:ReprodStatus"), Response = a,
                 OutputCovariates = c("Age"),
                 HoldFactors = c("ReprodStatus" = "None"),
                 Output = "Data", Family = "NBinomial",
                 AddPoints = F, TestDF = SurvivalIMList[[a]]$Data, PointAlpha = 0.1,
                 ReturnPlot = F)
    
  })

PlotList %>% unlist(recursive = F) %>% 
  # map(~.x + lims(y = c(0, 30))) %>% 
  ArrangeCowplot()

a <- Resps[1]

SmoothOutput(Data = SurvivalIMList[[a]]$Data,
             Model = SurvivalIMList[[a]]$FinalModel,
             Covariates = c(Covar, "Degree.t0", "Season:ReprodStatus"), Response = a,
             OutputCovariates = c("Degree.t0"),
             HoldFactors = c("ReprodStatus" = "None"),
             Output = "Data", Family = "NBinomial",
             AddPoints = T, TestDF = SurvivalIMList[[a]]$Data, PointAlpha = 0.1,
             ReturnPlot = F)[[1]] + lims(y = c(0, 15))

list(
  SmoothOutput(Data = SurvivalIMList[[a]]$Data,
               Model = SurvivalIMList[[a]]$FinalModel,
               Covariates = c(Covar, "Degree.t0", "Season:ReprodStatus"), Response = a,
               OutputCovariates = c("Degree.t0"),
               HoldFactors = c("ReprodStatus" = "None"),
               Output = "Data", Family = "NBinomial",
               AddPoints = F, TestDF = SurvivalIMList[[a]]$Data, PointAlpha = 0.1,
               ReturnPlot = F)[[1]] + lims(y = c(0, 15)), 
  PlotList[c(1, 2, 4)] %>% unlist(recursive = F)) %>% 
  # map(~.x + lims(y = c(0, 30)))) %>% 
  ArrangeCowplot()

SurvivalIMList %>% map(~ggField(.x$Spatial$Model, .x$Spatial$Mesh) + scale_fill_discrete_sequential(palette = AlberPalettes[[2]])) %>% #Efxplot
  # map(~INLADICFig(.x, ModelNames = c("Base", "SPDE"))) %>% 
  ArrangeCowplot() + plot_layout(guides = "collect")

SurvivalIMList[[1]]$Spatial$Model %>% 
  ggField(SurvivalIMList[[1]]$Spatial$Mesh, 
          PointAlpha = 0.1,
          Points = SurvivalIMList[[1]]$Data[,c("E", "N")]) +
  scale_fill_discrete_sequential(palette = AlberPalettes[[2]])

saveRDS(SurvivalIMList, file = "Output/AdultSurvivalIMList.rds")

# BAMs instead ####

AddCovar <- c("Age", "s(Age)", "s(Age, bs = 'ad')")

ClashList <- list(AddCovar)

BAMList <- list()

r <- 1

for(r in r:length(Resps)){
  
  print(Resps[r])
  
  if(Resps[r] %in% AdultParasites){
    
    Deer %>% 
      filter(Hind == 1) %>% 
      filter(Year > 2015) %>% 
      dplyr::select(IIDCovar, 
                    all_of(Resps[r]), Covar, Age,
                    # AddCovar, 
                    E, N) %>% 
      mutate_at(Resps[r], ~round(.x)) %>% 
      mutate_at("Year", as.factor) %>% 
      na.omit %>% droplevels -> 
      TestDF
    
    TestDF %>% nrow %>% print
    
    IM1 <- BAMModelAdd(Response = Resps[r], 
                       Explanatory = Covar %>% c("Season:ReprodStatus", "t2(E, N)", "s(Name, bs = 're')") %>% 
                         setdiff(AddCovar),
                       ScaleVariables = F,
                       Add = AddCovar %>% last,
                       Clashes = ClashList,
                       Family = nb(), 
                       # Rounds = 1,
                       Data = TestDF,
                       AllModels = T)
    
    IM1$FinalModel %>% Efxplot
    
    BAMList[[Resps[r]]] <- IM1
    
  }
}

PlotList <- 
  
  BAMList %>% names %>% map(function(a){
    
    SmoothOutput(Data = BAMList[[a]]$Data,
                 Model = BAMList[[a]]$AllModels[[2]][[1]],
                 Covariates = c(Covar, "Age", "E", "N", "Name"), Response = a,
                 OutputCovariates = c("Age"),
                 HoldFactors = c("ReprodStatus" = "None", "Season" = "Summer"),
                 Output = "Data", Family = "NBinomial",
                 AddPoints = F, TestDF = BAMList[[a]]$Data, PointAlpha = 0.1,
                 ReturnPlot = F)
    
  })

PlotList %>% ArrangeCowplot()
