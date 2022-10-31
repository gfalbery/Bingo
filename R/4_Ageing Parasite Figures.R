
# 4_Ageing Parasite Figures ####

{
  
  library(tidyverse);library(reshape2);library(igraph); library(ggregplot); library(INLA); library(gsheet); library(magrittr)
  library(readxl); library(patchwork); library(cowplot); library(colorspace); library(RColorBrewer)
  
  theme_set(theme_cowplot())
  
  ParasiteColours2 <- c(ParasiteColours[1:4])#[c(1, 3, 4, 2)] #, AlberColours[["Pink"]])
  
  ParasitePalettes2 <- c(ParasitePalettes[1:3], "Mint")
  
  AdultParasites <- c("Strongyles.g", "Flukes.g", "Ec.g", "Dictyocaulus.g")
  
  names(ParasitePalettes2) <- names(ParasiteColours2) <- AdultParasites[1:4]
  
  IMList <- readRDS("Output/AdultIMList.rds")
  
  SurvivalIMList <- readRDS("Output/SurvivalIMList.rds")
  
  Covar <- c("Year", "Season", "ReprodStatus")
  
  AddCovar <- c("Degree.t0", "Age")
  
  IIDCovar <- c("Name")
  
}

# Figure 1 ####

PlotList <- 
  
  IMList[1:4][AdultParasites] %>% names %>% map(function(a){
    
    SmoothOutput(Data = IMList[[a]]$Data,
                 Model = IMList[[a]]$FinalModel,
                 Covariates = c(Covar, AddCovar, "Season:ReprodStatus"), Response = a,
                 OutputCovariates = c("Age"),
                 HoldFactors = c("ReprodStatus" = "None"),
                 Output = "Data", Family = "NBinomial",
                 LineAlpha = 0.1,
                 AddPoints = T, TestDF = IMList[[a]]$Data, PointAlpha = 0.1, 
                 TextColour = ParasiteColours2[[a]], PointColour = ParasiteColours2[[a]],
                 AddP = T, AddEstimate = T,
                 ReturnPlot = F)
    
  })

PlotList %>% 
  unlist(recursive = F) %>% 
  ArrangeCowplot() +
  plot_annotation(tag_levels = "A")

ggsave("Figures/AgeFits.jpeg", units = "mm", height = 170, width = 170, dpi = 600)

# Figure 2 ####

a <- Resps[1]

SmoothOutput(Data = IMList[[a]]$Data,
             Model = IMList[[a]]$FinalModel,
             Covariates = c(Covar, "Age", "Degree.t0", "AnnualDensity.t0", "Season:ReprodStatus"), Response = a,
             OutputCovariates = c("Degree.t0"),
             HoldFactors = c("ReprodStatus" = "None"),
             Output = "Data", Family = "NBinomial",
             LineAlpha = 0.1,
             AddPoints = T, TestDF = IMList[[a]]$Data, PointAlpha = 0.1,
             TextColour = ParasiteColours2[[a]], PointColour = ParasiteColours2[[a]],
             AddP = T, AddEstimate = T,
             ReturnPlot = F)[[1]]

ggsave("Figures/DegreeFit.jpeg", units = "mm", height = 120, width = 120, dpi = 600)

# Figure 3 ####

# Age or Degree versus Spatial

AgeEstimates <- 
  IMList %>% #names %>% 
  map(function(a){
    
    list(a$AllModels[[1]], a$AllModels[[2]][[1]], a$Spatial$Model) %>% 
      map(~summary(.x)$fixed) %>% 
      map(function(b) b %>% 
            as.data.frame %>% 
            rename(Estimate = mean, Lower = `0.025quant`, Upper = `0.975quant`) %>% 
            rownames_to_column("Factor") %>% 
            mutate_at("Factor", ~str_replace_all(.x, ":", "_") %>% str_remove_all("[(]|[)]")) %>% 
            slice(c(n()-1, n())) %>% 
            dplyr::select(Factor, Estimate, Lower, Upper)) %>% 
      bind_rows(.id = "Model") %>% 
      filter(Factor == "Age") %>% 
      mutate_at("Model", ~factor(c("Base", "+ID", "+SPDE")[as.numeric(.x)], 
                                 levels = c("Base", "+ID", "+SPDE")))
    
  }) %>% bind_rows(.id = "Response") %>% 
  mutate_at("Response", ~factor(.x, levels = rev(AdultParasites)))

(AgePlot <- 
    AgeEstimates %>% 
    filter(Factor == "Age") %>% 
    filter(Response %in% AdultParasites[1:4]) %>% 
    ggplot(aes(Response, Estimate, group = Model)) +
    geom_hline(lty = 2, alpha = 0.3, aes(yintercept = 0)) +
    geom_point(colour = "black", position = position_dodge(w = 0.8), size = 3) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper, colour = Model), 
                  width = 0.2,
                  position = position_dodge(w = 0.8)) +
    geom_point(aes(colour = Model), position = position_dodge(w = 0.8), size = 2) +
    coord_flip() + labs(x = NULL) +
    scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[2]], AlberColours[[3]])))

# ggsave("Figures/AgeEstimates.jpeg", units = "mm", width = 200, height = 150, dpi = 300)

# Survival Estimates?

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

SurvivalEstimates %<>% 
  filter(!Factor == "Age") %>% 
  mutate_at("Factor", ~factor(.x, levels = rev(AdultParasites)))

(SurvivalPlot <- 
    SurvivalEstimates %>% 
    filter(Model == "Base") %>% mutate_at("Model", ~"+ID") %>% 
    filter(Factor %in% AdultParasites[1:4]) %>% 
    ggplot(aes(Factor, Estimate, group = Model)) +
    geom_hline(lty = 2, alpha = 0.3, aes(yintercept = 0)) +
    geom_point(colour = "black", position = position_dodge(w = 0.8), size = 3) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper, colour = Model), 
                  width = 0.2,
                  position = position_dodge(w = 0.8)) +
    geom_point(aes(colour = Model), position = position_dodge(w = 0.8), size = 2) +
    coord_flip() + labs(x = " ") +
    scale_x_discrete(labels = rep(" ", 4)) +
    scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[2]], AlberColours[[3]]), 
                        limits = c("Base", "+ID", "+SPDE")))

# ggsave("Figures/SurvivalEstimates.jpeg", units = "mm", width = 200, height = 150, dpi = 300)

AgePlot + SurvivalPlot + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

# ggsave("Figures/Figure3.jpeg", units = "mm", width = 200, height = 200, dpi = 300)
ggsave("Figures/Figure3.jpeg", units = "mm", width = 180, height = 120, dpi = 300)

# Tables ####

IMList %>% 
  map("FinalModel") %>% 
  map(~summary(.x)$fixed) %>% 
  map(function(a) a %>% 
        as.data.frame %>% 
        rename(Estimate = mean, Lower = `0.025quant`, Upper = `0.975quant`) %>% 
        rownames_to_column("Factor") %>% 
        mutate_at("Factor", ~str_replace_all(.x, ":", "_") %>% str_remove_all("[(]|[)]")) %>% 
        dplyr::select(Factor, Estimate, Lower, Upper))

# Supplementary Figures ####

# DIC Changes ####

IMList %>% 
  names() %>% 
  map(~list(IMList[[.x]]$FinalModel, IMList[[.x]]$Spatial$Model) %>% 
        INLADICFig(ModelNames = c("Base", "SPDE")) + labs(y = .x)) %>% 
  ArrangeCowplot() +
  plot_layout(guides = "collect")

ggsave("Figures/DICChanges.jpeg", units = "mm", height = 250, width = 180, dpi = 300)

DICDF <- 
  IMList %>% 
  names() %>% 
  map_dfc(~list(IMList[[.x]]$AllModels[[1]], IMList[[.x]]$FinalModel, IMList[[.x]]$Spatial$Model) %>% 
            MDIC %>% unlist) %>% 
  as.matrix

dimnames(DICDF) <- list(c("Base", "+ID", "+SPDE"), 
                        AdultParasites[1:4])

DICDF %>% t

DICDF %>% t %>% 
  data.frame %>% 
  rownames_to_column("Parasite") %>% 
  write.csv("Figures/DICChanges.csv", row.names = F)

SurvivalDICDF <- 
  SurvivalIMList %>% 
  names() %>% 
  map_dfc(~list(SurvivalIMList[[.x]]$FinalModel, 
                SurvivalIMList[[.x]]$Spatial$Model) %>% 
            MDIC %>% unlist) %>% 
  data.frame

dimnames(SurvivalDICDF) <- list(c("Base", "+SPDE"), 
                                AdultParasites)

SurvivalDICDF %>% t %>% data.frame %>% mutate(Delta = `X.SPDE` - Base)

# Full Effects ####

PlotList <- 
  IMList %>% 
  names() %>% 
  map(~list(IMList[[.x]]$FinalModel, IMList[[.x]]$Spatial$Model) %>% 
        Efxplot(ModelNames = c("Base", "SPDE"), Intercept = F) + labs(y = .x))

PlotList[[2]] <- PlotList[[2]] + scale_x_discrete(labels = rep(" ", 11))
PlotList[[4]] <- PlotList[[4]] + scale_x_discrete(labels = rep(" ", 11))

PlotList[1:4] %>% 
  ArrangeCowplot() +
  plot_layout(guides = "collect")

ggsave("Figures/FullEffects.jpeg", units = "mm", height = 250, width = 180, dpi = 300)

# Survival Effects ####

PlotList <- 
  SurvivalIMList %>% 
  names() %>% 
  map(~list(SurvivalIMList[[.x]]$FinalModel, 
            SurvivalIMList[[.x]]$Spatial$Model) %>% 
        Efxplot(ModelNames = c("Base", "SPDE"), Intercept = F) + labs(y = .x))

# PlotList[[2]] <- PlotList[[2]] + scale_x_discrete(labels = rep(" ", 11))
# PlotList[[4]] <- PlotList[[4]] + scale_x_discrete(labels = rep(" ", 11))

PlotList[1:4] %>% 
  ArrangeCowplot() +
  plot_layout(guides = "collect")

ggsave("Figures/SurvivalEffects.jpeg", units = "mm", height = 250, width = 250, dpi = 300)


# Spatial Distributions ####

Plot1 <- 
  ggField(IMList[[2]]$Spatial$Model, IMList[[2]]$Spatial$Mesh,
          Points = IMList[[2]]$Data[,c("E", "N")], PointAlpha = 0.2) + 
  scale_fill_discrete_sequential(palette = ParasitePalettes2[[2]]) + 
  labs(fill = AdultParasites[2])

Plot2 <- 
  ggField(IMList[[3]]$Spatial$Model, IMList[[3]]$Spatial$Mesh,
          Points = IMList[[3]]$Data[,c("E", "N")], PointAlpha = 0.2) + 
  scale_fill_discrete_sequential(palette = ParasitePalettes2[[3]]) + 
  labs(fill = AdultParasites[3])

Plot1 + Plot2

ggsave("Figures/Fields.jpeg", units = "mm", height = 180, width = 250, dpi = 300)
