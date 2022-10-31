
# X_Subsetting Deer ####

names(Resps) <- NULL

Deer %>% 
  mutate_at("Name", ~as.numeric(as.factor(.x))) %>% 
  filter(Hind == 1) %>% 
  filter(Year > 2015) %>% 
  dplyr::select(IIDCovar, 
                Resps %>% setdiff(c("TotA", "TcA")), Covar, 
                AddCovar, 
                Survived1,
                E, N) %>% 
  saveRDS("ModelDeer.rds")
