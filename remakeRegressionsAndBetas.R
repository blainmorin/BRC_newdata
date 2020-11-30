

### Load required packages
library(glm2)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(knitr)
library(tidyr)



### Load Original Data data
library(haven)
wide.df =  read_dta("Combined_Sandy_Harvey-Oct-08-2019-wide.dta")
long.df =  read_dta("Combined_Sandy_Harvey-Oct-08-2019-long.dta")

### Filter outliers
long.df = long.df %>%
  filter(all_outliers == 0)

wide.df = wide.df %>%
  filter(all_outliers == 0) 

num_tactics = c()

for (i in 1:nrow(wide.df)) {
  
  num_tactics[i] = sum(wide.df[i, 43:53])
  
}

### All data
df = long.df %>%
  drop_na(cost_sav) %>%
  drop_na(av_loss) %>%
  mutate(Name_of_Tactic = as.factor(Name_of_Tactic)) %>%
  mutate(recovery_shape = as.factor(recovery_path_Combined)) %>%
  mutate(hurricane = as.factor(hurricane)) %>%
  mutate(bi_dummy_employeescantwork = as.factor(ifelse(bi_employees_unable_to_work > 0, 1, 0))) %>%
  mutate(bi_dummy_employeesmoved = as.factor(ifelse(bi_employees_moved_away > 0 , 1, 0))) %>%
  mutate(bi_dummy_comms = as.factor(ifelse(bi_communications > 0, 1, 0))) %>%
  mutate(bi_dummy_supplychain = as.factor(ifelse(bi_supply_chain > 0, 1, 0))) %>%
  mutate(bi_dummy_powerout = as.factor(ifelse(bi_power_outages > 0 , 1, 0))) %>%
  mutate(bi_dummy_natgas = as.factor(ifelse(bi_natural_gas_outages > 0, 1, 0))) %>%
  mutate(bi_dummy_water = as.factor(ifelse(bi_water_outages > 0, 1, 0))) %>%
  mutate(bi_dummy_transport = as.factor(ifelse(bi_transportation > 0, 1, 0))) %>%
  mutate(log_prop_damage = log(property_damage_estimate + 1)) %>%
  mutate(log_prior_emp = log(employees_prior_hurricane)) %>%
  mutate(vrec = as.factor(vrec)) %>%
  rename(Recovered = Censor) %>%
  mutate(industry_codes = as.factor(industry_codes)) %>%
  mutate(max_pot_blain = max_potential_losses + 1) %>%
  select(vrec, cost_sav, av_loss, max_pot_blain, Name_of_Tactic, industry_codes,
         log_prop_damage, property_damage, log_prior_emp,
         bi_communications, bi_employees_moved_away,
         bi_employees_unable_to_work, bi_natural_gas_outages,
         bi_power_outages, bi_supply_chain, bi_transportation,
         bi_water_outages)

### Set seed
set.seed(14)
### New Number of firms
n = 50

id = df %>%
  select(vrec) %>%
  distinct()

newdata = id %>%
  sample_n(size = n, replace = TRUE) %>%
  left_join(y = df) %>%
  ### Make all tactics more cost effective
  mutate(av_loss = av_loss + abs(rnorm(1, mean = 0, sd = 50000))) %>%
  mutate(av_loss = ifelse(av_loss > max_pot_blain, max_pot_blain, av_loss))

df = rbind(df, newdata)

### Get cost incurring

df2 = df %>%
  filter(cost_sav > 0) %>%
  mutate(BCR = av_loss / cost_sav + .1) %>%
  mutate(av_loss_blain = av_loss + 1) %>%
  mutate(log_cost_sav = log(cost_sav)) %>%
  mutate(log_av_loss_blain = log(av_loss_blain)) %>%
  mutate(logBCR = log(BCR)) %>%
  filter(BCR < 50000) 

### Get cost saving

df3 = df %>%
  filter(cost_sav < 0) %>%
  mutate(BCR = av_loss / abs(cost_sav) + .1) %>%
  mutate(av_loss_blain = av_loss + 1) %>%
  mutate(log_cost_sav = log(abs(cost_sav))) %>%
  mutate(log_av_loss_blain = log(av_loss_blain)) %>%
  mutate(logBCR = log(BCR)) %>%
  filter(BCR < 50000)

### Make n INCUR and SAVINGS regressions

newincur.cost_save = list()
newincur.av_loss = list()
newsavings.cost_sav = list()
newsavings.av_loss = list()
newincur.maxpot = list()
newsavings.maxpot = list()


### Set seed for replicability
set.seed(14)

### Number of bootstrap regressions
n = 2500

### Ids to sample costs
ids = df2 %>%
  select(vrec) %>%
  distinct()

### Ids to sample benefits
ids1 = df3 %>%
  select(vrec) %>%
  distinct()

for (i in 1:n) {
  
  #### COST INCURRING REGRESSIONS
  
  boot.ids = ids %>%
    sample_n(size = nrow(ids), replace = TRUE)
  
  df10 = boot.ids %>%
    left_join(y = df2)
  
  costs.incur = glm2(cost_sav ~
                       Name_of_Tactic +
                       industry_codes +
                       log_prop_damage +
                       property_damage +
                       log_prior_emp + 
                       bi_communications +
                       bi_employees_moved_away +
                       bi_employees_unable_to_work +
                       bi_natural_gas_outages +
                       bi_power_outages +
                       bi_supply_chain +
                       bi_transportation +
                       bi_water_outages
                     ,
                     family = Gamma(link = 'log'),
                     control = list(maxit = 1000),
                     data = df10)
  
  benefits.incur = glm2(av_loss_blain ~
                          Name_of_Tactic +
                          industry_codes +
                          log_prop_damage +
                          property_damage +
                          log_prior_emp + 
                          bi_communications +
                          bi_employees_moved_away +
                          bi_employees_unable_to_work +
                          bi_natural_gas_outages +
                          bi_power_outages +
                          bi_supply_chain +
                          bi_transportation +
                          bi_water_outages
                        ,
                        family = Gamma(link = 'log'),
                        control = list(maxit = 1000),
                        data = df10)
  
  newincur.cost_save[[i]] = costs.incur
  newincur.av_loss[[i]] = benefits.incur
  
  
  maxpot.incur = glm2(max_pot_blain ~
                        industry_codes +
                        log_prop_damage +
                        property_damage +
                        log_prior_emp + 
                        bi_communications +
                        bi_employees_moved_away +
                        bi_employees_unable_to_work +
                        bi_natural_gas_outages +
                        bi_power_outages +
                        bi_supply_chain +
                        bi_transportation +
                        bi_water_outages 
                      ,
                      family = Gamma(link = 'log'),
                      control = list(maxit = 1000),
                      data = df10)
  
  newincur.maxpot[[i]] = maxpot.incur
  
  
  
  ####### COST SAVING REGRESSIONS
  
  
  boot.ids1 = ids1 %>%
    sample_n(size = nrow(ids1), replace = TRUE)
  
  df11 = boot.ids1 %>%
    left_join(y = df3)
  
  costs.saved = glm2(abs(cost_sav) ~
                       Name_of_Tactic +
                       industry_codes +
                       log_prop_damage +
                       property_damage +
                       log_prior_emp + 
                       bi_communications +
                       bi_employees_moved_away +
                       bi_employees_unable_to_work +
                       bi_natural_gas_outages +
                       bi_power_outages +
                       bi_supply_chain +
                       bi_transportation +
                       bi_water_outages
                     ,
                     family = Gamma(link = 'log'),
                     control = list(maxit = 1000),
                     data = df11)
  
  benefits.saved = glm2(av_loss_blain ~
                          Name_of_Tactic +
                          industry_codes +
                          log_prop_damage +
                          property_damage +
                          log_prior_emp + 
                          bi_communications +
                          bi_employees_moved_away +
                          bi_employees_unable_to_work +
                          bi_natural_gas_outages +
                          bi_power_outages +
                          bi_supply_chain +
                          bi_transportation +
                          bi_water_outages
                        ,
                        family = Gamma(link = 'log'),
                        control = list(maxit = 1000),
                        data = df11)
  
  newsavings.cost_sav[[i]] = costs.saved
  newsavings.av_loss[[i]] = costs.incur
  
  maxpot.save = glm2(max_pot_blain ~
                       industry_codes +
                       log_prop_damage +
                       property_damage +
                       log_prior_emp + 
                       bi_communications +
                       bi_employees_moved_away +
                       bi_employees_unable_to_work +
                       bi_natural_gas_outages +
                       bi_power_outages +
                       bi_supply_chain +
                       bi_transportation +
                       bi_water_outages 
                     ,
                     family = Gamma(link = 'log'),
                     control = list(maxit = 1000),
                     data = df11)
  
  newsavings.maxpot[[i]] = maxpot.save
  
  
}



######################### Get NEW BETAS
##############################
#### Cost incurring

newincur.av_loss.betas = matrix(nrow = 27, ncol = 2500)

for (i in c(1:2500)) {
  
  newincur.av_loss.betas[, i] = coef(newincur.av_loss[[i]])
  
}

save(newincur.av_loss.betas, file = "SimulateData/newincur_av_loss_betas")

newincur.cost_sav.betas = matrix(nrow = 27, ncol = 2500)

for (i in c(1:2500)) {
  
  newincur.cost_sav.betas[, i] = coef(newincur.cost_save[[i]])
  
}

save(newincur.cost_sav.betas, file = "SimulateData/newincur_cost_sav_betas")



###################################################
###### Savings

newsaved.av_loss.betas = matrix(nrow = 27, ncol = 2500)

for (i in c(1:2500)) {
  
  newsaved.av_loss.betas[, i] = coef(newsavings.av_loss[[i]])
  
}

save(newsaved.av_loss.betas, file = "SimulateData/newsaved_av_loss_betas")

newsaved.cost_sav.betas = matrix(nrow = 27, ncol = 2500)

for (i in c(1:2500)) {
  
  newsaved.cost_sav.betas[, i] = coef(newsavings.cost_sav[[i]])
  
}

save(newsaved.cost_sav.betas, file = "SimulateData/newsaved_cost_sav_betas")


################################################
####### Max Pot Regs


newincur.maxpot.betas = matrix(nrow = 17, ncol = 2500)
newsaved.maxpot.betas = matrix(nrow = 17, ncol = 2500)

for (i in c(1:2500)) {
  
  newincur.maxpot.betas[, i] = coef(newincur.maxpot[[i]])
  newsaved.maxpot.betas[, i] = coef(newsavings.maxpot[[i]])
  
}


save(newincur.maxpot.betas, file = "SimulateData/newincur.maxpot.betas")
save(newsaved.maxpot.betas, file = "SimulateData/newsaved.maxpot.betas")

