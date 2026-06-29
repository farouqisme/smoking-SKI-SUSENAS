
#####################################################################
#####        Rscript untuk generate analisis inferensial        #####
#####        data tingkat kabupaten/kota                        #####
#####################################################################

#--> Clean env.
rm(list=ls())

#--> Load packages
library(tidyverse)
library(tidyr)
library(dplyr)
library(rio)
library(janitor)
library(tibble)
library(gt)
library(scales)
library(modelsummary)
library(sandwich)
library(lmtest)
library(broom)
library(fixest)
library(patchwork)
library(officer)
library(flextable)

#--> Import Data
rokok          <- rio::import("Data/merokok agregat susenas-ski.csv")
keterlengkapan <- rio::import("Data/rasio keterlengkapan.csv")
hhi_faskes     <- rio::import("Data/HHI Faskes PODES 2021.csv")
balita         <- rio::import("Data/proporsi balita.dta")
pdrb           <- rio::import("Data/pdrb adhk 23.xlsx")

#--> Prep Kabupaten/Kota prevalensi
## 1) Seluruh Kelompok
rokok_allpop <- rokok %>% group_by(kodewil,source) %>% 
  summarise(pop = sum(pop,na.rm = T),
            rokok_all = sum(rokok_all,na.rm = T),
            rokok_tembakau = sum(rokok_tembakau,na.rm = T),
            rokok_elektrik = sum(rokok_elektrik,na.rm = T), .groups = "drop") %>%
  mutate(p_all = rokok_all/pop,
         p_tembakau = rokok_tembakau/pop,
         p_elektrik = rokok_elektrik/pop) %>% dplyr::select(., c(kodewil,source,p_all,p_tembakau,p_elektrik))
  
rokok_allpop.2 <- left_join(rokok_allpop %>% filter(source == "SUSENAS") %>% dplyr::select(., -c(source)),
                            rokok_allpop %>% filter(source == "SKI") %>% dplyr::select(., -c(source)),
                            by=c("kodewil"),suffix = c(".sus",".ski"))
rokok_allpop.2 <- rokok_allpop.2 %>% 
  mutate(dev_all = p_all.sus - p_all.ski,
         dev_tembakau = p_tembakau.sus - p_tembakau.ski,
         dev_elektrik = p_elektrik.sus - p_elektrik.ski) %>% 
  dplyr::select(.,c(kodewil,dev_all,dev_tembakau,dev_elektrik))

## 2) Kelompok usia 10-18
rokok_allpop <- rokok %>% filter(usia %in% c(10:20)) %>% group_by(kodewil,source) %>% 
  summarise(pop = sum(pop,na.rm = T),
            rokok_all = sum(rokok_all,na.rm = T),
            rokok_tembakau = sum(rokok_tembakau,na.rm = T),
            rokok_elektrik = sum(rokok_elektrik,na.rm = T), .groups = "drop") %>%
  mutate(p_all = rokok_all/pop,
         p_tembakau = rokok_tembakau/pop,
         p_elektrik = rokok_elektrik/pop) %>% dplyr::select(., c(kodewil,source,p_all,p_tembakau,p_elektrik))

rokok_allpop.2.Y <- left_join(rokok_allpop %>% filter(source == "SUSENAS") %>% dplyr::select(., -c(source)),
                              rokok_allpop %>% filter(source == "SKI") %>% dplyr::select(., -c(source)),
                              by=c("kodewil"),suffix = c(".sus",".ski"))
rokok_allpop.2.Y <- rokok_allpop.2.Y %>% 
  mutate(dev_all.Y = p_all.sus - p_all.ski,
         dev_tembakau.Y = p_tembakau.sus - p_tembakau.ski,
         dev_elektrik.Y = p_elektrik.sus - p_elektrik.ski) %>% 
  dplyr::select(.,c(kodewil,dev_all.Y,dev_tembakau.Y,dev_elektrik.Y))

## 2) Kelompok usia 50-75
rokok_allpop <- rokok %>% filter(usia %in% c(50:70)) %>% group_by(kodewil,source) %>% 
  summarise(pop = sum(pop,na.rm = T),
            rokok_all = sum(rokok_all,na.rm = T),
            rokok_tembakau = sum(rokok_tembakau,na.rm = T),
            rokok_elektrik = sum(rokok_elektrik,na.rm = T), .groups = "drop") %>%
  mutate(p_all = rokok_all/pop,
         p_tembakau = rokok_tembakau/pop,
         p_elektrik = rokok_elektrik/pop) %>% dplyr::select(., c(kodewil,source,p_all,p_tembakau,p_elektrik))

rokok_allpop.2.O <- left_join(rokok_allpop %>% filter(source == "SUSENAS") %>% dplyr::select(., -c(source)),
                              rokok_allpop %>% filter(source == "SKI") %>% dplyr::select(., -c(source)),
                              by=c("kodewil"),suffix = c(".sus",".ski"))
rokok_allpop.2.O <- rokok_allpop.2.O %>% 
  mutate(dev_all.O = p_all.sus - p_all.ski,
         dev_tembakau.O = p_tembakau.sus - p_tembakau.ski,
         dev_elektrik.O = p_elektrik.sus - p_elektrik.ski) %>% 
  dplyr::select(.,c(kodewil,dev_all.O,dev_tembakau.O,dev_elektrik.O))

#--> Prep Kabupaten/Kota deviasi keterlengkapan
keterlengkapan <- keterlengkapan %>% rename(kodewil = kodewil_susenaspre2022) %>% 
  dplyr::select(., c(kodewil,sus_ski_rat,sus_ski_dev))


#--> Merge All
list_df <- list(rokok_allpop.2, rokok_allpop.2.Y, rokok_allpop.2.O,keterlengkapan, balita, hhi_faskes, pdrb)
all_dataset <- Reduce(function(x, y) merge(x, y, by = "kodewil", all = TRUE), list_df)
all_dataset <- all_dataset %>% mutate(kodeprov = as.factor(substr(kodewil,1,2)))

# 1) all age group
model_all_1 <- lm(dev_all ~ sus_ski_dev + balita_pop_perc + hhi_faskes_std + log(pdrb_adhk) + kodeprov, data = all_dataset)
model_all_2 <- lm(dev_tembakau ~ sus_ski_dev + balita_pop_perc + hhi_faskes_std + log(pdrb_adhk) + kodeprov, data = all_dataset)
model_all_3 <- lm(dev_elektrik ~ sus_ski_dev + balita_pop_perc + hhi_faskes_std + log(pdrb_adhk) + kodeprov, data = all_dataset)

aAG_est <- modelsummary(list("Perokok" = model_all_1, "Perokok Tembakau" = model_all_2, "Perokok Elektrik" = model_all_3),
                        vcov = "HC1",coef_omit = "kodeprov",stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                        gof_map = c("nobs", "r.squared", "adj.r.squared"),
                        title = "Regression Results: Usia 10-75 tahun",output = "flextable")

# 2) Young age group (10-20 tahun)
model_all_1 <- lm(dev_all.Y ~ sus_ski_dev + balita_pop_perc + hhi_faskes_std + log(pdrb_adhk) + kodeprov, data = all_dataset)
model_all_2 <- lm(dev_tembakau.Y ~ sus_ski_dev + balita_pop_perc + hhi_faskes_std + log(pdrb_adhk) + kodeprov, data = all_dataset)
model_all_3 <- lm(dev_elektrik.Y ~ sus_ski_dev + balita_pop_perc + hhi_faskes_std + log(pdrb_adhk) + kodeprov, data = all_dataset)

yAG_est <- modelsummary(list("Perokok" = model_all_1, "Perokok Tembakau" = model_all_2, "Perokok Elektrik" = model_all_3),
                        vcov = "HC1",coef_omit = "kodeprov",stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                        gof_map = c("nobs", "r.squared", "adj.r.squared"),
                        title = "Regression Results: Usia 10-20 tahun",output = "flextable")

# 3) Young age group (50-70 tahun)
model_all_1 <- lm(dev_all.O ~ sus_ski_dev + balita_pop_perc + hhi_faskes_std + log(pdrb_adhk) + kodeprov, data = all_dataset)
model_all_2 <- lm(dev_tembakau.O ~ sus_ski_dev + balita_pop_perc + hhi_faskes_std + log(pdrb_adhk) + kodeprov, data = all_dataset)
model_all_3 <- lm(dev_elektrik.O ~ sus_ski_dev + balita_pop_perc + hhi_faskes_std + log(pdrb_adhk) + kodeprov, data = all_dataset)

oAG_est <- modelsummary(list("Perokok" = model_all_1, "Perokok Tembakau" = model_all_2, "Perokok Elektrik" = model_all_3),
                        vcov = "HC1",coef_omit = "kodeprov",stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                        gof_map = c("nobs", "r.squared", "adj.r.squared"),
                        title = "Regression Results: Usia 50-70 tahun",output = "flextable")


dev_bAG <- read_docx() %>%
  body_add_flextable(aAG_est) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(yAG_est) %>% 
  body_add_par("", style = "Normal") %>%
  body_add_flextable(oAG_est)

# -> Export files
print(dev_bAG, target = "Output/deviasi-sus-ski-dev-kabkota.docx")