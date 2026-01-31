
###############################################################################
##### Script untuk identifikasi perbedaan data SKI dan SUSENAS tahun 2023 #####
##### Analisis dibagi menjadi dua tahap; EDA dan inferensial              #####
###############################################################################

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

#--> Import Data
agr <- rio::import("Data/SUSENAS_SKI_AGR.dta")
ind <- rio::import("Data/IND_SUSENAS_SKI.dta")

tes <- agr %>% filter(source == 1) %>% select(., c(rokok_tembakau,rokok_elektrik, rokok_all))

#---------- EDA ----------#

#--> Labeling
## Age group
agr <- agr %>% 
  mutate(usia_group = case_when(usia <= 18 ~ "10-18",
                                usia %in% c(19:25) ~ "19-25", usia %in% c(26:30) ~ "26-30",
                                usia %in% c(31:35) ~ "31-35", usia %in% c(36:40) ~ "36-40",
                                usia %in% c(41:45) ~ "41-45", usia %in% c(46:50) ~ "46-50",
                                usia %in% c(51:55) ~ "51-55", usia %in% c(56:60) ~ "56-60",
                                usia %in% c(61:65) ~ "61-65", usia > 65 ~ ">66"))
## data source, urban/rural, sex
agr <- agr %>%
  mutate(source = factor(source,levels = c(1, 2),labels = c("SUSENAS", "SKI")),
    urban = factor(urban,levels = c(0, 1),labels = c("Rural", "Urban")),
    jenis_kelamin = factor(jenis_kelamin,levels = c(1,2),labels = c("Laki-laki","Perempuan")))

## region
agr <- agr %>% mutate(prov = substr(kodewil,1,2), island = case_when(
                        prov %in% c(10:30) ~ "Sumatera", prov %in% c(31:40) ~ "Jawa",
                        prov %in% c(51:60) ~ "Bali-Nusra", prov %in% c(61:70) ~ "Kalimantan",
                        prov %in% c(71:80) ~ "Sulawesi", prov %in% c(81:90) ~ "Maluku",
                        prov %in% c(91:99) ~ "Papua")) %>% rename(kab = kodewil)

####### Population Annals #######
#--> Viz 1: (POP) Population
agr %>% group_by(source) %>% 
  summarise(Population = sum(pop, na.rm = T), .groups = "drop") %>%
  mutate(pct = Population / sum(Population)) %>%
  mutate(display_val = paste0(scales::number(Population, big.mark = ".", decimal.mark = ","),
                              "(",scales::percent(pct, accuracy = 0.1),")")) %>%
  select(source, display_val) %>% 
  pivot_wider(names_from = source, values_from = display_val) %>% gt()

#--> Viz 2: (POP) Household
agr %>% group_by(source) %>% 
  summarise(hh = sum(hitung_rt, na.rm = T),
            population = sum(pop, na.rm = T),.groups = "drop") %>%
  mutate(rt_perpop = population / hh) %>%
  mutate(display_val = paste0(scales::number(hh, big.mark = ".", decimal.mark = ","),
                              "(",scales::number(rt_perpop, accuracy = 0.1),")")) %>%
  select(source, display_val) %>% 
  pivot_wider(names_from = source, values_from = display_val) %>% gt()

#--> Viz 3: (POP) urban/rural
agr %>% group_by(source, urban) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
  group_by(source) %>% mutate(pct = total_pop / sum(total_pop)) %>%
  ungroup() %>% 
  mutate(display_val = paste0(
    scales::number(total_pop, big.mark = ".", decimal.mark = ","),
    " (", scales::percent(pct, accuracy = 0.01), ")")) %>%
  select(source, urban, display_val) %>%
  pivot_wider(names_from = urban, values_from = display_val) %>% gt() 

#--> Viz 4.1: (POP) age
agr %>% group_by(source, usia_group) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE),.groups = "drop") %>%
  group_by(source) %>%
  mutate(pct = total_pop / sum(total_pop)) %>%
  ungroup() %>%
  pivot_wider(names_from  = source,values_from = c(total_pop, pct),names_sep   = "_") %>%
  mutate(diff_pct = pct_SUSENAS - pct_SKI) %>% 
  mutate(
    SUSENAS = paste0(number(total_pop_SUSENAS, big.mark=".", decimal.mark=","),
                     " (", percent(pct_SUSENAS, accuracy = 0.01), ")"),
    SKI = paste0(number(total_pop_SKI, big.mark=".", decimal.mark=","),
                 " (", percent(pct_SKI, accuracy = 0.01), ")"),
    Selisih = percent(diff_pct, accuracy = 0.01)) %>% 
  select(usia_group, SUSENAS, SKI, Selisih) %>% gt(rowname_col = "usia_group",auto_align = FALSE)

#--> Viz 4.1: (POP) age (young)
agr %>%
  filter(usia <= 21) %>%
  group_by(source, usia) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE),.groups = "drop") %>%
  group_by(source) %>%
  mutate(pct = total_pop / sum(total_pop)) %>%
  ungroup() %>%
  pivot_wider(names_from  = source,values_from = c(total_pop, pct),names_sep   = "_") %>%
  mutate(diff_pct = pct_SUSENAS - pct_SKI) %>% 
  mutate(
    SUSENAS = paste0(number(total_pop_SUSENAS, big.mark=".", decimal.mark=","),
      " (", percent(pct_SUSENAS, accuracy = 0.01), ")"),
    SKI = paste0(number(total_pop_SKI, big.mark=".", decimal.mark=","),
      " (", percent(pct_SKI, accuracy = 0.01), ")"),
    Selisih = percent(diff_pct, accuracy = 0.01)) %>% 
  select(usia, SUSENAS, SKI, Selisih) %>% gt(rowname_col = "usia",auto_align = FALSE)


#--> Viz 5: (POP) islands
agr %>% group_by(source, island) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE),.groups = "drop") %>%
  group_by(source) %>%
  mutate(pct = total_pop / sum(total_pop)) %>%
  ungroup() %>%
  pivot_wider(names_from  = source,values_from = c(total_pop, pct),names_sep   = "_") %>%
  mutate(diff_pct = pct_SUSENAS - pct_SKI) %>% 
  mutate(
    SUSENAS = paste0(number(total_pop_SUSENAS, big.mark=".", decimal.mark=","),
                     " (", percent(pct_SUSENAS, accuracy = 0.01), ")"),
    SKI = paste0(number(total_pop_SKI, big.mark=".", decimal.mark=","),
                 " (", percent(pct_SKI, accuracy = 0.01), ")"),
    Selisih = percent(diff_pct, accuracy = 0.01)) %>% 
  select(island, SUSENAS, SKI, Selisih) %>% gt(rowname_col = "island",auto_align = FALSE)

#--> Viz 6: (POP) sex
agr %>% group_by(source, jenis_kelamin) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE),.groups = "drop") %>%
  group_by(source) %>%
  mutate(pct = total_pop / sum(total_pop)) %>%
  ungroup() %>%
  pivot_wider(names_from  = source,values_from = c(total_pop, pct),names_sep   = "_") %>%
  mutate(diff_pct = pct_SUSENAS - pct_SKI) %>% 
  mutate(
    SUSENAS = paste0(number(total_pop_SUSENAS, big.mark=".", decimal.mark=","),
                     " (", percent(pct_SUSENAS, accuracy = 0.01), ")"),
    SKI = paste0(number(total_pop_SKI, big.mark=".", decimal.mark=","),
                 " (", percent(pct_SKI, accuracy = 0.01), ")"),
    Selisih = percent(diff_pct, accuracy = 0.01)) %>% 
  select(jenis_kelamin, SUSENAS, SKI, Selisih) %>% gt(rowname_col = "jenis_kelamin",auto_align = FALSE)

####### Smoking Annals #######
#--> Viz 1: (SMOKE) Overall prevalence
agr %>% group_by(source) %>% 
  summarise(total_pop = sum(pop, na.rm = TRUE),rokok_all = sum(rokok_all, na.rm = TRUE),
            rokok_tembakau = sum(rokok_tembakau, na.rm = TRUE),rokok_elektrik = sum(rokok_elektrik, na.rm = TRUE),
            .groups = "drop") %>% mutate(prev_all = rokok_all / total_pop, prev_tembakau = rokok_tembakau / total_pop,
                                         prev_elektrik  = rokok_elektrik / total_pop) %>% 
  select(source,rokok_all, prev_all,rokok_tembakau, prev_tembakau,rokok_elektrik, prev_elektrik) %>%
  pivot_longer(cols = -source,names_to = c(".value", "jenis"),names_pattern = "(rokok|prev)_(.*)") %>% 
  pivot_wider(names_from = source,values_from = c(rokok, prev)) %>%
  mutate(selisih = prev_SUSENAS - prev_SKI) %>%
  mutate(SUSENAS = paste0(number(rokok_SUSENAS, big.mark = ".", decimal.mark = ","),
                          " (", percent(prev_SUSENAS, accuracy = 0.01), ")"),
         SKI = paste0(number(rokok_SKI, big.mark = ".", decimal.mark = ","),
                      " (", percent(prev_SKI, accuracy = 0.01), ")"),
         Selisih = percent(selisih, accuracy = 0.01)) %>%
  transmute(Jenis = case_when(jenis == "all" ~ "Merokok",jenis == "tembakau" ~ "Tembakau",jenis == "elektrik" ~ "Elektrik"),
            SUSENAS, SKI, Selisih) %>%  gt(rowname_col = "Jenis", auto_align = F)

#--> Viz 2: (SMOKE) by rural/urban
agr %>% 
  group_by(source, urban) %>% 
  summarise(total_pop = sum(pop, na.rm = TRUE),
            rokok_all = sum(rokok_all, na.rm = TRUE),
            rokok_tembakau = sum(rokok_tembakau, na.rm = TRUE),
            rokok_elektrik = sum(rokok_elektrik, na.rm = TRUE), .groups = "drop") %>% 
  mutate(prev_all       = rokok_all / total_pop, prev_tembakau  = rokok_tembakau / total_pop,
         prev_elektrik  = rokok_elektrik / total_pop) %>% 
  select(source, urban, rokok_all, prev_all, rokok_tembakau, prev_tembakau, 
         rokok_elektrik, prev_elektrik) %>%
  pivot_longer(cols = -c(source, urban),names_to = c(".value", "jenis"),names_pattern = "(rokok|prev)_(.*)") %>% 
  pivot_wider(names_from  = source,values_from = c(rokok, prev)) %>%
  mutate(selisih = prev_SUSENAS - prev_SKI) %>%
  mutate(SUSENAS = paste0(number(rokok_SUSENAS, big.mark = ".", decimal.mark = ","),
                          " (", percent(prev_SUSENAS, accuracy = 0.01), ")"),
         SKI = paste0(number(rokok_SKI, big.mark = ".", decimal.mark = ","),
                      " (", percent(prev_SKI, accuracy = 0.01), ")"),
         Selisih = percent(selisih, accuracy = 0.01)) %>%
  transmute(urban,Jenis = case_when(jenis == "all" ~ "Merokok",jenis == "tembakau" ~ "Tembakau",
                                    jenis == "elektrik"   ~ "Elektrik"),SUSENAS, SKI, Selisih) %>%
  gt(rowname_col = "Jenis",groupname_col = "urban",auto_align = FALSE)



#--> Viz 3.1: (SMOKE) by age
agr %>%
  group_by(source, usia_group) %>% 
  summarise(total_pop = sum(pop, na.rm = T),
            rokok_all = sum(rokok_all, na.rm = T),
            rokok_tembakau = sum(rokok_tembakau, na.rm = T),
            rokok_elektrik = sum(rokok_elektrik, na.rm = T), .groups = "drop") %>%  
  group_by(source, usia_group) %>% mutate(prev_all = rokok_all/total_pop,
                              prev_tembakau = rokok_tembakau/total_pop,
                              prev_elektrik = rokok_elektrik/total_pop) %>% ungroup() %>%
  mutate(Merokok = scales::percent(prev_all, accuracy = 0.01),
         Tembakau = scales::percent(prev_tembakau, accuracy = 0.01),
         Elektrik = scales::percent(prev_elektrik, accuracy = 0.01)) %>%
  select(source, usia_group, Merokok, Tembakau, Elektrik) %>%
  pivot_wider(names_from = source, values_from = c(Merokok, Tembakau, Elektrik),
              names_sep = "_") %>% gt(rowname_col = "usia_group") %>%
  tab_spanner(label = "SUSENAS", columns = c(Merokok_SUSENAS,Tembakau_SUSENAS,Elektrik_SUSENAS)) %>%
  tab_spanner(label = "SKI", columns = c(Merokok_SKI,Tembakau_SKI,Elektrik_SKI)) %>% 
  cols_label(Merokok_SUSENAS = "Merokok", Tembakau_SUSENAS = "Tembakau",Elektrik_SUSENAS = "Elektrik",
             Merokok_SKI = "Merokok", Tembakau_SKI  = "Tembakau", Elektrik_SKI  = "Elektrik")
  
agr %>%
  group_by(source, usia_group) %>% 
  summarise(total_pop = sum(pop, na.rm = TRUE),rokok_all = sum(rokok_all, na.rm = TRUE),
            rokok_tembakau = sum(rokok_tembakau, na.rm = TRUE),rokok_elektrik = sum(rokok_elektrik, na.rm = TRUE),
            .groups = "drop") %>%  
  mutate(prev_all = rokok_all / total_pop, prev_tembakau = rokok_tembakau / total_pop,
         prev_elektrik = rokok_elektrik / total_pop) %>%
  select(source, usia_group,prev_all, prev_tembakau, prev_elektrik) %>%
  pivot_wider(names_from  = source,values_from = c(prev_all, prev_tembakau, prev_elektrik)) %>%
  transmute(usia_group,
            Merokok = percent(prev_all_SUSENAS - prev_all_SKI, accuracy = 0.01),
            Tembakau = percent(prev_tembakau_SUSENAS - prev_tembakau_SKI, accuracy = 0.01),
            Elektrik = percent(prev_elektrik_SUSENAS - prev_elektrik_SKI, accuracy = 0.01)) %>%
  gt(rowname_col = "usia_group", auto_align = FALSE)

#--> Viz 3.2: (SMOKE) by age (young)
agr %>% filter(usia <= 21) %>% group_by(source, usia) %>% 
  summarise(total_pop = sum(pop, na.rm = T),
            rokok_all = sum(rokok_all, na.rm = T),
            rokok_tembakau = sum(rokok_tembakau, na.rm = T),
            rokok_elektrik = sum(rokok_elektrik, na.rm = T), .groups = "drop") %>%  
  group_by(source, usia) %>% mutate(prev_all = rokok_all/total_pop,
                                          prev_tembakau = rokok_tembakau/total_pop,
                                          prev_elektrik = rokok_elektrik/total_pop) %>% ungroup() %>%
  mutate(Merokok = scales::percent(prev_all, accuracy = 0.01),
         Tembakau = scales::percent(prev_tembakau, accuracy = 0.01),
         Elektrik = scales::percent(prev_elektrik, accuracy = 0.01)) %>%
  select(source, usia, Merokok, Tembakau, Elektrik) %>%
  pivot_wider(names_from = source, values_from = c(Merokok, Tembakau, Elektrik),
              names_sep = "_") %>% gt(rowname_col = "usia") %>%
  tab_spanner(label = "SUSENAS", columns = c(Merokok_SUSENAS,Tembakau_SUSENAS,Elektrik_SUSENAS)) %>%
  tab_spanner(label = "SKI", columns = c(Merokok_SKI,Tembakau_SKI,Elektrik_SKI)) %>% 
  cols_label(Merokok_SUSENAS = "Merokok", Tembakau_SUSENAS = "Tembakau",Elektrik_SUSENAS = "Elektrik",
             Merokok_SKI = "Merokok", Tembakau_SKI  = "Tembakau", Elektrik_SKI  = "Elektrik")

agr %>% filter(usia <= 21) %>% group_by(source, usia) %>% 
  summarise(total_pop = sum(pop, na.rm = TRUE),rokok_all = sum(rokok_all, na.rm = TRUE),
            rokok_tembakau = sum(rokok_tembakau, na.rm = TRUE),rokok_elektrik = sum(rokok_elektrik, na.rm = TRUE),
            .groups = "drop") %>%  
  mutate(prev_all = rokok_all / total_pop, prev_tembakau = rokok_tembakau / total_pop,
         prev_elektrik = rokok_elektrik / total_pop) %>%
  select(source, usia,prev_all, prev_tembakau, prev_elektrik) %>%
  pivot_wider(names_from  = source,values_from = c(prev_all, prev_tembakau, prev_elektrik)) %>%
  transmute(usia,
            Merokok = percent(prev_all_SUSENAS - prev_all_SKI, accuracy = 0.01),
            Tembakau = percent(prev_tembakau_SUSENAS - prev_tembakau_SKI, accuracy = 0.01),
            Elektrik = percent(prev_elektrik_SUSENAS - prev_elektrik_SKI, accuracy = 0.01)) %>%
  gt(rowname_col = "usia", auto_align = FALSE)

#--> Viz 4: (POP) islands
agr %>% 
  group_by(source, island) %>% 
  summarise(total_pop = sum(pop, na.rm = T),
            rokok_all = sum(rokok_all, na.rm = T),
            rokok_tembakau = sum(rokok_tembakau, na.rm = T),
            rokok_elektrik = sum(rokok_elektrik, na.rm = T), .groups = "drop") %>%  
  group_by(source, island) %>% mutate(prev_all = rokok_all/total_pop,
                                    prev_tembakau = rokok_tembakau/total_pop,
                                    prev_elektrik = rokok_elektrik/total_pop) %>% ungroup() %>%
  mutate(Merokok = scales::percent(prev_all, accuracy = 0.01),
         Tembakau = scales::percent(prev_tembakau, accuracy = 0.01),
         Elektrik = scales::percent(prev_elektrik, accuracy = 0.01)) %>%
  select(source, island, Merokok, Tembakau, Elektrik) %>%
  pivot_wider(names_from = source, values_from = c(Merokok, Tembakau, Elektrik),
              names_sep = "_") %>% gt(rowname_col = "island") %>%
  tab_spanner(label = "SUSENAS", columns = c(Merokok_SUSENAS,Tembakau_SUSENAS,Elektrik_SUSENAS)) %>%
  tab_spanner(label = "SKI", columns = c(Merokok_SKI,Tembakau_SKI,Elektrik_SKI)) %>% 
  cols_label(Merokok_SUSENAS = "Merokok", Tembakau_SUSENAS = "Tembakau",Elektrik_SUSENAS = "Elektrik",
             Merokok_SKI = "Merokok", Tembakau_SKI  = "Tembakau", Elektrik_SKI  = "Elektrik")

agr %>% group_by(source, island) %>% 
  summarise(total_pop = sum(pop, na.rm = TRUE),rokok_all = sum(rokok_all, na.rm = TRUE),
            rokok_tembakau = sum(rokok_tembakau, na.rm = TRUE),rokok_elektrik = sum(rokok_elektrik, na.rm = TRUE),
            .groups = "drop") %>%  
  mutate(prev_all = rokok_all / total_pop, prev_tembakau = rokok_tembakau / total_pop,
         prev_elektrik = rokok_elektrik / total_pop) %>%
  select(source, island,prev_all, prev_tembakau, prev_elektrik) %>%
  pivot_wider(names_from  = source,values_from = c(prev_all, prev_tembakau, prev_elektrik)) %>%
  transmute(island,
            Merokok = percent(prev_all_SUSENAS - prev_all_SKI, accuracy = 0.01),
            Tembakau = percent(prev_tembakau_SUSENAS - prev_tembakau_SKI, accuracy = 0.01),
            Elektrik = percent(prev_elektrik_SUSENAS - prev_elektrik_SKI, accuracy = 0.01)) %>%
  gt(rowname_col = "island", auto_align = FALSE)

#--> Viz 5: (SMOKE) by sex
agr %>%
  group_by(source, jenis_kelamin) %>% 
  summarise(total_pop = sum(pop, na.rm = T),
            rokok_all = sum(rokok_all, na.rm = T),
            rokok_tembakau = sum(rokok_tembakau, na.rm = T),
            rokok_elektrik = sum(rokok_elektrik, na.rm = T), .groups = "drop") %>%  
  group_by(source, jenis_kelamin) %>% mutate(prev_all = rokok_all/total_pop,
                                          prev_tembakau = rokok_tembakau/total_pop,
                                          prev_elektrik = rokok_elektrik/total_pop) %>% ungroup() %>%
  mutate(Merokok = scales::percent(prev_all, accuracy = 0.01),
         Tembakau = scales::percent(prev_tembakau, accuracy = 0.01),
         Elektrik = scales::percent(prev_elektrik, accuracy = 0.01)) %>%
  select(source, jenis_kelamin, Merokok, Tembakau, Elektrik) %>%
  pivot_wider(names_from = source, values_from = c(Merokok, Tembakau, Elektrik),
              names_sep = "_") %>% gt(rowname_col = "jenis_kelamin") %>%
  tab_spanner(label = "SUSENAS", columns = c(Merokok_SUSENAS,Tembakau_SUSENAS,Elektrik_SUSENAS)) %>%
  tab_spanner(label = "SKI", columns = c(Merokok_SKI,Tembakau_SKI,Elektrik_SKI)) %>% 
  cols_label(Merokok_SUSENAS = "Merokok", Tembakau_SUSENAS = "Tembakau",Elektrik_SUSENAS = "Elektrik",
             Merokok_SKI = "Merokok", Tembakau_SKI  = "Tembakau", Elektrik_SKI  = "Elektrik")

agr %>% group_by(source, jenis_kelamin) %>% 
  summarise(total_pop = sum(pop, na.rm = TRUE),rokok_all = sum(rokok_all, na.rm = TRUE),
            rokok_tembakau = sum(rokok_tembakau, na.rm = TRUE),rokok_elektrik = sum(rokok_elektrik, na.rm = TRUE),
            .groups = "drop") %>%  
  mutate(prev_all = rokok_all / total_pop, prev_tembakau = rokok_tembakau / total_pop,
         prev_elektrik = rokok_elektrik / total_pop) %>%
  select(source, jenis_kelamin,prev_all, prev_tembakau, prev_elektrik) %>%
  pivot_wider(names_from  = source,values_from = c(prev_all, prev_tembakau, prev_elektrik)) %>%
  transmute(jenis_kelamin,
            Merokok = percent(prev_all_SUSENAS - prev_all_SKI, accuracy = 0.01),
            Tembakau = percent(prev_tembakau_SUSENAS - prev_tembakau_SKI, accuracy = 0.01),
            Elektrik = percent(prev_elektrik_SUSENAS - prev_elektrik_SKI, accuracy = 0.01)) %>%
  gt(rowname_col = "jenis_kelamin", auto_align = FALSE)
