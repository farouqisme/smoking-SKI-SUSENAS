
###############################################################################
##### Script untuk identifikasi perbedaan data SKI dan SUSENAS tahun 2023 #####
##### Analisis dibagi menjadi dua tahap; EDA dan inferensial              #####
###############################################################################

rm(list=ls())

library(tidyverse)
library(tidyr)
library(dplyr)
library(rio)
library(janitor)
library(tibble)
library(gt)

#---------- EDA ----------#

#--> Import Data
agr <- rio::import("Data/SUSENAS_SKI_AGR.dta")

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
    " (", scales::percent(pct, accuracy = 0.1), ")")) %>%
  select(source, urban, display_val) %>%
  pivot_wider(names_from = urban, values_from = display_val) %>% gt() 

#--> Viz 4: (POP) age
agr %>%
  group_by(source, usia_group) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
  group_by(source) %>% mutate(pct = total_pop / sum(total_pop)) %>%
  ungroup() %>%
  mutate(display_val = paste0(
    scales::number(total_pop, big.mark = ".", decimal.mark = ","),
    " (", scales::percent(pct, accuracy = 0.01), ")")) %>%
  select(source, usia_group, display_val) %>% tidyr::pivot_wider(
    names_from  = source,
    values_from = display_val) %>% gt(rowname_col = "usia_group", auto_align = F)

#--> Viz 5: (POP) islands
agr %>%
  group_by(source, island) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
  group_by(source) %>% mutate(pct = total_pop / sum(total_pop)) %>%
  ungroup() %>%
  mutate(display_val = paste0(
    scales::number(total_pop, big.mark = ".", decimal.mark = ","),
    " (", scales::percent(pct, accuracy = 0.01), ")")) %>%
  select(source, island, display_val) %>% tidyr::pivot_wider(
    names_from  = source,
    values_from = display_val) %>% gt(rowname_col = "island", auto_align = F)

#--> Viz 6: (POP) sex
agr %>%
  group_by(source, jenis_kelamin) %>%
  summarise(total_pop = sum(pop, na.rm = TRUE), .groups = "drop") %>%
  group_by(source) %>% mutate(pct = total_pop / sum(total_pop)) %>%
  ungroup() %>%
  mutate(display_val = paste0(
    scales::number(total_pop, big.mark = ".", decimal.mark = ","),
    " (", scales::percent(pct, accuracy = 0.01), ")")) %>%
  select(source, jenis_kelamin, display_val) %>% tidyr::pivot_wider(
    names_from  = source,
    values_from = display_val) %>% gt(rowname_col = "jenis_kelamin", auto_align = F)
