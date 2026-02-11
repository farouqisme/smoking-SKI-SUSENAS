
#####################################################################
#####        Rscript untuk generate data tingkat faskes (podes) #####
#####        untuk estimasi level kabupaten kota                #####
#####################################################################

# Salah satu bentuk strata implisit SKI adalah indeks aksesibilitas faskes.
# maka, script ini bertujuan menguji apakah faktor tersebut memengaruhi perbedaan
# prevalensi antar PODES dan SKI. Namun, karena tidak disebutkan detil metode 
# yang digunakan maka disusun variabel proksi yang menggambarkan indeks
# aksesibilitas.

# Variabel yang dikonstruksi adalah standardized HH Index untuk mengukur konsentrasi
# Faskes di tingkat Kabupaten/Kota berdasarkan ketersediaannya di masing-masing desa,
# disesuaikan dengan jumlah populasi desa tersebut. Data Faskes bersumber dari Podes 
# 2021, sedangkan data Populasi menggunakan data population gridded level (Landscan)
# di tahun yang sama

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
library(sf)
library(modelsummary)
library(broom)
library(fixest)
library(patchwork)
library(officer)
library(flextable)

#--> Import data
podes <- rio::import("C:/Users/ASUS/OneDrive/DATASET BUAT OLAH-OLAH/BPS/Podes/Podes 2021/Podes21.dta")
id_shp <- sf::st_read("C:/Users/ASUS/OneDrive/DATASET BUAT OLAH-OLAH/idn_adm_bps_20200401_shp/idn_admbnda_adm3_bps_20200401.shp")

# path_bd <- "Data/RBI10K_ADMINISTRASI_DESA_20230928.gdb"
# st_layers(path_bd)
# id_bd <- st_read(dsn = path_bd,layer = "ADMINISTRASI_AR_DESAKEL")
# id_bd2 <- sf::st_drop_geometry(id_bd)

#--> Konstruksi data faskes
podes <- podes %>% mutate(rs      = rowSums(across(c(R704AK2,R704BK2)),na.rm = T),
                          pusk    = rowSums(across(c(R704CK2,R704DK2)),na.rm = T),
                          poli_tp = rowSums(across(c(R704FK2,R704GK2,R704HK2,R704IK2)),na.rm = T)) %>% 
  mutate(faskes = rowSums(across(c(rs,pusk,poli_tp))))

podes_tes <- podes %>% select(.,c(R101,R102,R103,R104,R104N_x)) %>% group_by(R101,R102,R103) %>% summarise(,.groups = "drop") %>%
  mutate(kodewil = paste0(R101,sprintf("%02d",R102))) %>%
  group_by(kodewil) %>% summarise(total=n())

podes_cln_des <- podes %>% mutate(kodeprov = R101, kodewil = paste0(R101,sprintf("%02d",R102)),
                              kodekec = paste0(kodewil,sprintf("%03d",R103)), kodedes = paste0(kodekec,sprintf("%03d",R104))) %>% 
  dplyr::select(., c(kodeprov,kodewil,kodekec,kodedes,R103N,R104N,rs,pusk,poli_tp,faskes))

podes_cln_kec <- podes_cln_des %>% group_by(kodeprov,kodewil,kodekec) %>% summarise(across(where(is.numeric), sum))

#--> Checking data SHP
tes_shp <- id_shp %>% sf::st_drop_geometry(geometry)
tes_shp <- tes_shp %>% dplyr::select(., c(ADM1_EN,ADM2_EN,ADM2_PCODE,ADM3_EN,ADM3_PCODE)) %>%
  mutate(kodekec = substr(ADM3_PCODE,3,nchar(ADM3_PCODE)))

merge_tes <- podes_cln_kec %>% anti_join(., tes_shp,by=c("kodekec"))
tes <- merge_tes %>% group_by(kodewil) %>% summarise(n_=n())
est <- left_join(podes_tes,tes,by=c("kodewil"))

#--> perhitungan HHI_Watson2017
n_desa <- podes_cln_des %>% group_by(kodewil) %>% summarise(n_desa = n())
podes_des_hhi <- podes_cln_des %>% group_by(kodewil) %>% 
  mutate(t_faskes = sum(faskes,na.rm = T),part1_faskes = (faskes/t_faskes)^2) %>% ungroup() %>%
  group_by(kodewil) %>% summarise(hhi_faskes = sum(part1_faskes,na.rm = T),.groups = "drop")
podes_des_hhi <- left_join(podes_des_hhi,n_desa,by=c("kodewil")) %>% 
  mutate(hhi_faskes = (hhi_faskes - 1/n_desa)/(1/n_desa))

min_val <- min(podes_des_hhi$hhi_faskes)
max_val <- max(podes_des_hhi$hhi_faskes)

podes_des_hhi <- podes_des_hhi %>% mutate(hhi_faskes_std = (hhi_faskes - min_val)/(max_val - min_val))

rio::export(podes_des_hhi,"Data/HHI Faskes PODES 2021.csv")
