##########################################################
##### Dukungan data cleaning untuk visualisasi       #####
##### progres penurunan prevalensi rokok 2015-2024   #####
##### berdasarkan subgroups                          #####
##########################################################
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
library(survey)
#--> Import Data
susenas15 <- rio::import("D:/dataset/BPS/Susenas/Susenas Maret 2015 - KOR/susenas15mar_ki.dta")
susenas24 <- rio::import("D:/dataset/BPS/Susenas/Susenas Maret 2024 - KOR/ssn202403_kor_ind1.dbf")


#--> Susenas 2015 ----

#--> Data cleaning
susenas15_clean <- susenas15 |>
  mutate(
    rokok_tembakau  = as.integer(r807 %in% c(1, 2)),
    mantan_tembakau = as.integer(r810 %in% c(1, 2)),
    int_merokok     = if_else(rokok_tembakau == 1, r809, NA_real_),
    cig10more       = if_else(!is.na(int_merokok), as.integer(int_merokok >= 70), NA_integer_),
    cig10less       = if_else(!is.na(int_merokok), as.integer(int_merokok <  70), NA_integer_),
    jenis_kelamin   = r405,
    usia            = r407,
    pop             = 1L,
    isl             = case_when(r101 < 30 ~ "Sumatera",
                                r101 %in% c(30:39) ~ "Jawa",
                                r101 %in% c(40:59) ~ "Bali-Nusra",
                                r101 %in% c(60:69) ~ "Kalimantan",
                                r101 %in% c(70:79) ~ "Sulawesi",
                                r101 %in% c(80:89) ~ "Maluku",
                                r101 > 90 ~ "Papua"),
    isl             = factor(isl, levels = c("Sumatera","Jawa","Bali-Nusra",
                                                "Kalimantan","Sulawesi",
                                                "Maluku","Papua")),
    FWT_int         = as.integer(ceiling(fwt_tahun))
  ) |>
  group_by(urut) |>
  mutate(ada_perokok_rt = max(rokok_tembakau)) |>
  ungroup() |>
  mutate(
    shs = case_when(
      rokok_tembakau == 0 & ada_perokok_rt == 0 ~ 0L,
      rokok_tembakau == 0 & ada_perokok_rt == 1 ~ 1L,
      TRUE ~ NA_integer_
    )
  )

#--> Collapse (weighted sum) by usia x jenis_kelamin
susenas15_collapsed <- susenas15_clean |>
  group_by(usia, jenis_kelamin, isl) |>
  summarise(across(c(pop, shs, rokok_tembakau, mantan_tembakau, cig10more, cig10less),
                   ~ sum(. * FWT_int, na.rm = TRUE)),
            .groups = "drop") |> mutate(tahun = 2015L)

#--> Susenas 2024 ----
#--> Data cleaning
susenas24_clean <- susenas24 |>
  mutate(
    rokok_tembakau  = as.integer(R1207 %in% c(1, 2)),
    mantan_tembakau = as.integer(R1209 %in% c(1, 2)),
    int_merokok     = if_else(rokok_tembakau == 1, R1208, NA_real_),
    cig10more       = if_else(!is.na(int_merokok), as.integer(int_merokok >= 70), NA_integer_),
    cig10less       = if_else(!is.na(int_merokok), as.integer(int_merokok <  70), NA_integer_),
    jenis_kelamin   = R405,
    usia            = R407,
    pop             = 1L,
    isl             = case_when(R101 < 30 ~ "Sumatera",
                                R101 %in% c(30:39) ~ "Jawa",
                                R101 %in% c(40:59) ~ "Bali-Nusra",
                                R101 %in% c(60:69) ~ "Kalimantan",
                                R101 %in% c(70:79) ~ "Sulawesi",
                                R101 %in% c(80:89) ~ "Maluku",
                                R101 > 90 ~ "Papua"),
    isl             = factor(isl, levels = c("Sumatera","Jawa","Bali-Nusra",
                                             "Kalimantan","Sulawesi",
                                             "Maluku","Papua")),
    FWT_int         = as.integer(ceiling(FWT))
  ) |>
  group_by(URUT) |>
  mutate(ada_perokok_rt = max(rokok_tembakau)) |>
  ungroup() |>
  mutate(
    shs = case_when(
      rokok_tembakau == 0 & ada_perokok_rt == 0 ~ 0L,
      rokok_tembakau == 0 & ada_perokok_rt == 1 ~ 1L,
      TRUE ~ NA_integer_
    )
  )

#--> Collapse (weighted sum) by usia x jenis_kelamin
susenas24_collapsed <- susenas24_clean |>
  group_by(usia, jenis_kelamin, isl) |>
  summarise(across(c(pop, shs, rokok_tembakau, mantan_tembakau, cig10more, cig10less),
                   ~ sum(. * FWT_int, na.rm = TRUE)),
            .groups = "drop") |> mutate(tahun = 2024L)


#--> Appending data ----
susenas <- rbind(susenas15_collapsed,susenas24_collapsed)

#--> Export
rio::export(susenas15_collapsed, "Data/by age jk for PAF (2015).xlsx")


#--> Hasil headline (usia 10+) ----

## 1. Prevalensi perokok tembakau
susenas |>
  filter(usia >= 10) |>
  group_by(tahun) |>
  summarise(prev = sum(rokok_tembakau) / sum(pop))
# 2015 = 26,6% ; 2024 = 26,3%

## 2. Jumlah perokok tembakau (orang, weighted)
susenas |>
  filter(usia >= 10) |>
  group_by(tahun) |>
  summarise(jumlah = sum(rokok_tembakau))
# 2015 = 55.385.816 orang (~55,4 juta) ; 2024 = 62.005.937 orang (~62,0 juta)

## 3. Prevalensi perokok tembakau per jenis kelamin
susenas |>
  filter(usia >= 10) |>
  mutate(jenis_kelamin = factor(jenis_kelamin, levels = c(1, 2),
                                labels = c("Laki-laki", "Perempuan"))) |>
  group_by(tahun, jenis_kelamin) |>
  summarise(prev = sum(rokok_tembakau) / sum(pop), .groups = "drop")
# 2015: Laki-laki = 52,1% ; Perempuan = 1,1%
# 2024: Laki-laki = 51,6% ; Perempuan = 0,9%


#--> Visualisasi ----

## Data dasar (usia 10-64 per tahun, 65+ diagregasi)
plot_dat <- susenas |>
  filter(usia >= 10) |>
  mutate(usia_num  = if_else(usia >= 65, 65L, as.integer(usia)),
         usia_plot = if_else(usia >= 65, "65+", as.character(usia))) |>
  group_by(tahun, usia_num, usia_plot) |>
  summarise(rokok = sum(rokok_tembakau), pop = sum(pop), .groups = "drop") |>
  mutate(prev       = rokok / pop,
         rokok_ribu = rokok / 1000,
         tahun      = factor(tahun))

warna_tahun    <- c("2015" = "steelblue", "2024" = "firebrick")
caption_sumber <- "Sumber: SUSENAS 2015 dan SUSENAS 2024, diolah"

## 4. Prevalensi perokok tembakau menurut umur
p_prev_umur <- ggplot(plot_dat, aes(usia_num, prev, color = tahun)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = c(seq(10, 64, 5), 65),
                     labels = c(seq(10, 64, 5), "65+")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = warna_tahun) +
  labs(x = "Usia", y = "Prevalensi", color = NULL,
       title   = "Prevalensi Perokok Tembakau menurut Umur, 2015 vs 2024",
       caption = caption_sumber) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "italic", size = 12),
        plot.caption     = element_text(hjust = 0, color = "gray40"))

ggsave("Output/prev-perokok-umur-2015-2024.png", p_prev_umur,
       width = 9, height = 5, dpi = 300)

## 5. Jumlah perokok tembakau menurut umur (ribu orang)
p_jml_umur <- ggplot(plot_dat, aes(usia_num, rokok_ribu, color = tahun)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = c(seq(10, 64, 5), 65),
                     labels = c(seq(10, 64, 5), "65+")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = warna_tahun) +
  labs(x = "Usia", y = "Jumlah perokok (ribu orang)", color = NULL,
       title   = "Jumlah Perokok Tembakau menurut Umur, 2015 vs 2024",
       caption = caption_sumber) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "italic", size = 12),
        plot.caption     = element_text(hjust = 0, color = "gray40"))

ggsave("Output/jumlah-perokok-umur-2015-2024.png", p_jml_umur,
       width = 9, height = 5, dpi = 300)

## 6. Prevalensi perokok tembakau menurut pulau
isl_dat <- susenas |>
  filter(usia >= 10) |>
  group_by(tahun, isl) |>
  summarise(prev = sum(rokok_tembakau) / sum(pop), .groups = "drop") |>
  mutate(tahun = factor(tahun))

## label + arrow: hitung selisih 2024 - 2015 per pulau, tempel di bar 2024
isl_arrow <- isl_dat |>
  tidyr::pivot_wider(names_from = tahun, values_from = prev, names_prefix = "y") |>
  mutate(arah  = if_else(y2024 >= y2015, "↑", "↓"),   # ↑ / ↓
         label = paste0(scales::percent(y2024, accuracy = 0.1), " ", arah))

p_prev_pulau <- ggplot(isl_dat, aes(isl, prev, fill = tahun)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_label(data = isl_arrow,
             aes(x = isl, y = y2024, label = label, fill = NULL),
             position    = position_dodge(width = 0.8),
             hjust = 0.5, vjust = -0.3, size = 3.2, color = "gray20",
             label.size  = 0.4, label.padding = unit(0.2, "lines"),
             fill = "white", inherit.aes = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, by = 0.05),
                     expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = warna_tahun) +
  labs(x = NULL, y = "Prevalensi", fill = NULL,
       title   = "Prevalensi Perokok Tembakau menurut Pulau, 2015 vs 2024",
       caption = caption_sumber) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "italic", size = 12),
        plot.caption     = element_text(hjust = 0, color = "gray40"))

p_prev_pulau

ggsave("Output/prev-perokok-pulau-2015-2024.png", p_prev_pulau,
       width = 9, height = 5, dpi = 300)
