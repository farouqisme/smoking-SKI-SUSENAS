
#####################################################################
#####        Rscript untuk generate analisis inferensial        #####
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
library(broom)
library(fixest)
library(patchwork)
library(officer)
library(flextable)

#--> Import Data
ind <- rio::import("Data/IND_SUSENAS_SKI.dta")
nmprov <- rio::import("Data/Nama Provinsi BPS.xlsx")

#--> Prep
nmprov <- nmprov %>% mutate(kodeprov = as.character(kodeprov))
ind <- left_join(ind,nmprov,by=c("kodeprov"))

## Age group
ind <- ind %>% 
  mutate(usia_group = case_when(usia <= 14 ~ "10-14",usia %in% c(15:18) ~ "15-18",
                                usia %in% c(19:25) ~ "19-25", usia %in% c(26:30) ~ "26-30",
                                usia %in% c(31:35) ~ "31-35", usia %in% c(36:40) ~ "36-40",
                                usia %in% c(41:45) ~ "41-45", usia %in% c(46:50) ~ "46-50",
                                usia %in% c(51:55) ~ "51-55", usia %in% c(56:60) ~ "56-60",
                                usia %in% c(61:65) ~ "61-65", usia > 65 ~ ">66"))

RT <- ind %>% group_by(kode_RT) %>% summarise(jum_RT = n())
ind <- left_join(ind,RT,by=c("kode_RT"))
  
ind <- ind %>% mutate(
    rokok_all = as.integer(rokok_all),rokok_tembakau = as.integer(rokok_tembakau),
    rokok_elektrik = as.integer(rokok_elektrik),source = factor(source),urban = as.integer(urban),
    kodeprov = factor(kodeprov),usia_group = factor(usia_group),usia_factor = factor(usia))


#--> 1) Logit univariate
m1_all <- glm(rokok_all ~ as.factor(source),data = ind,family = binomial(link = "logit"))
m1_tembakau <- glm(rokok_tembakau ~ as.factor(source),data = ind,family = binomial(link = "logit"))
m1_elektrik <- glm(rokok_elektrik ~ as.factor(source),data = ind,family = binomial(link = "logit"))

#--> 2) Logit multivariate
## Est
controls <- c("usia", "jenis_kelamin", "jum_RT", "kodeprov")

m2_all <- glm(as.formula(paste("rokok_all ~ as.factor(source) +",paste(controls, collapse = " + "))),
          data = ind,family = binomial(link = "logit"))
m2_tembakau <- glm(as.formula(paste("rokok_tembakau ~ as.factor(source) +",paste(controls, collapse = " + "))),
              data = ind,family = binomial(link = "logit"))
m2_elektrik <- glm(as.formula(paste("rokok_elektrik ~ as.factor(source) +",paste(controls, collapse = " + "))),
              data = ind,family = binomial(link = "logit"))

## Reg Table
#1) All
m12_all_obj <- modelsummary(
  list("Univariate" = m1_all,"Multivariate" = m2_all),
  exponentiate = TRUE, statistic = "({std.error})",
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),coef_omit = "^kodeprov",
  add_rows = data.frame(term = "Province Fixed Effects",`Univariate` = "No",`Multivariate` = "Yes"),
  title = "Estimasi logit untuk perokok \nsource: SKI = 1",output = "flextable")

#2) Tembakau
m12_tembakau_obj <- modelsummary(
  list("Univariate" = m1_tembakau,"Multivariate" = m2_tembakau),
  exponentiate = TRUE, statistic = "({std.error})",
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),coef_omit = "^kodeprov",
  add_rows = data.frame(term = "Province Fixed Effects",`Univariate` = "No",`Multivariate` = "Yes"),
  title = "Estimasi logit untuk perokok tembakau \nsource: SKI = 1",output = "flextable")

#3) Elektrik
m12_elektrik_obj <- modelsummary(
  list("Univariate" = m1_elektrik,"Multivariate" = m2_elektrik),
  exponentiate = TRUE, statistic = "({std.error})",
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),coef_omit = "^kodeprov",
  add_rows = data.frame(term = "Province Fixed Effects",`Univariate` = "No",`Multivariate` = "Yes"),
  title = "Estimasi logit untuk perokok elektrik \nsource: SKI = 1",output = "flextable")


m12_doc <- read_docx() %>%
  body_add_par("Regression Results", style = "heading 1") %>%
  body_add_flextable(m12_all_obj) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(m12_tembakau_obj) %>% 
  body_add_par("", style = "Normal") %>%
  body_add_flextable(m12_elektrik_obj)


print(m12_doc, target = "Output/ovr-logit-res.docx")



#--> 3) Subgroup: Province
m3_all <- ind %>% group_by(nama_prov, kodeprov) %>%
  group_modify(~ {
    m <- feglm(rokok_all ~ as.factor(source) + usia + jenis_kelamin + jum_RT,
               data = .x,family = binomial())
    tidy(m)}) %>%
  ungroup() %>% filter(str_detect(term, "^as.factor\\(source\\)")) %>%
  mutate(or = exp(estimate),or_l = exp(estimate - 1.96 * std.error),or_u = exp(estimate + 1.96 * std.error))

m3_tembakau <- ind %>% group_by(nama_prov, kodeprov) %>%
  group_modify(~ {
    m <- feglm(rokok_tembakau ~ as.factor(source) + usia + jenis_kelamin + jum_RT,
               data = .x,family = binomial())
    tidy(m)}) %>%
  ungroup() %>% filter(str_detect(term, "^as.factor\\(source\\)")) %>%
  mutate(or = exp(estimate),or_l = exp(estimate - 1.96 * std.error),or_u = exp(estimate + 1.96 * std.error))

m3_elektrik <- ind %>% group_by(nama_prov, kodeprov) %>%
  group_modify(~ {
    m <- feglm(rokok_elektrik ~ as.factor(source) + usia + jenis_kelamin + jum_RT,
               data = .x,family = binomial())
    tidy(m)}) %>%
  ungroup() %>% filter(str_detect(term, "^as.factor\\(source\\)")) %>%
  mutate(or = exp(estimate),or_l = exp(estimate - 1.96 * std.error),or_u = exp(estimate + 1.96 * std.error))

prov_order <- m3_all %>%
  arrange(desc(or)) %>%
  pull(nama_prov) %>%
  unique()

plot_or <- function(data, show_y = TRUE, show_x = TRUE, title_text = NULL) {
  ggplot(
    data %>% mutate(nama_prov = factor(nama_prov, levels = prov_order)),
    aes(x = or, y = nama_prov)
  ) +
    geom_point() +
    geom_errorbarh(aes(xmin = or_l, xmax = or_u), height = 0) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    labs(
      x = if (show_x) "Odds Ratio (SKI = 1)" else NULL,
      y = if (show_y) "Provinsi" else NULL,
      title = title_text
    ) +
    theme_minimal() +
    theme(axis.text.y = if (show_y) element_text() else element_blank(),
          axis.ticks.y = if (show_y) element_line() else element_blank())
  }

p_all <- plot_or(m3_all,show_y = TRUE,show_x = FALSE,title_text = "Perokok")
p_tembakau <- plot_or(m3_tembakau,show_y = FALSE,title_text = "Perokok Tembakau")
p_elektrik <- plot_or(m3_elektrik,show_y = FALSE,show_x = FALSE,title_text = "Perokok Elektrik")
p_all | p_tembakau | p_elektrik
ggsave("Output/res-by-province.png",get_last_plot())

#--> 4) Subgroup: Age
m4_all <- ind %>% group_by(usia) %>%
  group_modify(~ {
    m <- feglm(rokok_all ~ as.factor(source) + kodeprov + jenis_kelamin + jum_RT,
               data = .x,family = binomial())
    tidy(m)}) %>%
  ungroup() %>% filter(str_detect(term, "^as.factor\\(source\\)")) %>%
  mutate(or = exp(estimate),or_l = exp(estimate - 1.96 * std.error),
         or_u = exp(estimate + 1.96 * std.error), tipe = "Merokok")

m4_tembakau <- ind %>% group_by(usia) %>%
  group_modify(~ {
    m <- feglm(rokok_tembakau ~ as.factor(source) + kodeprov + jenis_kelamin + jum_RT,
               data = .x,family = binomial())
    tidy(m)}) %>%
  ungroup() %>% filter(str_detect(term, "^as.factor\\(source\\)")) %>%
  mutate(or = exp(estimate),or_l = exp(estimate - 1.96 * std.error),
         or_u = exp(estimate + 1.96 * std.error),tipe = "Merokok Tembakau")

m4_elektrik <- ind %>% group_by(usia) %>%
  group_modify(~ {
    m <- feglm(rokok_elektrik ~ as.factor(source) + kodeprov + jenis_kelamin + jum_RT,
               data = .x,family = binomial())
    tidy(m)}) %>%
  ungroup() %>% filter(str_detect(term, "^as.factor\\(source\\)")) %>%
  mutate(or = exp(estimate),or_l = exp(estimate - 1.96 * std.error),
         or_u = exp(estimate + 1.96 * std.error),tipe = "Merokok Elektrik")

m4 <- bind_rows(m4_all,m4_tembakau,m4_elektrik)

ggplot(m4, aes(x = usia, y = or, color = tipe, fill = tipe)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = or_l, ymax = or_u),
              alpha = 0.2,color = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = "Usia",y = "Odds Ratio (SKI = 1)",color = "Jenis Rokok",fill = "Jenis Rokok") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave("Output/res-by-age.png",get_last_plot())

