
# Setup -------------------------------------------------------------------

library("tidyverse")
library("readxl")
library("mgcv")
library("broom")
library("testthat")

# Import ------------------------------------------------------------------

tot_model_data <- read.csv("data-clean/model_data_tot.csv")
diss_model_data <- read_csv("data-clean/model_data_diss.csv")


# Clean -------------------------------------------------------------------

tot_model_data <- tot_model_data %>%
  mutate(ph = as.factor(pH_lvl),
         mnfe = as.factor(Mn_Fe_lvl),
         tmp = as.factor(trimeta_lvl),
         si = as.factor(Si_lvl)) %>%
  select(CC, date, value, ph, mnfe, tmp, si, total_changes) %>%
  filter(
    ph != 0,
    si != 0,
    mnfe != 0,
    tmp != 0
  )


diss_model_data <- diss_model_data %>%
  mutate(CC = as.factor(CC),
         pH_lvl = as.factor(pH_lvl),
         Mn_Fe_lvl = as.factor(Mn_Fe_lvl),
         trimeta_lvl = as.factor(trimeta_lvl),
         Si_lvl = as.factor(Si_lvl))

# model -------------------------------------------------------------------

# Total Pb
m2 <- gamm(
  log(value) ~
    (si + ph + tmp + mnfe)^2 +
   s(total_changes) +
   s(total_changes, by = CC, bs = "fs"),
  correlation = corAR1(form = ~total_changes | CC),
  data = tot_model_data,
  method = "REML"
)

tot_model <- broom::tidy(m2$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), exp))

leg <- get_legend(p1)

