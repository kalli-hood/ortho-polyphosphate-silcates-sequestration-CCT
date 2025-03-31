

# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggcorrplot)

# import ------------------------------------------------------------------

# read in files & convert to formats compatible for merging
turb <- read_csv("data-clean/CC_turb.csv") |>
  select(-`...1`) |>
  pivot_wider(names_from = param, values_from = value)
colour <- read_csv("data-clean/CC_colour.csv") |>
  select(-c(`...1`)) |>
  pivot_wider(names_from = param, values_from = value) |>
  rename(colour_app = `Apparent Colour`,
         colour_true = `True Colour`)
size <- read_csv("data-clean/CC_size.csv") |>
  select(-`...1`)

## lead dataset
diss <- read_csv("data-clean/model_data_diss.csv") |>
  rename(dissolved_pb_ppb = value) |>
  select(-c(Si_dose, Mn_Fe_dose, pH_dose, trimeta_dose))
tot <- read_csv("data-clean/model_data_tot.csv") |>
  rename(total_pb_ppb = value) |>
  select(-c(Si_dose, Mn_Fe_dose, pH_dose, trimeta_dose))

# merge -------------------------------------------------------------------

data_pb <- diss |>
  left_join(tot, by = c("date", "CC", "Si_lvl", "pH_lvl", "Mn_Fe_lvl",
                        "trimeta_lvl", "total_changes")) |>
  rename(si = Si_lvl,
         tmp = trimeta_lvl,
         ph = pH_lvl,
         mnfe = Mn_Fe_lvl) |>
  # since dates don't match exactly (sampling days/week may differ)
  ## collapse date to week
  mutate(date = as.Date(date),
         week = floor_date(date, "week"))



data <- turb |>
  left_join(colour, by = c("CC", "treatment", "ph", "mnfe",
                           "si", "tmp")) |>
  left_join(size, by = c("CC", "treatment", "ph", "mnfe",
                         "si", "tmp")) |>
  # since dates don't match exactly (sampling days/week may differ)
  ## collapse date to month
  mutate(date = as.Date(date),
         week = floor_date(date, "week"))

data_full <- data |>
  left_join(data_pb, by = c("week", "CC", "ph", "mnfe", "si", "tmp")) |>
  select(-date.x, -date.y) |>
  mutate(particulate_pb = total_pb_ppb - dissolved_pb_ppb,
         particulate_pb = ifelse(particulate_pb < 0, NA, particulate_pb),
         si = factor(si),
         tmp = factor(tmp),
         mnfe = factor(mnfe),
         ph = factor(ph)) |>
  drop_na(particulate_pb)

# correlation -------------------------------------------------------------

# Compute correlation with particulate lead
cor_spearman <- cor(data_full %>%
                     select(Turb_avg, Size_avg, colour_app,
                            particulate_pb),
                   method = "spearman")
print(cor_spearman)

# Spearman correlation heatmap
ggcorrplot(cor_spearman, method = "circle", type = "lower", title = "Spearman Correlations")


# linear models -----------------------------------------------------------

m1 <- lm(colour_app ~ (mnfe + ph + si + tmp)^2,
         data_full)
summary(m1)

m2 <- lm(Turb_avg ~ (mnfe + ph + si + tmp)^2,
         data_full)
summary(m2)

m3 <- lm(Size_avg ~ (mnfe + ph + si + tmp)^2,
         data_full)
summary(m3)
