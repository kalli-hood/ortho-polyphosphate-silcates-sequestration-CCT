
# Setup ------------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(PNWColors)

phosphate_data <- read_excel("data-supplementary/Phosphate.xlsx")
silicat_data <- read_excel("data-supplementary/SilicateZeta.xlsx")
zeta <- read_excel("data-supplementary/cell_zeta.xlsx")

# Plot Phosphate --------------------------------------------------------------------

phosphate_data %>%
 # group_by(-Trial) %>%
 # mutate(phosphate_med = median(Phosphate, na.rm = TRUE)) %>%
  #ungroup() %>%
  ggplot(aes(x = Hour, y = Phosphate, col = as.factor(pH))) +
  geom_point(size = 3) +
 # geom_line() +
  scale_colour_manual(values = pnw_palette("Bay")) +
  theme_bw() +
  theme(legend.position = "bottom",
        text= element_text(size =12, face = "bold")) +
  labs(y = "Orthophosphate [PO4 ppm]", col = "pH",
       col = "pH")


# Silicate ----------------------------------------------------------------

silicat_data %>%
  group_by(pH, Silicate) %>%
  mutate(zeta_mean = mean(Zeta)) %>%
  ungroup() %>%
  ggplot(aes(x = pH, y = zeta_mean, col = as.factor(Silicate))) +
  geom_point(size = 3) +
  # geom_line() +
  scale_colour_manual(values = pnw_palette("Bay")) +
  theme_bw() +
  theme(legend.position = "bottom",
        text= element_text(size =12, face = "bold")) +
  labs(y = "Zeta Potential [mV]", col = "Silicate")

# Zeta ---------------------------------------------------------------------

zeta %>%
  filter(Trimetaphosphate == 0) %>%
  ggplot(aes(x = Time, y = Zeta, col = as.factor(pH), shape = as.factor(Metals))) +
  geom_point(size = 3, position = position_jitter(width = 2), alpha = 0.8) +
  # geom_line() +
  scale_colour_manual(values = pnw_palette("Bay")) +
  theme_bw() +
  theme(legend.position = "bottom",
        text= element_text(size =12, face = "bold")) +
  labs(y = "Zeta Potential [mV]", col = "pH", shape = "[Mn, Fe] ppm")


