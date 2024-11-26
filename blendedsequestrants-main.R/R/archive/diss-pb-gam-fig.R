# Setup -------------------------------------------------------------------

library("tidyverse")
library("PNWColors")

# Import ------------------------------------------------------------------

data <- read_csv("data-figs/diss-pb-with-gam-preds.csv")



# plot --------------------------------------------------------------------

data %>%
  filter(element == "208Pb",
         total_changes > 150) %>%
  drop_na(si, tmp, mnfe, ph) %>%
  filter(
    ph == -1,
    mnfe == -1,
    tmp != 0,
    si != 0
  ) %>%
  mutate(
    treatment = factor(treatment, levels = c(0, 1, 2, 3),
                       labels = c(
                         "Control", "20 ppm Si", "3 ppm P",
                         "20 ppm Si x 3 ppm P"
                       ))
  ) %>%
  ggplot(
    aes(total_changes, value,
        col = as.factor(treatment),
        group = as.factor(cell))) +
  geom_point(alpha = 0.7) +
  geom_line(aes(y = epred), size=1) +
  # scale_y_continuous(trans = "log10") +
  scale_colour_manual(values = pnw_palette("Bay", n = 4)) +
  labs(
    x = "Number of water changes", y = "[Pb] (ppb)",
    col = "Treatment"
  ) +
  theme_bw()
