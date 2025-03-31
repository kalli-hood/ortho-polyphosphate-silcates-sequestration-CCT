# Setup -------------------------------------------------------------------

library("tidyverse")
library("PNWColors")
library("ggpubr")

# Import ------------------------------------------------------------------

tot_data <- read_csv("data-figs/total-pb-with-gam-preds.csv")
diss_data <- read_csv("data-figs/diss-pb-with-gam-preds.csv")


est_d <- read.csv("data-figs/diss_pb-gam-estimates.csv") %>%
  mutate(type = "dissolved")
est_t <- read.csv("data-figs/tot_pb-gam-estimates.csv") %>%
  mutate(type = "total")

est <- rbind(est_d, est_t)


# plot --------------------------------------------------------------------

p1 <- tot_data %>%
  filter(element == "208Pb") %>%
  drop_na(si, tmp, mnfe, ph) %>%
  filter(
   # ph == -1,
  #  mnfe == -1,
    tmp != 0,
    si != 0
  ) %>%
  mutate(
    treatment = ifelse(si == -1 & tmp == -1, "1 ppm Orthophosphate (Control)",
                       ifelse(si == -1 & tmp == 1, "3 ppm TMP",
                              ifelse(si == 1 & tmp == -1, "20 ppm Si",
                                     ifelse(si == 1 & tmp == 1, "20 ppm Si x 3 ppm TMP", NA)))),
    treatment = factor(treatment, levels = c("20 ppm Si",
                                             "3 ppm TMP",
                                             "20 ppm Si x 3 ppm TMP",
                                             "1 ppm Orthophosphate (Control)")),
    ph = factor(ph, labels = c("pH 7.5", "pH 9.5")),
    mnfe = factor(mnfe, labels = c("0 mg Mn,Fe / L", "1 mg Mn,Fe / L"))
  ) %>%
  ggplot(
    aes(total_changes, value,
        col = as.factor(treatment),
        group = as.factor(cell))) +
  geom_point(alpha = 0.7) +
  geom_line(aes(y = epred), size =1, alpha = 0.8) +
  # scale_y_continuous(trans = "log10") +
  facet_grid(cols = vars(ph),
             rows = vars(mnfe),
             scales = "free_y") +
  ylim(0, 500) +
  scale_colour_manual(values = pnw_palette("Bay", n = 4)) +
  labs(
    y = "[Pb] (ppb)",
    col = "Treatment",
    x = element_blank()
  ) +
  theme_bw() +
  theme(text = element_text(size=14, face = "bold"),
        legend.position = "bottom")
p1

p2 <- diss_data %>%
  filter(element == "208Pb") %>%
  drop_na(si, tmp, mnfe, ph) %>%
  filter(
    # ph == -1,
    #  mnfe == -1,
    tmp != 0,
    si != 0
  ) %>%
  mutate(
    treatment = ifelse(si == -1 & tmp == -1, "1 ppm Orthophosphate (Control)",
                       ifelse(si == -1 & tmp == 1, "3 ppm TMP",
                              ifelse(si == 1 & tmp == -1, "20 ppm Si",
                                     ifelse(si == 1 & tmp == 1, "20 ppm Si x 3 ppm TMP", NA)))),
    treatment = factor(treatment, levels = c("20 ppm Si",
                                             "3 ppm TMP",
                                             "20 ppm Si x 3 ppm TMP",
                                             "1 ppm Orthophosphate (Control)")),
    ph = factor(ph, labels = c("pH 7.5", "pH 9.5")),
    mnfe = factor(mnfe, labels = c("0 mg Mn,Fe / L", "1 mg Mn,Fe / L"))
  ) %>%
  ggplot(
    aes(total_changes, value,
        col = as.factor(treatment),
        group = as.factor(cell))) +
  geom_point(alpha = 0.7) +
  geom_line(aes(y = epred), size=1) +
  # scale_y_continuous(trans = "log10") +
  ylim(0, 300) +
  facet_grid(cols = vars(ph),
             rows = vars(mnfe),
             scales = "free_y") +
  scale_colour_manual(values = pnw_palette("Bay", n = 4)) +
  labs(,
    col = "Treatment",
    y = "[Pb] (ppb)",
    x = element_blank()
  ) +
  theme_bw() +
  theme(text = element_text(size = 14, face = "bold"),
        legend.position = "bottom")
p2
