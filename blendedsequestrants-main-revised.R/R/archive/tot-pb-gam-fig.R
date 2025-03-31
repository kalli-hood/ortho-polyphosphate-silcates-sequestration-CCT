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
  geom_line(aes(y = epred), size =1) +
  # scale_y_continuous(trans = "log10") +
  scale_colour_manual(values = pnw_palette("Bay", n = 4)) +
  labs(
    y = "[Pb] (ppb)",
    col = "Treatment",
    x = element_blank()
  ) +
  theme_bw() +
  theme(text = element_text(size=14))

p2 <- diss_data %>%
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
  ylim(0, 500) +
  scale_colour_manual(values = pnw_palette("Bay", n = 4)) +
  labs(,
    col = "Treatment",
    y = "[Pb] (ppb)",
    x = element_blank()
  ) +
  theme_bw() +
  theme(text = element_text(size = 14))


# plot 2 ------------------------------------------------------------------

p3 <- est %>%
#  filter(str_detect(term, ":") == FALSE) %>%
  mutate(
    term = factor(term,
                  levels = c(
                    "(Intercept)", "si", "tmp", "ph", "mnfe",
                    "si:tmp", "si:ph", "si:mnfe", "tmp:ph", "tmp:mnfe",
                    "ph:mnfe"
                  ),
                  labels = c(
                    "Intercept", "20 ppm Si", "3 ppm TMP", "pH 9.5", "1 ppm Mn+Fe",
                    "Si x TMP", "Si x pH 9.5", "Si x Mn+Fe", "TMP x pH 9.5", "TMP x Mn+Fe",
                    "pH 9.5 x Mn+Fe"
                  )),
    sig = ifelse(p.value < 0.05, 1, 0),
    sig = factor(sig, levels = c(0, 1), labels = c("p>0.05", "p<0.05"))
  ) %>%
  filter(term != "Intercept") %>%
  ggplot(aes(
    estimate, term,
    col = factor(sig),
    shape = type)) +
  geom_point(
    size = 3,
    position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 1, linetype = 3) +
  geom_errorbarh(aes(y = term, xmin = conf.low, xmax = conf.high),
                 height = 0,
                 position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = c(-50, -25, 0, 25, 50)) +
  labs(
    y = element_blank(),
    x = "% change in Pb",
    shape = "Pb",
    col = ""
  ) +
  scale_colour_manual(values = pnw_palette("Bay", n = 2)) +
 # guides(colour = "none") +
  theme_bw() +
  theme(text = element_text(size = 12),
        legend.position = "right")


p4 <- ggarrange(p1, p2, ncol = 1,
          common.legend = TRUE, legend = "bottom",
          labels = "AUTO")

ggarrange(p4, p3,
          widths = c(0.55, 0.45),
          labels = c("", "C"))
