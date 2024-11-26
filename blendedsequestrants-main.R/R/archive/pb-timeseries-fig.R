## This generates two plots:
## Times series of A (p1) total lead release and B (p2) dissolved lead release
## faceted by condition

# Libraries -------------------------------------------------------------------

library("tidyverse")
library("PNWColors")
library("ggplot2")
library("ggpubr")

# Import ------------------------------------------------------------------

data <- read_csv("data-clean/icp-ms_data.csv") %>%
  mutate(censored = factor(censored, levels = c("none", "left")))

treatment <- tribble(
  ~si, ~tmp, ~ph, ~inhibitor,
  -1, -1,    -1,  "Orthophosphate",
  -1, -1,    1,   "Orthophosphate x pH 9.5",
  -1,  1,    -1,   "TMP + OrthoP",
  -1,  1,    1,  "TMP + OrthoP x pH 9.5",
  1,  -1,    -1,  "Si + OrthoP",
  1,  -1,    1,   "Si + OrthoP x pH 9.5",
  1,   1,    -1,  "Si + TMP + OrthoP",
  1,   1,    1,  "Si + TMP + OrthoP x pH 9.5"
)

data_pb <- data %>%
  filter(element == "208Pb")

model_data_t <- data_pb %>%
  filter(type == "total") %>%
  filter(phase == 2)

model_data_d <- data_pb %>%
  filter(type == "dissolved") %>%
  filter(phase == 2)

data_pb <- data %>%
  filter(element == "208Pb") %>%
  left_join(treatment, by = c("si", "tmp", "ph")) %>%
  drop_na()

# plot --------------------------------------------------------------------

# Total Lead
p1 <- model_data_t %>%
  filter(element == "208Pb") %>%
  drop_na(si, tmp, mnfe, ph) %>%
  ggplot(
    aes(total_changes, value,
        col = as.factor(ph),
        group = as.factor(cell),
        shape = censored)) +
  facet_grid(
    cols = vars(tmp, si),
    rows = vars(mnfe),
    labeller = label_both,
    scales = "free_y"
  ) +
  geom_line(alpha = 0.7) +
  scale_y_continuous(trans = "log10") +
  scale_colour_manual(values = pnw_palette("Bay")) +
  labs(
    x = "Number of water changes", y = "[Pb] (ppb)",
    col = "pH"
  ) +
  theme_classic()
p1

# Dissolved Lead
p2 <- model_data_d %>%
  filter(element == "208Pb") %>%
  drop_na(si, tmp, mnfe, ph) %>%
  ggplot(
    aes(total_changes, value,
        col = as.factor(ph),
        group = as.factor(cell),
        shape = censored)) +
  facet_grid(
    cols = vars(tmp, si),
    rows = vars(mnfe),
    labeller = label_both,
    scales = "free_y"
  ) +
  geom_line(alpha = 0.7) +
  scale_y_continuous(trans = "log10") +
  scale_colour_manual(values = pnw_palette("Bay")) +
  labs(
    x = "Number of water changes", y = "[Pb] (ppb)",
    col = "pH"
  ) +
  theme_classic()
p2


# phase 1 data ------------------------------------------------------------

p3 <- data_pb %>%
  filter(phase == 1) %>%
  mutate(type = factor(type, labels = c("Dissolved Pb", "Total Pb"))) %>%
  ggplot(aes(total_changes, value,
             col = as.factor(type))) +
  geom_point(alpha = 0.5, position = "jitter") +
 # geom_point(alpha = 0.5) +
  scale_y_log10() +
  # facet_wrap(vars(type),
  #            scales = "free") +
  labs(x = "# water changes",
       y = "[Pb] ppb",
       col = "") +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, face = "bold")) +
  scale_colour_manual(values = pnw_palette("Bay", n= 2))

p4 <- data_pb %>%
  filter(phase == 1) %>%
  mutate(type = factor(type, labels = c("Dissolved Pb", "Total Pb"))) %>%
  ggplot(aes(x = value, y = as.factor(cell),
             col = as.factor(type))) +
  geom_boxplot() +
  # geom_point(alpha = 0.5) +
  scale_x_log10() +
  # facet_wrap(vars(type),
  #            scales = "free") +
  labs(y = "cell",
       x = "[Pb] ppb",
       col = "") +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, face = "bold")) +
  scale_colour_manual(values = pnw_palette("Bay", n= 2))


ggarrange(p4, p3, widths = c(0.6, 0.4), labels = "AUTO")
# phase 2 data ------------------------------------------------------------

data_pb %>%
  filter(phase == 2) %>%
  filter(inhibitor != "Si + TMP + OrthoP",
         inhibitor != "Si + TMP + OrthoP x pH 9.5") %>%
  mutate(mnfe = factor(mnfe, labels = c("0 ppm Mn,Fe", "1 ppm Mn, Fe")),
         type = factor(type, labels = c("Dissolved Pb", "Total Pb"))) %>%
  ggplot(aes(total_changes, value,
             group = as.factor(cell),
             col = inhibitor)) +
  geom_line() +
  # geom_point(alpha = 0.5) +
  scale_y_log10() +
  facet_grid(rows = vars(type),
             cols = vars(mnfe),
             scales = "free") +
  labs(x = "# water changes",
       y = "[Pb] ppb",
       col = "") +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, face = "bold")) +
  scale_colour_manual(values = pnw_palette("Bay", n= 7))

