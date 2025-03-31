# Setup -------------------------------------------------------------------

library("tidyverse")
library("broom")
library("mgcv")
# library("patchwork")
# library("testthat")
library("PNWColors")

# Import ------------------------------------------------------------------

data <- read_csv("data-clean/icp-ms_data.csv") %>%
  mutate(censored = factor(censored, levels = c("none", "left")))

data_pb <- data %>%
  filter(element == "208Pb") %>%
  drop_na()

model_data_t <- data_pb %>%
  filter(type == "total") %>%
  filter(phase == 2)

model_data_d <- data_pb %>%
  filter(type == "dissolved") %>%
  filter(phase == 2)


# models -------------------------------------------------------------------

mt <- gamm(
  log(value) ~
    si * tmp * ph * mnfe +
    # (si + tmp + ph + mnfe)^2 +
    s(total_changes, k=12, bs = "tp"),
  #  s(total_changes, cell, bs = "fs"),
  correlation = corAR1(form = ~total_changes | cell),
  data = model_data_t,
  method = "REML"
)

md <- gamm(
  log(value) ~
    si * tmp * ph * mnfe +
    # (si + tmp + ph + mnfe)^2 +
    s(total_changes, k=12, bs = "tp"),
    # s(total_changes, cell, bs = "fs"),
  correlation = corAR1(form = ~total_changes | cell),
  data = model_data_d,
  method = "REML"
)


# Build a grid of input values --------------------------------------------

preds_grid_total <- model_data_t %>%
  filter(phase == 2) %>%
  with(crossing(
    tmp = seq(-1, 1, length.out = 50),
    si = seq(-1, 1, length.out = 50),
    mnfe = seq(-1, 1, by = 1),
    ph = seq(-1, 1, by = 1),
    total_changes = seq(min(total_changes), max(total_changes), by = 3)
  )) %>%
  mutate(preds = predict(mt$gam, newdata = .))


# plot --------------------------------------------------------------------
ph_labs <- c("pH 7.5", "pH 8.5", "pH 9.5")
names(ph_labs) <- c(-1, 0, 1)
mnfe_labs = c("[Mn,Fe] 0 ppm", "[Mn,Fe] 0.5 ppm", "[Mn,Fe] 1 ppm")
names(mnfe_labs) <- c(-1, 0, 1)


p1 <- preds_grid_total |>
  filter(mnfe != 0,
         si != 0,
         ph != 0,
         tmp != 0) %>%
  group_by(tmp, si, mnfe, ph) %>%
  summarize(preds = mean(exp(preds))) %>%
  ungroup() %>%
  ggplot(aes(tmp, si)) +
  geom_tile(aes(fill = preds)) +
  geom_contour(aes(z = preds), col = "white") +
  scale_fill_viridis_c() +
  facet_grid(rows = vars(ph),
             cols = vars(mnfe),
             labeller = labeller(
               ph = ph_labs,
               mnfe = mnfe_labs
             )) +
 labs(fill = "Predicted Pb (ppb)",
      y = "Silicate ppm",
      x = "TMP ppm") +
  scale_y_continuous(breaks = c(-1, 0, 1),
                     labels = c("0", "10", "20")) +
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("0", "1.5", "3")) +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 12, face = 'bold'))




### Repeat for dissolved model
# Build a grid of input values --------------------------------------------

preds_grid_diss <- model_data_d %>%
  filter(phase == 2) %>%
  with(crossing(
    tmp = seq(-1, 1, length.out = 50),
    si = seq(-1, 1, length.out = 50),
    mnfe = seq(-1, 1, by = 1),
    ph = seq(-1, 1, by = 1),
    total_changes = seq(min(total_changes), max(total_changes), by = 3)
  )) %>%
  mutate(preds = predict(md$gam, newdata = .))


# plot --------------------------------------------------------------------
ph_labs <- c("pH 7.5", "pH 8.5", "pH 9.5")
names(ph_labs) <- c(-1, 0, 1)
mnfe_labs = c("[Mn,Fe] 0 ppm", "[Mn,Fe] 0.5 ppm", "[Mn,Fe] 1 ppm")
names(mnfe_labs) <- c(-1, 0, 1)


p2 <- preds_grid_diss |>
  filter(mnfe != 0,
         si != 0,
         ph != 0,
         tmp != 0) %>%
  group_by(tmp, si, mnfe, ph) %>%
  summarize(preds = mean(exp(preds))) %>%
  ungroup() %>%
  ggplot(aes(tmp, si)) +
  geom_tile(aes(fill = preds)) +
  geom_contour(aes(z = preds), col = "white") +
  scale_fill_viridis_c() +
  facet_grid(rows = vars(ph),
             cols = vars(mnfe),
             labeller = labeller(
               ph = ph_labs,
               mnfe = mnfe_labs
             )) +
  labs(fill = "Predicted Pb (ppb)",
       y = "Silicate ppm",
       x = "TMP ppm") +
  scale_y_continuous(breaks = c(-1, 0, 1),
                     labels = c("0", "10", "20")) +
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c("0", "1.5", "3")) +
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(size = 12, face= "bold"))

ggarrange(p1, p2, ncol = 1)

