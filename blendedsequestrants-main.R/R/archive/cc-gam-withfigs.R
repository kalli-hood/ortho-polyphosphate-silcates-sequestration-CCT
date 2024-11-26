# Setup -------------------------------------------------------------------

library("tidyverse")
library("mgcv")
library("broom")
library("gratia")
library("patchwork")
library("testthat")
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


# plot --------------------------------------------------------------------

# Total Lead
p1 <- model_data_t %>%
 # filter(phase == 2) %>%
  filter(element == "208Pb") %>%
  # filter(si != 0,
  #        tmp != 0,
  #        ph == -1,
  #        mnfe == -1) %>%
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
  geom_point(alpha = 0.7) +
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
 # filter(phase == 2) %>%
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
  geom_point(alpha = 0.7) +
  scale_y_continuous(trans = "log10") +
  scale_colour_manual(values = pnw_palette("Bay")) +
  labs(
    x = "Number of water changes", y = "[Pb] (ppb)",
    col = "pH"
  ) +
  theme_classic()
p2

# Model 1T: Additive ------------------------------------------------------

m1 <- gam(
  log(value) ~
    si + tmp + ph + mnfe,
  data = model_data_t,
  method = "REML"
)

summary(m1)

appraise(m1)

# acf(residuals(m1$lme, type = "response"))
# acf(residuals(m1$lme, type = "normalized"))

broom::tidy(m1, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), exp))

p1 + geom_line(aes(y = exp(fitted(m1))))


# Model 2T: Additive + Smooth Term ----------------------------------------
m2 <- gam(
  log(value) ~
    si + tmp + ph + mnfe +
    s(total_changes),
  data = model_data_t,
  method = "REML"
)

summary(m2)

appraise(m2)
acf(residuals(m2))

# acf(residuals(m1$lme, type = "response"))
# acf(residuals(m1$lme, type = "normalized"))

broom::tidy(m2, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), exp))

p1 + geom_line(aes(y = exp(fitted(m2))))


# Model 3T: Two-way interaction + Smooth ----------------------------------
m3 <- gam(
  log(value) ~
    (si + tmp + ph + mnfe)^2 +
    s(total_changes),
  data = model_data_t,
  method = "REML"
)

summary(m3)

appraise(m3)
acf(residuals(m3))

# acf(residuals(m1$lme, type = "response"))
# acf(residuals(m1$lme, type = "normalized"))

broom::tidy(m3, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), exp))

p1 + geom_line(aes(y = exp(fitted(m3))))



# Model 4T: Two-way interaction + smooth term + CAR1 term -----------------

m4 <- gamm(
  log(value) ~
    (si + tmp + ph + mnfe)^2 +
    s(total_changes, k=12, bs = "tp") +
    s(total_changes, cell, bs = "fs"),
  correlation = corAR1(form = ~total_changes | cell),
  data = model_data_t,
  method = "REML"
)

m_tot_pb <- broom::tidy(m4$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), exp))


r1 <- residuals(m4$lme, type = "normalized")
r2 <- residuals(m4$lme, type = "response")

cor(r1[-1], lag(r1)[-1])
cor(r2[-1], lag(r2)[-1])

acf(r1)
acf(r2)

p1 + geom_line(aes(y = exp(fitted(m4$gam))))

sub <- model_data_t %>%
  filter(element == "208Pb") %>%
  drop_na(si, tmp, mnfe, ph) %>%
  filter(
    ph == -1,
    mnfe == -1,
    tmp != 0,
    si != 0
  )

sub %>%
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
  geom_line(
    aes(y = exp(predict.gam(m4$gam, newdata = sub)))
  ) +
 # scale_y_continuous(trans = "log10") +
  scale_colour_manual(values = pnw_palette("Bay", n = 4)) +
  labs(
    x = "Number of water changes", y = "[Pb] (ppb)",
    col = "Treatment"
  ) +
  theme_bw()
# partial effects ---------------------------------------------------------




# Model 1D: Additive ------------------------------------------------------

m5 <- gam(
  log(value) ~
    si + tmp + ph + mnfe,
  data = model_data_d,
  method = "REML"
)

summary(m5)

appraise(m5)

# acf(residuals(m1$lme, type = "response"))
# acf(residuals(m1$lme, type = "normalized"))

broom::tidy(m5, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), exp))

p2 + geom_line(aes(y = exp(fitted(m5))))


# Model 2D: Additive + Smooth Term ----------------------------------------
m6 <- gam(
  log(value) ~
    si + tmp + ph + mnfe +
    s(total_changes),
  data = model_data_d,
  method = "REML"
)

summary(m6)

appraise(m6)
acf(residuals(m6))

# acf(residuals(m1$lme, type = "response"))
# acf(residuals(m1$lme, type = "normalized"))

broom::tidy(m6, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), exp))

p2 + geom_line(aes(y = exp(fitted(m6))))


# Model 3D: Two-way interaction + Smooth ----------------------------------
m7 <- gam(
  log(value) ~
    (si + tmp + ph + mnfe)^2 +
    s(total_changes),
  data = model_data_d,
  method = "REML"
)

summary(m7)

appraise(m7)
acf(residuals(m7))

# acf(residuals(m1$lme, type = "response"))
# acf(residuals(m1$lme, type = "normalized"))

broom::tidy(m7, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), exp))

p2 + geom_line(aes(y = exp(fitted(m7))))



# Model 4D: Two-way interaction + smooth term + CAR1 term -----------------

m8 <- gamm(
  log(value) ~
    (si + tmp + ph + mnfe)^2 +
    s(total_changes, k=20, bs = "tp") +
    s(total_changes, cell, bs = "fs"),
  correlation = corAR1(form = ~total_changes | cell),
  data = model_data_d,
  method = "REML"
)

broom::tidy(m8$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), exp))


r3 <- residuals(m4$lme, type = "normalized")
r4 <- residuals(m4$lme, type = "response")

cor(r3[-1], lag(r3)[-1])
cor(r4[-1], lag(r4)[-1])

acf(r3)
acf(r4)

plot(m4$gam)

p2 + geom_line(aes(y = exp(fitted(m8$gam))))

