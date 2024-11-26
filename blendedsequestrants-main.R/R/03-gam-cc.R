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


# Model Total: Two-way interaction + smooth term + CAR1 term -----------------

m4 <- gamm(
  log(value) ~
    si * tmp * ph * mnfe +
    # (si + tmp + ph + mnfe)^2 +
    s(total_changes, k=12, bs = "tp") +
    s(total_changes, cell, bs = "fs"),
  correlation = corAR1(form = ~total_changes | cell),
  data = model_data_t,
  method = "REML"
)

# percent change in lead in each factor 'high' condition relative to control
m_tot_pb <- broom::tidy(m4$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), ~100*(exp(2* .x)-1)))
write.csv(m_tot_pb, "data-figs/tot_pb-gam-percent-change.csv")

# effect of switching from -1 to 1 in each factor.
tot_pb_est <- broom::tidy(m4$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), ~(exp(2* .x))))
write.csv(tot_pb_est, "data-figs/tot_pb-gam-ests.csv")

# Model Diagnostics -------------------------------------------------------
appraise(m4$gam)
draw(m4$gam)

r1 <- residuals(m4$lme, type = "normalized")
r2 <- residuals(m4$lme, type = "response")

cor(r1[-1], lag(r1)[-1])
cor(r2[-1], lag(r2)[-1])

acf(r1)
acf(r2)



# retrieve and save predictions -------------------------------------------

tot_data_with_preds <- model_data_t %>%
  mutate(
    epred = exp(predict.gam(m4$gam, newdata = model_data_t))
  )

write.csv(tot_data_with_preds, "data-figs/total-pb-with-gam-preds.csv")


# Model 4D: Two-way interaction + smooth term + CAR1 term -----------------

m8 <- gamm(
  log(value) ~
    si * tmp * ph * mnfe +
    # (si + tmp + ph + mnfe)^2 +
    s(total_changes, k=20, bs = "tp") +
    s(total_changes, cell, bs = "fs"),
  correlation = corAR1(form = ~total_changes | cell),
  data = model_data_d,
  method = "REML"
)

# percent change
m_diss_pb <- broom::tidy(m8$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), ~100*(exp(2* .x) - 1)))
write.csv(m_diss_pb, "data-figs/diss_pb-gam-percent-change.csv")

# relative change for each factor at high level relative to control
diss_pb_est <- broom::tidy(m8$gam, parametric = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, starts_with("conf.")), ~(exp(2* .x))))

write.csv(diss_pb_est, "data-figs/diss_pb-gam-ests.csv")
# model diagnostics -------------------------------------------------------

r3 <- residuals(m8$lme, type = "normalized")
r4 <- residuals(m8$lme, type = "response")

cor(r3[-1], lag(r3)[-1])
cor(r4[-1], lag(r4)[-1])

acf(r3)
acf(r4)

plot(m8$gam)

# retrieve and save predictions -------------------------------------------

diss_data_with_preds <- model_data_d %>%
  mutate(
    epred = exp(predict.gam(m8$gam, newdata = model_data_d))
  )

write.csv(diss_data_with_preds, "data-figs/diss-pb-with-gam-preds.csv")


