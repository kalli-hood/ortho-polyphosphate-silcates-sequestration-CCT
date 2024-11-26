
# setup -------------------------------------------------------------------

library(tidyverse)
library(PNWColors)

theme_set(
  theme_classic() +
    theme(
      legend.position = "bottom"
    )
)

# import ------------------------------------------------------------------

data <- read_csv("data-clean/icp-ms_data.csv")

tot_data <- data %>%
  filter(type == "total")
diss_data <- data %>%
  filter(type == "dissolved")


# summarize ---------------------------------------------------------------

# P by P condition

data %>%
  filter(element == "31P") %>%
  group_by(phase, tmp) %>%
  summarise(
    min = min(value),
    median = median(value),
    max = max(value)
  )

# P by cell

data %>%
  filter(element == "31P",
         is.na(tmp) == FALSE) %>%
  ggplot(aes(value/1000, reorder(treatment, value), col = as.factor(treatment))) +
  geom_point() +
  facet_grid(
    cols = vars(phase),
    rows = vars(tmp)
  ) +
  theme(legend.position = "none") +
  labs(
    y = "",
    x = "[P] ppm"
  )

# P over time by treatment
data %>%
  filter(element == "31P") %>%
  drop_na() %>%
  ggplot(aes(total_changes, value, col = as.factor(tmp), group = cell)) +
  geom_line() +
  geom_vline(xintercept = 67, linetype = 2) +
  scale_colour_manual(values = pnw_palette("Bay"))

# Si over time by treatment
data %>%
  filter(element == "28Si") %>%
  ggplot(aes(total_changes, value, col = as.factor(si), group = cell)) +
  geom_line() +
  geom_vline(xintercept = 67, linetype = 2) +
  scale_colour_manual(values = pnw_palette("Bay"))

# Mn over time by treatment
data %>%
  filter(element == "55Mn") %>%
  ggplot(aes(total_changes, value, col = as.factor(mnfe), group = cell)) +
  geom_line() +
  geom_vline(xintercept = 67, linetype = 2) +
  scale_colour_manual(values = pnw_palette("Bay"))

data %>%
  filter(element == "56Fe") %>%
  ggplot(aes(total_changes, value, col = as.factor(mnfe), group = cell)) +
  geom_line() +
  geom_vline(xintercept = 67, linetype = 2) +
  scale_colour_manual(values = pnw_palette("Bay"))

data %>%
  filter(element == "208Pb",
         mnfe != 0,
         si != 0,
         tmp != 0,
         ph != 0) %>%
  ggplot(aes(total_changes, value, col = as.factor(ph), group = cell)) +
  geom_line() +
  scale_y_continuous(trans = "log10") +
  facet_grid(
    rows = vars(si),
    cols = vars(tmp)
  )
  geom_vline(xintercept = 67, linetype = 2)


# Pb by all conditions

data %>%
  filter(element == "208Pb") %>%
  filter(phase == "2") %>%
  filter(si != 0, tmp !=0, ph != 0, mnfe != 0) %>%
  drop_na(tmp, si, ph, mnfe) %>%
  ggplot(aes(as.factor(tmp), value, fill = as.factor(si))) +
  geom_boxplot() +
  facet_grid(rows = vars(mnfe),
             cols = vars(ph),
             labeller = label_both) +
  scale_y_continuous(trans = "log10",
                     breaks = c(1, 10, 100, 1000)) +
  scale_fill_manual(values = pnw_palette("Bay", n =2))


# simple gam model -----------------------------------------------------------

tot_pb <- tot_data %>% filter(element == "208Pb")

m0 <- gam(
  log(value) ~
    s(total_changes) +
    s(total_changes, cell, bs = "fs"),
  data = tot_pb,
  method = "REML"
)
summary(m0)
draw(m0)
sm <- smooth_estimates(m0, smooth = "s(total_changes)")

#plot

 sm %>%
  add_confint() %>%
  mutate(
    sig = ifelse(.estimate - .se > 0, 1,
               ifelse(.estimate + .se < 0, 1, 0))
  )  %>%
  ggplot(aes(y = .estimate, x = total_changes)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
              alpha = 0.2) +
  geom_line(aes(col = sig), linewidth = 1.5) +
  geom_vline(xintercept = 67, linetype =2) +
  labs(
    y = "Partial Effect",
    x = "Water Changes"
  )

# linear model ------------------------------------------------------------

model_in <- tot_data %>%
  filter(element == "208Pb") %>%
  filter(phase == "2") %>%
  drop_na(tmp, si, ph, mnfe) %>%
  group_by(cell, si, tmp, ph, mnfe) %>%
  summarize(value = median(value)) %>%
  ungroup()

# total Pb
m1 <- lm(value ~ (si + tmp + ph + mnfe)^2,
         data = model_in)
summary(m1)



model_in2 <- diss_data %>%
  filter(element == "208Pb") %>%
  filter(phase == "2") %>%
  drop_na(tmp, si, ph, mnfe) %>%
  group_by(cell, si, tmp, ph, mnfe) %>%
  summarize(value = median(value)) %>%
  ungroup()

# total Pb
m2 <- lm(value ~ (si + tmp + ph + mnfe)^2,
         data = model_in2)
summary(m2)


# phase 1 to 2: -----------------------------------------------------------

model_in3 <- tot_data %>%
  filter(element == "208Pb")

m3 <- lm(value ~ phase,
         data = model_in3)
m3
# time series -------------------------------------------------------------

data %>%
  filter(element == "208Pb") %>%
  filter(type == "total") %>%
  drop_na(tmp, si, ph, mnfe) %>%
  ggplot(aes(total_changes, value, col = as.factor(treatment))) +
  # facet_grid(rows = vars(tmp),
  #            cols = vars(si),
  #            labeller = label_both) +
  geom_line(aes(group = cell)) +
  scale_y_continuous(trans = "log10")

