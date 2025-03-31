
# libraries ---------------------------------------------------------------

library(tidyverse)

# import ------------------------------------------------------------------

turb <- read_csv("data-clean/CC_turb.csv")

# model -------------------------------------------------------------------

m1 <- lm(
  value ~ (si + tmp + ph + mnfe),
  data = turb
)

summary(m1)


m2 <- lm(
  value ~ (si + tmp + ph + mnfe)^2,
  data = turb
)

summary(m2)


# plot --------------------------------------------------------------------

# Generate predictions
new_data <- expand.grid(
  si = c(-1, 1),
  tmp = c(-1, 1),
  ph = c(-1, 1),
  mnfe = c(-1, 1)
)
new_data$pred <- predict(m2, newdata = new_data)

# Interaction plot
ggplot(new_data, aes(x = ph, y = pred, color = as.factor(mnfe), group = mnfe)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(x = "pH Level", y = "Predicted Turbidity",
       color = "MnFe Level", title = "Interaction Between pH and MnFe") +
  theme_minimal()

ggplot(new_data, aes(x = si, y = pred, color = as.factor(ph), group = ph)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(vars(tmp, mnfe)) +
  labs(x = "Silicate Level", y = "Predicted Turbidity",
       color = "MnFe Level", title = "Interaction Between Silicate and MnFe") +
  theme_minimal()

