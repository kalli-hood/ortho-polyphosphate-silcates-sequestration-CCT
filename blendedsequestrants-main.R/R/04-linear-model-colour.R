

# import ------------------------------------------------------------------

colour <- read_csv("data-clean/CC_colour.csv")

# model -------------------------------------------------------------------

# Apparent colour
colour_app <- colour %>%
  filter(param == "Apparent Colour",
       #  mnfe == 1,
         si != 0,
         tmp != 0,
         ph != 0) %>%
  group_by(CC, si, tmp, ph, mnfe) %>%
  summarize(value_med = median(value, na.rm = TRUE)) %>%
  ungroup()

m1 <- lm(
  value_med ~ (si + tmp + ph + mnfe)^2,
  data = colour_app
)

summary(m1)

# True colour
colour_true <- colour %>%
  filter(param == "True Colour",
         #  mnfe == 1,
         si != 0,
         tmp != 0,
         ph != 0) %>%
  group_by(CC, si, tmp, ph, mnfe) %>%
  summarize(value_med = median(value, na.rm = TRUE)) %>%
  ungroup()

m2 <- lm(
  value_med ~ (si + tmp + ph + mnfe)^2,
  data = colour_true
)

summary(m2)



