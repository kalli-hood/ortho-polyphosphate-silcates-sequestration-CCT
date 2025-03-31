
# library -----------------------------------------------------------------

library("ggplot2")
library("tidyverse")
library("PNWColors")

# import ------------------------------------------------------------------

colour <-read.csv("data-clean/CC_colour.csv")


# manip -------------------------------------------------------------------

colour <- colour %>%
  mutate(
    treatment = ifelse(si == -1 & tmp == -1, "Orthophosphate",
                       ifelse(si == 1 & tmp == -1, "Si + OrthoP",
                              ifelse(si == -1 & tmp == 1, "TMP + OrthoP",
                                     ifelse(si == 1 & tmp ==1, "Si + TMP + OrthoP", "Centre Point"))))
  )


# plot --------------------------------------------------------------------

p1 <- colour %>%
  group_by(si, ph, tmp, mnfe, param, treatment) %>%
  summarize(sd = sd(value, na.rm = TRUE),
            value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(mnfe == 1,
         tmp != 0,
         si != 0,
         ph != 0,
        param == "Apparent Colour",
         treatment != "Si + TMP + OrthoP") %>%
  mutate(
    ph = factor(ph, labels = c("7.5", "9.5"))
  ) %>%
  ggplot(aes(x = factor(treatment), y = value, fill = factor(ph))) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(x = factor(treatment), ymin = value - sd, ymax = value + sd),
                    position = position_dodge2(padding = 0.5)) +
  geom_hline(yintercept = 15, linetype = 2, col = "red") +
  facet_grid(rows = vars(param),
             scales = "free_y") +
  labs(x = "", y = "Colour (Pt-Co)", fill = "pH") +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, face = "bold")) +
  scale_fill_manual(values = pnw_palette("Bay"))
p1


p2 <- colour %>%
  group_by(si, ph, tmp, mnfe, param, treatment) %>%
  summarize(sd = sd(value, na.rm = TRUE),
            value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(mnfe == 1,
         tmp != 0,
         si != 0,
         ph != 0,
         param == "True Colour",
         treatment != "Si + TMP + OrthoP") %>%
  mutate(
    ph = factor(ph, labels = c("7.5", "9.5"))
  ) %>%
  ggplot(aes(x = factor(treatment), y = value, fill = factor(ph))) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(x = factor(treatment), ymin = value - sd, ymax = value + sd),
                position = position_dodge2(padding = 0.5)) +
  #geom_hline(yintercept = 15, linetype = 2, col = "red") +
  facet_grid(rows = vars(param),
             scales = "free_y") +
  labs(x = "", y = "Colour (Pt-Co)", fill = "pH") +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, face = "bold")) +
  scale_fill_manual(values = pnw_palette("Bay"))
p2

ggarrange(p1, p2, ncol = 1, common.legend = TRUE, legend = "bottom")


