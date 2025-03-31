
# library -----------------------------------------------------------------

library("ggplot2")
library("ggpubr")
library("tidyverse")
library("PNWColors")

# import ------------------------------------------------------------------

colour <-read.csv("data-clean/CC_colour.csv")
turb <-read.csv("data-clean/CC_turb.csv")

# manip -------------------------------------------------------------------

colour <- colour %>%
  mutate(
    treatment = ifelse(si == -1 & tmp == -1, "Orthophosphate",
                       ifelse(si == 1 & tmp == -1, "Si + OrthoP",
                              ifelse(si == -1 & tmp == 1, "TMP + OrthoP",
                                     ifelse(si == 1 & tmp ==1, "Si + TMP + OrthoP", "Centre Point"))))
  )

turb <- turb %>%
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
  filter(mnfe ==1,
         tmp != 0,
         si != 0,
         ph != 0,
       # param == "Apparent Colour",
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

p2 <- turb %>%
  group_by(si, ph, tmp, mnfe, treatment) %>%
  summarize(sd = sd(value, na.rm = TRUE),
            value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(mnfe != 0,
         tmp != 0,
         si != 0,
         ph != 0,
         # param == "Apparent Colour",
         treatment != "Si + TMP + OrthoP") %>%
  mutate(
    ph = factor(ph, labels = c("7.5", "9.5")),
    mnfe = factor(mnfe, labels= c("0 ppm Mn,Fe", "1 ppm Mn,Fe"))
  ) %>%
  ggplot(aes(x = factor(treatment), y = value, fill = factor(ph))) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(x = factor(treatment), ymin = value - sd, ymax = value + sd),
                position = position_dodge2(padding = 0.5)) +
  # geom_hline(yintercept = 15, linetype = 2, col = "red") +
  facet_grid(rows = vars(mnfe),
             scales = "free_y") +
  labs(x = "", y = "Turbidity (NTU)", fill = "pH") +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, face = "bold")) +
  scale_fill_manual(values = pnw_palette("Bay"))


# combine into one plot ---------------------------------------------------

p <- ggarrange(p1, p2, common.legend = TRUE, legend = "bottom", labels = "AUTO")

ggsave("figures/figure6.jpg", p, width = 8, height = 5, dpi = 300)
