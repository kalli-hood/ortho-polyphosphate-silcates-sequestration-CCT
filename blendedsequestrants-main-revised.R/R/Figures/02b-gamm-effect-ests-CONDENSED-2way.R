
# import ------------------------------------------------------------------

tot_model_per_change <- read.csv("data-figs/tot_pb-gam-percent-change.csv") %>%
  mutate(type = "Total Pb")
diss_model_per_change <- read.csv("data-figs/diss_pb-gam-percent-change.csv") %>%
  mutate(type = "Dissolved Pb")

per_change <- rbind(tot_model_per_change, diss_model_per_change)

# plot 2 ------------------------------------------------------------------

per_change%>%
  filter(term != "(Intercept)") %>%
   # filter(str_detect(term, ":") == FALSE) %>%
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
  theme(text = element_text(size = 12, face = "bold"),
        legend.position = "right")
