
# library -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(PNWColors)

# import ------------------------------------------------------------------

tot_pb_est <- read_csv("data-figs/tot_pb-gam-ests.csv")
diss_pb_est <- read_csv("data-figs/diss_pb-gam-ests.csv")

data <- read_csv("data-clean/icp-ms_data.csv") %>%
  mutate(censored = factor(censored, levels = c("none", "left")))

# manip -------------------------------------------------------------------

model_intercept <- 47.2

tot_pb_est <- tot_pb_est %>%
  mutate(
    Mn_Fe = factor(ifelse(str_detect(term, "mnfe"), "1 ppm", "0 ppm")),
    pH = factor(ifelse(str_detect(term, "ph"), "9.5", "7.5")),
    Treatment = factor(term, levels = c("(Intercept)", "si", "tmp", "ph", "mnfe", "si:tmp",
                                        "si:ph", "tmp:ph", "si:mnfe", "tmp:mnfe", "ph:mnfe",
                                        "si:tmp:ph", "si:tmp:mnfe", "si:ph:mnfe", "tmp:ph:mnfe", "si:tmp:ph:mnfe"),
                       labels = c("OrthoP", "OrthoP + Si", "OrthoP + TMP", "OrthoP", "OrthoP", "OrthoP + Si + TMP",
                                  "OrthoP + Si", "OrthoP + TMP", "OrthoP + Si", "OrthoP + TMP", "OrthoP",
                                  "OrthoP + Si + TMP", "OrthoP + Si + TMP", "OrthoP + Si", "OrthoP + TMP", "OrthoP + Si + TMP")),
    multiplier = ifelse(str_detect(term, "(Intercept)") == TRUE, 1,
                        ifelse(str_detect(term, ":") == FALSE, 47.2,
                               ifelse(term == "si:tmp", 47.2*1.08*1.23,
                                      ifelse(term == "si:ph", 47.2*1.08*0.761,
                                             ifelse(term == "si:mnfe", 47.2*1.08*1.41,
                                                    ifelse(term == "tmp:ph", 47.2*1.23*0.761,
                                                           ifelse(term == "tmp:mnfe", 47.2*1.23*1.41,
                                                                  ifelse(term ==  "ph:mnfe", 47.2*0.761*1.41,
                                                                         ifelse(term == "si:tmp:ph", 47.2*1.08*1.23*0.761*0.963*1.12*0.85,
                                                                                ifelse(term == "si:tmp:mnfe", 49.99,
                                                                                       ifelse(term == "si:ph:mnfe", 47.2*1.08*0.761*1.41*1.12*0.87*0.938,
                                                                                              ifelse(term =="tmp:ph:mnfe", 47.2*1.23*0.761*1.41*0.85*0.87*0.938,
                                                                                                     ifelse(term =="si:tmp:ph:mnfe", 47.2*1.08*1.226*0.762*1.41*0.963*112*0.85*0.87*0.938*0.895*0.918*0.838*0.986, NA))))))))))))),
    est_pb2 = ifelse(str_detect(term, "(Intercept)"), 47.2, estimate*multiplier),
    conf.low2 = ifelse(str_detect(term, "(Intercept)"), 47.2 - 47.2*std.error, conf.low*multiplier),
    conf.high2 = ifelse(str_detect(term, "(Intercept)"), 47.2 + 47.2*std.error, conf.high*multiplier),
    type = "Total")


diss_pb_est <- diss_pb_est %>%
  mutate(
    Mn_Fe = factor(ifelse(str_detect(term, "mnfe"), "1 ppm", "0 ppm")),
    pH = factor(ifelse(str_detect(term, "ph"), "9.5", "7.5")),
    Treatment = factor(term, levels = c("(Intercept)", "si", "tmp", "ph", "mnfe", "si:tmp",
                                        "si:ph", "tmp:ph", "si:mnfe", "tmp:mnfe", "ph:mnfe",
                                        "si:tmp:ph", "si:tmp:mnfe", "si:ph:mnfe", "tmp:ph:mnfe", "si:tmp:ph:mnfe"),
                       labels = c("OrthoP", "OrthoP + Si", "OrthoP + TMP", "OrthoP", "OrthoP", "OrthoP + Si + TMP",
                                  "OrthoP + Si", "OrthoP + TMP", "OrthoP + Si", "OrthoP + TMP", "OrthoP",
                                  "OrthoP + Si + TMP", "OrthoP + Si + TMP", "OrthoP + Si", "OrthoP + TMP", "OrthoP + Si + TMP")),
    multiplier = ifelse(str_detect(term, "(Intercept)") == TRUE, 1,
                    ifelse(str_detect(term, ":") == FALSE, 17,
                           ifelse(term == "si:tmp", 17*1.296*1.54,
                                  ifelse(term == "si:ph", 17*1.296*0.662,
                                         ifelse(term == "si:mnfe", 17*1.296*0.555,
                                                ifelse(term == "tmp:ph", 17*1.54*0.662,
                                                       ifelse(term == "tmp:mnfe", 17*1.54*0.555,
                                                              ifelse(term == "ph:mnfe", 17*0.662*0.55,
                                                                     ifelse(term == "si:tmp:ph", 17*1.296*1.54*0.662*1.123*1.242*0.591,
                                                                            ifelse(term == "si:tmp:mnfe", 17*1.296*1.54*0.555*1.123*1.033*0.975,
                                                                                   ifelse(term == "si:ph:mnfe", 17*1.296*0.662*0.555*1.242*1.033*0.866,
                                                                                          ifelse(term == "tmp:ph:mnfe", 17*1.540*0.662*0.555*0.591*0.975*0.866,
                                                                                                 ifelse(term == "si:tmp:ph:mnfe", 17*1.296*1.54*0.662*0.555*1.123*1.242*0.591*1.033*0.975*0.866*0.915*1.024*0.857*0.823*0.754, NA))))))))))))),
    est_pb2 = ifelse(str_detect(term, "(Intercept)"), 17, estimate*multiplier),
    conf.low2 = ifelse(str_detect(term, "(Intercept)"), 17 - std.error*17, conf.low*multiplier),
    conf.high2 = ifelse(str_detect(term, "(Intercept)"), 17 + std.error*17, conf.high*multiplier),
    type = "Dissolved")

est_pb <- rbind(diss_pb_est, tot_pb_est)


est_pb %>%
  mutate(Mn_Fe = factor(Mn_Fe, labels = c("0 ppm Mn,Fe", "1 ppm Mn,Fe"))) %>%
  filter(Treatment != "OrthoP + Si + TMP") %>%
  ggplot(aes(y = Treatment, x = est_pb2, fill = pH)) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(xmin = conf.low2, xmax = conf.high2),
                position = position_dodge2(padding = 0.5)) +
  facet_grid(cols = vars(type),
             rows = vars(Mn_Fe),
             scales = "free_x") +
  labs(x = "Estimate [Pb] ppb") +
  scale_fill_manual(values = pnw_palette("Bay")) +
  theme_classic() +
  theme(text = element_text(size = 12, face = "bold"))
# plot --------------------------------------------------------------------


# plot --------------------------------------------------------------------

data %>%
  filter(element == "208Pb") %>%
  filter(phase == 2) %>%
  filter(si != 0,
         tmp != 0,
         ph != 0,
         mnfe != 0) %>%
  group_by(type, si, tmp, ph, mnfe) %>%
  summarise(
    geo_mean = mean(log(value)),
    geo_sd = sd(log(value))
  ) %>%
  ungroup() %>%
  mutate(
    CCT = ifelse(si == -1 & tmp == -1, "OrthoP (Control)",
                 ifelse(si == 1 & tmp == -1, "OrthoP + Si",
                        ifelse(si == -1 & tmp == 1, "OrthoP + TMP",
                               ifelse(si == 1 & tmp == 1, "OrthoP + Si + TMP", NA)))),
    value = exp(geo_mean),
    sd = exp(geo_sd),
    mnfe = factor(mnfe, levels = c(-1, 1), labels = c("0 ppm Mn/Fe", "1 ppm Mn/Fe")),
    ph = factor(ph, levels = c(-1, 1), labels = c("7.5", "9.5")),
    type = factor(type, labels = c("Dissolved Pb", "Total Pb"))
  ) %>%
  filter(CCT != "OrthoP + Si + TMP") %>%
  ggplot(aes(x = value, y = CCT, fill = ph)) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_errorbar(aes(xmin = value - sd, xmax = value + sd),
                position = position_dodge2(padding = 0.5)) +
  facet_grid(rows = vars(mnfe),
             cols = vars(type),
             scales = "free_x") +
  labs(x="Mean Pb", y = "", fill = "pH") +
  scale_fill_manual(values = pnw_palette("Bay")) +
  theme_bw() +
  theme(text = element_text(size = 14))

# OLD ---------------------------------------------------------------------




# effects of blends when mn/fe present
# product of each main effect si/tmp/ph and their interaction term
est_pb <- tribble(
  ~term,             ~estimate,          ~type,
  "Control",         47.2,             "total",
  "Si x MnFe",       1.08*1.41*0.877,  "total",
  "TMP x MnFe",      1.22*1.41*0.874,  "total",
  "pH 9.5 x MnFe",   0.759*1.41*0.937, "total",
  "Ortho x MnFe",    1.41,             "total",
  "Control",         17.0,               "dissolved",
  "Si x MnFe",       1.30*0.557*1.03,    "dissolved",
  "TMP x MnFe",      1.54*0.557*0.87,    "dissolved",
  "pH 9.5 x MnFe",   0.660*0.557*0.864,  "dissolved",
  "Ortho x MnFe",    0.557,              "dissolved"
) %>%
  mutate(
    rel_change = ifelse(type == "total", 100*(estimate - 1.41), 100*(estimate - 0.557)),
    est_conc_pb = ifelse(type == "total", estimate*47.2, estimate*17)
  )

# est_pb %>%
#   filter(term != c("Control")) %>%
#   filter(term != c("Ortho x MnFe")) %>%
#   ggplot() +
#   geom_bar(aes(x = term, y = rel_change, fill = type),
#            position = "dodge2",
#            stat = "identity") +
#   # facet_grid(rows = vars(type),
#   #            scales = "free_y") +
#   labs(y = "% change in Pb") +
#   theme_bw() +
#   scale_fill_manual(values = pnw_palette("Bay"))


est_pb %>%
  filter(term != "Control") %>%
  ggplot(aes(x = term, y = est_conc_pb, fill = type)) +
  geom_bar(position = "dodge2",
           stat = "identity") +
  geom_text(aes(label = paste0(round(rel_change),"%")),
            position = position_dodge(width =0.9),
            vjust = -0.5) +
  # facet_grid(rows = vars(type),
  #            scales = "free_y") +
  labs(y = "Esimated [Pb] ppb",
       x = "",
       fill = "") +
  theme_bw() +
  scale_fill_manual(values = pnw_palette("Bay")) +
  theme(text = element_text(size = 14))
