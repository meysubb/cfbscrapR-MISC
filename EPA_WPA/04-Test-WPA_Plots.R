source("../plotting_funcs/2_wpa_plot.R")
sc_ga <- df %>% filter(
  year == 2019,
  offense_play %in% c("Georgia","South Carolina"),
  defense_play %in% c("Georgia","South Carolina")
) %>% create_wpa(wp_mod = wp_model)

away_color <- c(SC='#73000A')
home_color <- c(UGA='#BA0C2F')

wpa_plot_func(sc_ga,away_color,home_color,year=2019,winner="away")

ttu_bu <- epa_w %>% filter(
  year == 2019,
  offense %in% c("Penn State","Iowa"),
  defense %in% c("Penn State","Iowa")
) %>% create_wpa(wp_mod = wp_model)

away_color <- c(PSU="#041E42")
home_color <- c(IOWA="#FA4616")

wpa_plot_func(ttu_bu,away_color,home_color,year=2019,winner="away")

lsu_bama <- epa_w %>% filter(
  year == 2019,
  offense_play %in% c("LSU","Alabama"),
  defense_play %in% c("LSU","Alabama")
) %>% create_wpa(wp_mod = wp_model)

away_color <- c(LSU="#461D7C")
home_color <- c(BAMA="#9E1B32")

wpa_plot_func(lsu_bama,away_color,home_color,year=2019,winner="away")

# pitt_penn_st <- epa_w %>% filter(
#   year == 2019,
#   offense %in% c("Pittsburgh","Penn State"),
#   defense %in% c("Pittsburgh","Penn State")
# ) %>% create_wpa(wp_mod = wp_model)
#
# away_color <- c(PITT="#FFB81C")
# home_color <- c(PSU="#041E42")
#
# wpa_plot_func(pitt_penn_st,away_color,home_color,year=2019,winner="home")
#
#
# uk_uf <- epa_w %>% filter(
#   year == 2019,
#   offense %in% c("Kentucky","Florida"),
#   defense %in% c("Kentucky","Florida")
# ) %>% create_wpa(wp_mod = wp_model)
#
# home_color <- c(UK="#0033A0")
# away_color <- c(UF="#FA4616")
#
# wpa_plot_func(uk_uf,away_color,home_color,year=2019,winner="away")
#
# iowa_isu <- epa_w %>% filter(
#   year == 2019,
#   offense %in% c("Iowa","Iowa State"),
#   defense %in% c("Iowa","Iowa State")
# ) %>% create_wpa(wp_mod = wp_model)
#
# home_color <- c(ISU="#C8102E")
# away_color <- c(IOWA="#FA4616")
#
# wpa_plot_func(iowa_isu,away_color,home_color,year=2019,winner="away")
