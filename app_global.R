# shinyOptions(bslib = TRUE)


xadm1_mo <- read_civis(sql("SELECT * FROM staging_pmihq.selfserv_adm1_mo")) %>%
  # date is somehow read as factor 
  mutate(date = as.Date(as.character(date))) %>%
  rename(label = variable_name_short) %>%
  # characters are read as factor and affect some of app functionality
  mutate_at(c("variable", "country", "admin_level_1", "label"), .funs = list(~ as.character(.)))

xadm1_yr <- read_civis(sql("SELECT * FROM staging_pmihq.selfserv_adm1_yr")) %>%
  mutate(date = as.Date(as.character(date))) %>%
  rename(label = variable_name_short)  %>%
  # characters are read as factor and affect some of app functionality
  mutate_at(c("variable", "country", "label"), .funs = list(~ as.character(.)))

# xadm1_mo <- read_csv("data/adm1_mo.csv")
# xadm1_yr <- read_csv("data/adm1_yr.csv") %>%
  # add date here -- temporary patch as this should be done in the previous step (data processing)
#   mutate(date = as.Date(paste(year, "-01-01", sep = "")))

ctry_list <- unique(xadm1_mo$country) %>% sort()

plot_descr <- list("scatter" = "Select two indicators and time range for each. If time ranges are different for each indicator,
                   they need to be the same length (i.e. Jun - Aug 2020 for the first indicator, Jan - Mar 2019 for the second.",
                   "ts" = "Select two indicators and a time range",
                   "bar" = "[Bar plot or histogram description / instruction",
                   "map" = "Map displays indicator(s) values at a point in time. If a time range is selected, the indicators are summarized by taking
                   either the sum or mean across time")

clr10 <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
           "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6")