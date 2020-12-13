shinyOptions(bslib = TRUE)
# bs_global_theme(bootswatch = "litera")
# my_theme <- bs_theme(
#   bg = "#202123", fg = "#B8BCC2", primary = "#EA80FC", 
#   base_font = font_google("Grandstander"),
#   "font-size-base" = "1.1rem"
# )

xadm1_mo <- read_csv("data/adm1_mo.csv")
xadm1_yr <- read_csv("data/adm1_yr.csv")

ctry_list <- unique(xadm1_mo$country) %>% sort()

plot_descr <- list("scatter" = "Select two indicators and time range for each. If time ranges are different for each indicator,
                   they need to be the same length (i.e. Jun - Aug 2020 for the first indicator, Jan - Mar 2019 for the second.",
                   "ts" = "Select two indicators and a time range",
                   "bar" = "[Bar plot or histogram description / instruction",
                   "map" = "Map displays indicator(s) values at a point in time. If a time range is selected, the indicators are summarized by taking
                   either the sum or mean across time")