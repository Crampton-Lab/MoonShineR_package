library(devtools)
library(roxygen2)

setwd("/Users/lokpoon/Desktop/programming repo/MoonShineR")

devtools::document()
devtools::load_all()

plot_lux


#####################################
library("MoonShineR")
?MoonShineR::predict_lux
?MoonShineR::plot_lux

moonlight_output <- predict_lux(latitude = -4.21528, longitude = -69.94056, site_elev = 0,
                                time_zone = "EST", date_start = "2026-08-26", time_start = "18:00:00",
                                duration_day = 14, time_interval_minutes = 10, darksky_value = 0.0008,
                                output_directory = NULL, export_table = FALSE)

plot_lux(df = moonlight_output, illuminance_type_plot = "total_illuminance_all", plot_y_max = "AUTO",  plot_daytime_gray_mask = FALSE, plot_twilight = "astro",
         vertical_time_label = TRUE, time_label_interval_hr = 12, time_label_shift_hr = -1)


plot_lux(df = moonlight_output, illuminance_type_plot = "total_illuminance_all",
         plot_y_max = 2,  plot_daytime_gray_mask = TRUE, plot_eclipse_red_mask = FALSE,
         plot_twilight = "astro", vertical_time_label = TRUE, time_label_interval_hr = 24,
         time_label_shift_hr = -1)

str(moonlight_output)
vignette("rd")
vignette("rd-other")
vignette("rd-formatting")
vignette("reuse")
vignette("namespace")

library(suncalc)
?suncalc::getSunlightPosition
MoonShineR::plot_lux
