library("devtools")
library("roxygen2")

setwd("/Users/lokpoon/Desktop/programming repo/MoonShineR")
document()

setwd("/Users/lokpoon/Desktop/programming repo/")
install("MoonShineR")

.rs.restartR()



#####################################
library("MoonShineR")
?plot_lux

moonlight_output <- predict_lux(latitude = -4.21528, longitude = -69.94056, site_elev = 0, time_zone = "EST",
            date_start = "2023-02-27", time_start = "18:00:00", duration_day = 2, time_interval_minutes = 5,
            darksky_value = 0.0008, illuminance_type_plot = "moon_final_lux_nighttime",
            output_directory = NULL, export_table = FALSE, export_plot = FALSE,
            plot_width = 11, plot_height = 8.5, plot_y_max = "AUTO",  plot_dayttime_gray_mask = TRUE, plot_twilight = "astro")

plot_lux(df = moonlight_output, illuminance_type_plot = "total_illuminance_all", plot_y_max = "AUTO",  plot_dayttime_gray_mask = TRUE, plot_twilight = "astro",
         vertical_time_label = TRUE, time_label_interval_hr = 12, time_labe_shift_hr = -14)


str(moonlight_output)
vignette("rd")
vignette("rd-other")
vignette("rd-formatting")
vignette("reuse")
vignette("namespace")

library(suncalc)
?suncalc::getSunlightPosition

