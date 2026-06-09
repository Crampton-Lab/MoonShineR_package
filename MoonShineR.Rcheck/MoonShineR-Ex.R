pkgname <- "MoonShineR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "MoonShineR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('MoonShineR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("plot_lux")
### * plot_lux

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_lux
### Title: Create a plot from the illuminance prediction dataframe
### Aliases: plot_lux
### Keywords: moonlight

### ** Examples

# Plot a predict_lux generated dataframe named moonlight_output

# First create a set of data with predict_lux()
moonlight_output <- predict_lux(latitude = -4.21528, longitude = -69.94056, site_elev = 0,
                    time_zone = "EST", date_start = "2023-02-27", time_start = "18:00:00",
                    duration_day = 10.5, time_interval_minutes = 30, darksky_value = 0.0008,
                    output_directory = NULL, export_table = FALSE)

# Proceed to plotting
plot_lux(df = moonlight_output, illuminance_type_plot = "total_illuminance_all",
         plot_y_max = 0.3,  plot_daytime_gray_mask = TRUE, plot_eclipse_red_mask = TRUE,
         plot_twilight = "astro", vertical_time_label = TRUE, time_label_interval_hr = 24,
         time_label_shift_hr = -1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_lux", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("predict_lux")
### * predict_lux

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict_lux
### Title: Predict moonlight, sunlight, and twilight ground illuminance
### Aliases: predict_lux
### Keywords: moonlight

### ** Examples

# Predict the nighttime moonlight illuminance in Leticia, Colombia,
# for 14 days starting on 2023-02-27 at 6pm.

moonlight_output <- predict_lux(latitude = -4.21528, longitude = -69.94056, site_elev = 0,
                    time_zone = "EST", date_start = "2023-02-27", time_start = "18:00:00",
                    duration_day = 2, time_interval_minutes = 15, darksky_value = 0.0008,
                    output_directory = NULL, export_table = FALSE, show_progress = TRUE)

moonlight_output #return completed data frame



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict_lux", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
