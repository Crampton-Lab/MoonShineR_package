#' Predict moonlight, sunlight, twilight ground illuminance
#'
#' * predict_lux() predicts moonlight, sunlight, and/or twilight ground illumination in lux for any defined geographical location and time period. It creates a data.frame output and automatically plot to the console. Automatic export of the table (.csv) and plot (.pdf) is optional.
#' * User is informed about the presence of lunar eclipse during the simulated period as a console messeage. The illuminance reduction during lunar eclipse is not modeled.
#' * To create a moonlight or sunlight LED schedule for light re-creation using MoonShineP, user need to download the R script version of the MoonShineR: Moonlight scheduler or MoonShineR: Sunlight/twilight scheduler from GitHub repository: <https://github.com/Crampton-Lab/MoonShine>
#' * To learn more about MoonShineR, see instruction manual: <https://lokpoon.github.io/moonshine_manual/overview.html>
#' @param latitude `numeric`. Latitude in decimal degrees (e.g., `-4.21528`).
#' @param longitude `numeric`. Longitude in decimal degrees (e.g., `-69.94056`).
#' @param site_elev `numeric`. Site elevation in meters (e.g., `0` is sea level). Default is `0`. Elevation correction only applies to moonlight but not sunlight and twilight. Site elevation of a coordinate location can be obtained from <https://www.dcode.fr/earth-elevation>.
#' @param time_zone `character`. Time zone for the location set (e.g., `“EST”`). Remember to change time_zone to corrospond it to the location set. For a list of time zone names, enter `OlsonNames(tzdir = NULL)` in R console. Use a time zone without DST to avoid confusion (e.g., use `"EST"` instead of `"America/New_York"`).
#' @param date_start `character`. Starting date of the simulation (`"YYYY-MM-DD"`).
#' @param time_start `character`. Starting time of the simulation (`"hh:mm:ss"`). Default is `"00:00:00"`.
#' @param duration_day `numeric`. Duration of the simulation in days.
#' @param time_interval_minutes `numeric`.The temporal resolution of the simulation in minutes. E.g., `5` calculates the illuminance every 5 minutes. Using small time interval requires longer computation time. Default is `5`.
#' @param darksky_value `numeric`. A baseline illuminance (in lux) added to the model to represent other constant nocturnal light sources (e.g., starlight and airglow). Default is `0.0008`. Change it to zero if a completely dark sky is preferred.
#' @param illuminance_type_plot `character`. Choose one type of illuminance to plot. See options in the section below. Default is `"moon_final_lux_nighttime"`.
#' @param output_directory `character`. Directory to save the output table (.csv) and plot (.pdf). Ignore output_directory if the two export options are turned OFF (i.e., `export_table = FALSE` and `export_plot = FALSE`).
#' @param export_table `logical`. `TRUE` to export output .csv table to the output_directory. `FALSE` to disable. Default is `FALSE`.
#' @param export_plot `logical`. `TRUE` to export output .pdf plot to the output_directory. `FALSE` to disable. Default is `FALSE`.
#' @param plot_width `numeric`. The exported .pdf plot width in inch. Default is `11`.
#' @param plot_height `numeric`. The exported .pdf plot height in inch. Default is `8.5`.
#' @param plot_y_max `character` `"AUTO"`, or `numeric`. Let the plot y-axis scale automatically or manually set a y-axis upper limit. Affects both the plot in the plot window and the exported .pdf. Default is `"AUTO"`.
#' @param plot_dayttime_gray_mask `logical`. `TRUE` to mask daytime plot line in gray. Affects both the plot in the plot window and the exported .pdf. `FALSE` to disable (plot line always black). Default is `TRUE`.
#' @param plot_twilight `character`. Set the twilight period to plot as a gray area. `"astro"` is astronomical twilight (longest). `"nautic"` is nautical twilight (intermediate). `"civil"` is civil twilight (shortest). `"none"` to disable plotting of twilight period. Default is `"astro"`.
#' @details
#' # `illuminance_type_plot` options:
#' * **"moon_final_lux"** plots only the illuminance of moonlight (plus the darksky_value) during both day and night.
#' * **"moon_final_lux_nighttime"** plots only the illuminance of moonlight at night (no value during daytime, when `sun_altitude` > 0 degrees)
#' * **"moonlight_twilight_nighttime"** plots the illuminance of moonlight plus twilight (no value during daytime, when `sun_altitude` > 0 degrees)
#' * **"twilight"** plots only the illuminance of twilight (defined as the light when `sun_altitude` < 0 degrees).
#' * **"sunlight"** plots only the illuminance of sunlight (defined as the light when `sun_altitude` > 0 degrees).
#' * **"sunlight_twilight"** plots only the sum of sunlight and twilight.
#' * **"total_illuminance_all"** plots all illuminance together, calculated as the sum of moonlight, twilight, and sunlight.
#' Note: The above terms corresponding to the columns headers in the output table.
#' @details
#' # Columns found in the output data.frame/.csv table:
#' * A series of astronomical and illuminance values are reported for every time stamp. In explanation,
#' * **datetime** `POSIXct`. Datetime in `"YYYY-MM-DD hh:mm:ss"`.
#' * **phase_angle** `numeric`. Phase angle of the moon in degree angle.
#' * **fraction** `numeric`. Illuminated fraction of the moon.
#' * **Z_moon** `numeric`. Zenith distance of the moon in degree angle (i.e., the angle of separation from directly overhead). > 90 means the moon is below the horizon.
#' * **distance** `numeric`. The moon-Earth distance in km.
#' * **sun_altitude** `numeric`. The sun altitude (relative to the horizon) in degree angle. Negative value means the sun is below the horizon.
#' * See above **"illuminance_type_plot` options** for explanation of the rest of the columns that report different type of illuminance.
#' # Plotting details:
#' * The magnitude of moonlight and sunlight illuminance differs immensely. So depending on the `illuminance_type_plot` selected, the user might want to manually adjust the plot_y_max.
#' * `plot_dayttime_gray_mask` is `TRUE` by default to mask daytime illuminance in gray. This is particularly useful when plotting `"moon_final_lux"` to make it clear that moonlight during daytime should be ignored.
#' * Nighttime is always shaded in dark gray.
#' * Twilight period is shaded in light gray. The period of twilight can be adjusted with `plot_twilight`.
#' # Attribution
#' * Astronomical values used in calculating ground illuminance are provided by the R package suncalc (Thieurmel & Elmarhraoui, 2022).
#' * Atmospheric pressure at elevation is provided by the R package RPMODEL (Stocker et al., 2020).
#' @keywords moonlight
#' @import magrittr
#' @import dplyr
#' @export
#' @references
#' * Allen, C. W. (1976). Astrophysical quantities. Athelone Press.
#' * Austin, R. H., Phillips, B. F., & Webb, D. J. (1976). A method for calculating moonlight illuminance at the earth’s surface. The Journal of Applied Ecology, 13(3), 741.
#' * Buratti, B. J., Hillier, J. K., & Wang, M. (1996). The lunar opposition surge: Observations by clementine. Icarus, 124(2), 490–499.
#' * Hänel, A., Posch, T., Ribas, S. J., Aubé, M., Duriscoe, D., Jechow, A., Kollath, Z., Lolkema, D. E., Moore, C., Schmidt, N., Spoelstra, H., Wuchterl, G., & Kyba, C. C. M. (2018). Measuring night sky brightness: Methods and challenges. Journal of Quantitative Spectroscopy and Radiative Transfer, 205, 278–290.
#' * Krisciunas, K., & Schaefer, B. E. (1991). A model of the brightness of moonlight. Publications of the Astronomical Society of the Pacific, 103, 1033.
#' * Laue, E. G. (1970). The measurement of solar spectral irradiance at different terrestrial elevations. Solar Energy, 13(1), 43–57.
#' * Schaefer, B. E. (1990). Telescopic limiting magnitudes. Publications of the Astronomical Society of the Pacific, 102, 212.
#' * Seidelmann, P. K., United States Naval Observatory, & Great Britain (Eds.). (1992). Explanatory supplement to the Astronomical almanac (Rev. ed.). University Science Books.
#' * Stocker, B. D., Wang, H., Smith, N. G., Harrison, S. P., Keenan, T. F., Sandoval, D., Davis, T., & Prentice, I. C. (2020). P-model v1.0: An optimality-based light use efficiency model for simulating ecosystem gross primary production. Geoscientific Model Development, 13(3), 1545–1581.
#' * Thieurmel, B., & Elmarhraoui, A. (2022). suncalc: Compute sun position, sunlight phases, moon position and lunar phase. R package version 0.5.1. <https://CRAN.R-project.org/package=suncalc>
#' @examples
#' # Predict the nighttime moonlight illuminance in Leticia, Colombia,
#' # for 14 days starting on 2023-02-27 at 6pm.
#'
#' moonlight_output <- predict_lux(latitude = -4.21528, longitude = -69.94056, site_elev = 0,
#'                     time_zone = "EST", date_start = "2023-02-27", time_start = "18:00:00",
#'                     duration_day = 14, time_interval_minutes = 5, darksky_value = 0.0008,
#'                     illuminance_type_plot = "moon_final_lux_nighttime", output_directory = NULL,
#'                     export_table = FALSE, export_plot = FALSE, plot_width = 11, plot_height = 8.5,
#'                     plot_y_max = "AUTO",  plot_dayttime_gray_mask = TRUE, plot_twilight = "astro")




plot_lux <- function(df = NULL, plot_y_max = "AUTO",  plot_dayttime_gray_mask = TRUE, plot_twilight = "astro",
                     vertical_time_label = FALSE, time_label_interval_hr = 6) {



  #---------------------------PLOTTING

# Create a clean ggplot theme
  theme_rectangular_clean <-
    ggplot2::theme(axis.line = ggplot2::element_line(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank()) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12, colour = 'black'),
                   axis.title = ggplot2::element_text(size = 12, colour = 'black')) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0.25,0.25,0.25,0.25),"cm"))

  # Specify the time of night period and twilight period for shading
  night_time <- dplyr::filter(df, sun_altitude < 0) %>% dplyr::select(datetime)
  night_time <- lubridate::as_datetime(night_time$datetime, tz = time_zone)

  # Define twilight period
  if(plot_twilight == "astro"){twilight_angle <- -18}
  if(plot_twilight == "nautic"){twilight_angle <- -12}
  if(plot_twilight == "civil"){twilight_angle <- -6}
  if(plot_twilight == "none"){twilight_angle <- 0}

  after_twilight_time <- dplyr::filter(df, sun_altitude < (twilight_angle)) %>% dplyr::select(datetime)
  after_twilight_time <- lubridate::as_datetime(after_twilight_time$datetime, tz =  time_zone)

  day_time <- dplyr::filter(df, sun_altitude > 0) %>% dplyr::select(datetime)
  day_time <- lubridate::as_datetime(day_time$datetime, tz = time_zone)


# Plotting:
  if(plot_y_max == "AUTO"){
  plot_output <- ggplot2::ggplot() + theme_rectangular_clean +
    ggplot2::geom_rect(ggplot2::aes(xmin = night_time, # night period
                           xmax = night_time + lubridate::dminutes(time_interval_minutes),
                           ymin = 0, ymax = Inf), fill = "grey88", alpha = 1, na.rm = TRUE) +
    ggplot2::geom_rect(ggplot2::aes(xmin = after_twilight_time, # after twilight
                           xmax = after_twilight_time + lubridate::dminutes(time_interval_minutes),
                           ymin = 0, ymax = Inf), fill = "grey80", alpha = 1, na.rm = TRUE) +
    ggplot2::geom_line(data = df, ggplot2::aes(x = datetime, y = eval(as.symbol(illuminance_type_plot))), colour = 'black', linewidth = 0.75) +
    ggplot2::scale_x_datetime(date_breaks = paste0(time_label_interval_hr/24, " day")) + # change the date break here for different x-axis label. A more specific format can be specified.
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) + # rotate date label 90 degree.
    ggplot2::labs(x = "", y = "predicted ground illuminance (lx)") } else
      {plot_output <- ggplot2::ggplot() + theme_rectangular_clean +
        ggplot2::geom_rect(ggplot2::aes(xmin = night_time, # night period
                                        xmax = night_time + lubridate::dminutes(time_interval_minutes),
                                        ymin = 0, ymax = Inf), fill = "grey88", alpha = 1, na.rm = TRUE) +
        ggplot2::geom_rect(ggplot2::aes(xmin = after_twilight_time, # after twilight
                                        xmax = after_twilight_time + lubridate::dminutes(time_interval_minutes),
                                        ymin = 0, ymax = Inf), fill = "grey80", alpha = 1, na.rm = TRUE) +
        ggplot2::geom_line(data = df, ggplot2::aes(x = datetime, y = eval(as.symbol(illuminance_type_plot))), colour = 'black', linewidth = 0.75) +
        ggplot2::scale_y_continuous(limits = c(0, plot_y_max), # change the y-axis range here
                                    breaks = c(round(seq(from = 0, to = plot_y_max, length.out = 4), digits = 3))) + # change the y-axis breaks here
        ggplot2::scale_x_datetime(date_breaks = "1 day") + # change the date break here for different x-axis label. A more specific format can be specified.
        ggplot2::labs(x = "", y = "predicted ground illuminance (lx)") }


  if(plot_dayttime_gray_mask == TRUE){
    plot_output <- plot_output +
                    ggplot2::geom_rect(ggplot2::aes(xmin = day_time, # day time mask moonlight regression to gray color
                                      xmax = day_time + lubridate::dminutes(time_interval_minutes),
                                      ymin = 0, ymax = Inf), fill = "white", alpha = 0.85, na.rm = TRUE)}

  if(vertical_time_label == TRUE){
    plot_output <- plot_output +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))} # rotate date label 90 degree.

  print(plot_output)

}

  #---------------------------END OF SCRIPT---------------------------
