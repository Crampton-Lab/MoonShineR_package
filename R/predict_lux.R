#' A function to predict ground illuminance
#'
#' * This function predicts moonlight, sunlight, and/or twilight ground illumination in lux for any defined geographical location and time period. It creates a data.frame output and automatically plot to the console. Automatic export of the table (.csv) and plot (.pdf) is optional.
#' * To learn more about MoonShine, see instruction manual: <https://lokpoon.github.io/moonshine_manual/overview.html>
#' @param latitude `numeric`. Latitude in decimal degrees (e.g., -4.21528).
#' @param longitude `numeric`. Longitude in decimal degrees (e.g., -69.94056).
#' @param site_elev `numeric`. Site elevation in meters (e.g., 0 = sea level). Default = 0. Elevation correction only applies to moonlight but not sunlight and twilight. Site elevation of a coordinate location can be obtained from <https://www.dcode.fr/earth-elevation>.
#' @param time_zone `character`. Time zone for the location set (e.g., “EST”). Remember to change time_zone to corrospond it to the location set. For a list of time zone names, enter OlsonNames(tzdir = NULL) in R console. Use a time zone without DST (e.g., use "EST" instead of "America/New_York").
#' @param date_start `character`. Starting date of the simulation ("YYYY-MM-DD").
#' @param time_start `character`. Starting time of the simulation ("hh:mm:ss"). Default = "00:00:00".
#' @param duration_day `numeric`. Duration of the simulation in days.
#' @param time_interval_minutes `numeric`.The temporal resolution of the simulation in minutes. E.g., 5 = calculates the illuminance every 5 minutes. Using small time interval requires longer computation time.
#' @param darksky_value `numeric`. A baseline illumination added to the model to represent other constant nocturnal light sources (e.g., starlight and airglow). Default = 0.0008. Change it to zero if a completely dark sky is preferred.
#' @param illuminance_type_plot `character`. Choose one type of illuminance to plot. See options in the section below. Default = "moon_final_lux_nighttime".
#' @param output_directory `character`. Directory to save the output table (.csv) and plot (.pdf). NULL if saving is turned OFF (i.e., save_table = FALSE and save_plot = FALSE).
#' @param save_table `logical`. TRUE to export output .csv table to the output_directory. FALSE to disable. Default = FALSE.
#' @param save_plot `logical`. TRUE to export output .pdf plot to the output_directory. FALSE to disable. Default = FALSE.
#' @param plot_width `numeric`. The exported .pdf plot width in inch. Default = 11.
#' @param plot_height `numeric`. The exported .pdf plot height in inch. Default = 8.5.
#' @param plot_y_max `character` "AUTO", or `numeric`. Let the plot y-axis scale automatically or manually set a y-axis upper limit. Affects both the plot in the plot window and the exported .pdf. Default = "AUTO".
#' @param plot_dayttime_gray_mask 'logical`. TRUE to mask daytime plot line in gray. Affects both the plot in the plot window and the exported .pdf. FALSE to disable (plot line always black).
#' @details
#' # illuminance_type_plot options:
#' Note: these terms corresponding to the columns headers in the output table:
#' * **"moon_final_lux"** plots only the illuminance of moonlight (plus the darksky_value) during both day and night.
#' * **"moon_final_lux_nighttime"** plots only the illuminance of moonlight at night (no value during daytime, when sun altitude > 0 degrees)
#' * **"moonlight_twilight_nighttime"** plots the illuminance of moonlight plus twilight (no value during daytime, when sun altitude > 0 degrees)
#' * **"twilight"** plots only the illuminance of twilight (defined as the light when sun altitude < 0 degrees).
#' * **"sunlight"** plots only the illuminance of sunlight (defined as the light when sun altitude > 0 degrees).
#' * **"sunlight_twilight"** plots only the sum of sunlight and twilight
#' * **"total_illuminance_all"** plots all illuminance together, calculated as the sum of moonlight, twilight, and sunlight.
#' @details
#' # Columns in the output data.frame/.csv table
#' * **datetime** `POSIXct`. Datetime in "YYYY-MM-DD hh:mm:ss".
#' * **phase_angle** `numeric` phase angle of the moon in degree angle.
#' @keywords moonlight
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' moonlight_output <- predict_lux(latitude = -4.21528, longitude = -69.94056, site_elev = 0, time_zone = "EST",
#'                     date_start = "2023-02-27", time_start = "18:00:00", duration_day = 14, time_interval_minutes = 5,
#'                     darksky_value = 0.0008, illuminance_type_plot = "moon_final_lux_nighttime",
#'                     output_directory = NULL, save_table = FALSE, save_plot = FALSE,
#'                     plot_width = 11, plot_height = 8.5, plot_y_max = "AUTO",  plot_dayttime_gray_mask = TRUE)


# $ datetime                    : POSIXct, format: "2023-02-27 18:00:00" "2023-02-27 18:05:00" "2023-02-27 18:10:00" "2023-02-27 18:15:00" ...
# $ phase_angle                 : num  83.1 83 83 83 82.9 ...
# $ fraction                    : num  0.56 0.561 0.561 0.561 0.562 ...
# $ Z_moon                      : num  29.7 29.7 29.7 29.7 29.8 ...
# $ distance                    : num  396890 396904 396918 396931 396945 ...
# $ sun_altitude                : num  -1.13 -2.36 -3.59 -4.83 -6.06 ...
# $ moon_final_lux              : num  0.0252 0.0253 0.0253 0.0253 0.0253 ...
# $ moon_final_lux_nighttime    : num  0.0252 0.0253 0.0253 0.0253 0.0253 ...
# $ twilight                    : num  369.61 139.82 44.87 12.59 3.19 ...
# $ sunlight                    : num  0 0 0 0 0 0 0 0 0 0 ...
# $ total_illuminance_all       : num  369.64 139.84 44.89 12.62 3.21 ...
# $ sunlight_twilight           : num  369.61 139.82 44.87 12.59 3.19 ...
# $ moonlight_twilight_nighttime: num  369.64 139.84 44.89 12.62 3.21 ...

predict_lux <- function(latitude = NULL, longitude = NULL, site_elev = 0, time_zone = NULL, date_start = NULL, time_start = "00:00:00",
                        duration_day = NULL, time_interval_minutes = 5, darksky_value = 0.0008, illuminance_type_plot = "moon_final_lux_nighttime",
                        output_directory = NULL, save_table = FALSE, save_plot = FALSE,
                        plot_width = 11, plot_height = 8.5, plot_y_max = "AUTO",  plot_dayttime_gray_mask = TRUE) {

  #---------------------------START OF ILLUMINATION COMPUTATION-------------------

  # Start time formatting
  date_time_start <- lubridate::as_datetime(paste(date_start, time_start, sep = " ", collapse = NULL), tz = time_zone)
  number_of_interval <- as.numeric(lubridate::ddays(duration_day)) / as.numeric(lubridate::dminutes(time_interval_minutes))

  # Create an empty dataframe, to be filled during for loop
  moon_value_table <- data.frame(matrix(ncol = 1, nrow = number_of_interval))
  x <- c("x")
  colnames(moon_value_table) <- x

  # Create for loop [i] time interval list
  time_interval_list <- seq(1, number_of_interval, by = 1)

  # Create a progress bar object
  #pb <- progress_bar$new(total = length(time_interval_list))

  # Fill in empty data frame with suncalc data
  for (i in time_interval_list) {
    moon_value_table[i, "datetime"] <- suncalc::getMoonIllumination(date = date_time_start + (i - 1) * time_interval_minutes * 60)[1, "date"]
    moon_value_table[i, "phase_angle"] <- (suncalc::getMoonIllumination(date = date_time_start + (i - 1) * time_interval_minutes * 60)[1, "phase"]) * (-360) + 180 # phase_angle = angular separation of the sun and Earth, as seen on the moon
    moon_value_table[i, "fraction"] <- (suncalc::getMoonIllumination(date = date_time_start + (i - 1) * time_interval_minutes*60)[1, "fraction"]) # Moon illuminated fraction, not used in calculation. This is calculated only for user to learn about the moon phase.
    moon_value_table[i, "Z_moon"] <- 90 - (REdaS::rad2deg(suncalc::getMoonPosition(date = date_time_start + (i - 1) * time_interval_minutes * 60, lat = latitude, lon = longitude)[1, "altitude"])) # Z_moon = Zenith distance in degree of the moon = 90 - moon altitude
    moon_value_table[i, "distance"] <- suncalc::getMoonPosition(date = date_time_start + (i - 1) * time_interval_minutes * 60, lat = latitude, lon = longitude)[1, "distance"] # distance = moon/Earth distance in km
    moon_value_table[i, "sun_altitude"] <- REdaS::rad2deg(suncalc::getSunlightPosition(date = date_time_start + (i - 1) * time_interval_minutes * 60, lat = latitude, lon = longitude, keep = c("altitude"))[1, "altitude"]) # sun_altitude = altitude of the sun in degree
    #pb$tick() # Update the progress bar
  }
  moon_value_table <- subset(moon_value_table, select = -c(x))

  # Replace moon altitude that are lower than horizon_elev with NA (moon below horizon)
  moon_value_table <- transform(moon_value_table, Z_moon = ifelse(Z_moon > 90, NA, Z_moon))

  # Calculate atmospheric extinction
  moon_value_table$atm_ext <- (-0.140194 * moon_value_table$Z_moon) / (-91.674385 + moon_value_table$Z_moon) - 0.03 # A Michaelis-Menten that fits Table 2 in Austin et al. (1976)
  moon_value_table$atm_ext <- ifelse(moon_value_table$atm_ext < 0, 0, moon_value_table$atm_ext)

  # Moon magnitude calculated from phase angle (unit in relative magnitude) (see Allen 1976, p. 144)
  moon_value_table$m <- (-12.73) + 0.026 * abs(moon_value_table$phase_angle) + 4 * (10 ^ -9) * (abs(moon_value_table$phase_angle) ^ 4)

  # Initial illuminance of moonlight (see eq. (16) of Schaefer 1990a)
  illuminance_temp <- 10 ^ ((-0.4) * (moon_value_table$m + moon_value_table$atm_ext + 16.57)) # In unit foot candle
  moon_value_table$illuminance_temp_lux <- illuminance_temp * 10.7639 # Convert to lux

  # Correct for the effect of site elevation
  atm_pressure_relative_to_sea <- rpmodel::patm(site_elev, patm0 = 101325) / 101325 # The atmospheric pressure at the site elevation relative to sea level
  sea_555nm <- 18.964 * exp(-0.229 * 1) # Sea level irradiance at 555nm. Function extracted from figure 1 of Laue (1970)
  elevated_555nm <- 18.964 * exp(-0.229 * atm_pressure_relative_to_sea) # Sea level irradiance at 555nm. Function extracted from figure 1 of Laue (1970)
  increase_factor_elev <- elevated_555nm / sea_555nm # relative increase in illuminance due to elevation
  moon_value_table$illuminance_temp_lux <- moon_value_table$illuminance_temp_lux * increase_factor_elev

  # Apply lunar opposition surge when p < 6 (Buratti et al. 1996, figure 5)
  moon_value_table <- transform(moon_value_table, illuminance_temp_lux = ifelse(abs(phase_angle) < 6, illuminance_temp_lux + ((0.4 * (6 - abs(phase_angle)) / 6) * illuminance_temp_lux), illuminance_temp_lux))
  ## A linear increase in moon brightness as phase angle decreases below 6

  # Apply spreading out effect (angle of incidence) of light when illuminating at an angle (Austin et al. 1976)
  moon_value_table$moon_final_lux <- moon_value_table$illuminance_temp_lux * sin(REdaS::deg2rad(90 - moon_value_table$Z_moon))

  # The moon/Earth distance effect (inverse square law) (Austin et al. 1976)
  moon_value_table$moon_final_lux <- moon_value_table$moon_final_lux * (1 / ((moon_value_table$distance / 384400) ^ 2))

  # Waxing Waning asymmetric effect (simplified function derived from Austin et al. 1976 Table 1)
  moon_value_table$moon_final_lux <- ifelse(moon_value_table$phase_angle < 0,
                                            moon_value_table$moon_final_lux * (-0.00026 * moon_value_table$phase_angle + 1),
                                            moon_value_table$moon_final_lux)

  # Final moonlight illuminance (with a slight adjustment factor, calibrated from Leticia, Colombia, Aug 11, 2022 field full moon measurement)
  moon_value_table$moon_final_lux <- moon_value_table$moon_final_lux * 0.863 + darksky_value
  moon_value_table$moon_final_lux <- replace(moon_value_table$moon_final_lux, is.na(moon_value_table$moon_final_lux), darksky_value) # Replace NA with darksky_value

  moon_value_table <- transform(moon_value_table, moon_final_lux_nighttime = ifelse(sun_altitude > 0, NA, moon_final_lux)) # Create moon_final_lux_nighttime column

  #---------------------------TWILIGHT CALCULATION---------------------------

  # Calculation of sunlight and twilight follows Seidelmann (1992)
  moon_value_table$twilight <- 0
  moon_value_table$twilight <- ifelse(moon_value_table$sun_altitude < 0 & moon_value_table$sun_altitude > -0.8,
                                      10 ^ (2.88 + (22.26 * (moon_value_table$sun_altitude / 90)) - 207.64 * ((moon_value_table$sun_altitude / 90) ^ 2) + 1034.3 * ((moon_value_table$sun_altitude / 90) ^ 3)), moon_value_table$twilight)
  moon_value_table$twilight <- ifelse(moon_value_table$sun_altitude < -0.8 & moon_value_table$sun_altitude > -5,
                                      10 ^ (2.88 + (21.81 * (moon_value_table$sun_altitude / 90)) - 258.11 * ((moon_value_table$sun_altitude / 90) ^ 2) - 858.36 * ((moon_value_table$sun_altitude / 90) ^ 3)), moon_value_table$twilight)
  moon_value_table$twilight <- ifelse(moon_value_table$sun_altitude < -5 & moon_value_table$sun_altitude > -12,
                                      10 ^ (2.7 + (12.17 * (moon_value_table$sun_altitude / 90)) - 431.69 * ((moon_value_table$sun_altitude / 90) ^ 2) - 1899.83 * ((moon_value_table$sun_altitude / 90) ^ 3)), moon_value_table$twilight)
  moon_value_table$twilight <- ifelse(moon_value_table$sun_altitude < -12 & moon_value_table$sun_altitude > -18,
                                      10 ^ (13.84 + (262.72 * (moon_value_table$sun_altitude / 90)) + 1447.42 * ((moon_value_table$sun_altitude / 90) ^ 2) + 2797.93 * ((moon_value_table$sun_altitude / 90) ^ 3)), moon_value_table$twilight)

  #---------------------------SUNLIGHT (SUN > 0 DEGREE) CALCULATION---------------------------

  moon_value_table$sunlight <- 0
  moon_value_table$sunlight <- ifelse(moon_value_table$sun_altitude > 20,
                                      10 ^ (3.74 + (3.97 * (moon_value_table$sun_altitude / 90)) - 4.07 * ((moon_value_table$sun_altitude / 90) ^ 2) + 1.47 * ((moon_value_table$sun_altitude / 90) ^ 3)), moon_value_table$sunlight)
  moon_value_table$sunlight <- ifelse(moon_value_table$sun_altitude < 20 & moon_value_table$sun_altitude > 5,
                                      10 ^ (3.05 + (13.28 * (moon_value_table$sun_altitude / 90)) - 45.98 * ((moon_value_table$sun_altitude / 90) ^ 2) + 64.33 * ((moon_value_table$sun_altitude / 90) ^ 3)), moon_value_table$sunlight)
  moon_value_table$sunlight <- ifelse(moon_value_table$sun_altitude < 5 & moon_value_table$sun_altitude > 0,
                                      10 ^ (2.88 + (22.26 * (moon_value_table$sun_altitude / 90)) - 207.64 * ((moon_value_table$sun_altitude / 90) ^ 2) + 1034.3 * ((moon_value_table$sun_altitude / 90) ^ 3)), moon_value_table$sunlight)

  #---------------------------ADD MOON, SUNLIGHT AND TWILIGHT TOGETHER---------------------------

  moon_value_table$total_illuminance_all <- moon_value_table$moon_final_lux + moon_value_table$twilight + moon_value_table$sunlight
  moon_value_table$sunlight_twilight <- moon_value_table$twilight + moon_value_table$sunlight
  moon_value_table$moonlight_twilight_nighttime <- moon_value_table$twilight + moon_value_table$moon_final_lux_nighttime

  #---------------------------REMOVE UNNECESSARY COLUMNS---------------------------

  moon_value_table <- subset(moon_value_table, select = -c(atm_ext, m, illuminance_temp_lux))

  #---------------------------END OF ILLUMINATION COMPUTATION--------------------

# Save moon table csv file
  if (save_table) {
    utils::write.csv(moon_value_table, paste0(output_directory, "/", "lux_calculator_output.csv"), row.names = TRUE)
  }

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
  night_time <- dplyr::filter(moon_value_table, sun_altitude < 0) %>% dplyr::select(datetime)
  night_time <- lubridate::as_datetime(night_time$datetime, tz = time_zone)

  after_twilight_time <- dplyr::filter(moon_value_table, sun_altitude < (-18)) %>% dplyr::select(datetime)
  after_twilight_time <- lubridate::as_datetime(after_twilight_time$datetime, tz =  time_zone)

  day_time <- dplyr::filter(moon_value_table, sun_altitude > 0) %>% dplyr::select(datetime)
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
    ggplot2::geom_line(data = moon_value_table, ggplot2::aes(x = datetime, y = eval(as.symbol(illuminance_type_plot))), colour = 'black', linewidth = 0.75) +
    ggplot2::scale_x_datetime(date_breaks = "1 day") + # change the date break here for different x-axis label. A more specific format can be specified.
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) + # rotate date label 90 degree.
    ggplot2::labs(x = "", y = "predicted ground illuminance (lx)") } else
      {plot_output <- ggplot2::ggplot() + theme_rectangular_clean +
        ggplot2::geom_rect(ggplot2::aes(xmin = night_time, # night period
                                        xmax = night_time + lubridate::dminutes(time_interval_minutes),
                                        ymin = 0, ymax = Inf), fill = "grey88", alpha = 1, na.rm = TRUE) +
        ggplot2::geom_rect(ggplot2::aes(xmin = after_twilight_time, # after twilight
                                        xmax = after_twilight_time + lubridate::dminutes(time_interval_minutes),
                                        ymin = 0, ymax = Inf), fill = "grey80", alpha = 1, na.rm = TRUE) +
        ggplot2::geom_line(data = moon_value_table, ggplot2::aes(x = datetime, y = eval(as.symbol(illuminance_type_plot))), colour = 'black', linewidth = 0.75) +
        ggplot2::scale_y_continuous(limits = c(0, plot_y_max), # change the y-axis range here
                                    breaks = c(round(seq(from = 0, to = plot_y_max, length.out = 4), digits = 3))) + # change the y-axis breaks here
        ggplot2::scale_x_datetime(date_breaks = "1 day") + # change the date break here for different x-axis label. A more specific format can be specified.
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) + # rotate date label 90 degree.
        ggplot2::labs(x = "", y = "predicted ground illuminance (lx)") }


  if(plot_dayttime_gray_mask == TRUE){
    plot_output <- plot_output +
                    ggplot2::geom_rect(ggplot2::aes(xmin = day_time, # day time mask moonlight regression to gray color
                                      xmax = day_time + lubridate::dminutes(time_interval_minutes),
                                      ymin = 0, ymax = Inf), fill = "white", alpha = 0.85, na.rm = TRUE)}

# Save plot:
    if (save_plot) {
      ggplot2::ggsave(plot = plot_output, paste0(output_directory, "/", "lux_calculator_output_plot.pdf"), width = plot_width, height = plot_height)
    }

  print(plot_output)

  print("The calculation is completed. The .csv table and .pdf plot (if required) are saved to the specified output_directory.")

  #---------------------------Lunar eclipse warning---------------------------

  # MoonShineR warns the user if an eclipse occurs during a simulation, and it reports the start and end time of the simulation.
  # However, MoonShineR does not simulate the reduction in moon ground illuminance associated with the eclipse.

  if (any(abs(moon_value_table$phase_angle) < 1.5 & moon_value_table$sun_altitude < 0)) { # eclipse defined as a moon with phase angle < 1.5 during nighttime
    print("ECLIPSE IN SIMULATION!!!")
    eclipse_list <- (abs(moon_value_table$phase_angle) < 1.5 & moon_value_table$sun_altitude < 0)
    moon_value_table[which(eclipse_list == TRUE),]
  } else {
    print("no eclipse in simulation")
  }

  # Return table
  return(moon_value_table)

  #---------------------------END OF SCRIPT---------------------------
}
