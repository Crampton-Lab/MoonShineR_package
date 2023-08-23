#' Predict moonlight, sunlight, and twilight ground illuminance
#'
#' * predict_lux() predicts moonlight, sunlight, and/or twilight ground illumination (in lux) for any defined geographical location and time period. It creates a data.frame output and automatically plot to the console. Automatic export of the table (.csv) is optional.
#' * The presence of lunar eclipse during the simulated period is also reported as a console message. The illuminance reduction during lunar eclipse is not modeled.
#' * To create a moonlight or sunlight LED schedule for light re-creation using MoonShineP, user need to download the R script version of the MoonShineR: Moonlight scheduler or MoonShineR: Sunlight/twilight scheduler from GitHub repository: <https://github.com/Crampton-Lab/MoonShine>
#' * The R script version also have other simulation features, such as simulating the blocking of direct moonlight from horizon obstructions (e.g., tree line or ridgeline), and attenuation by random passing clouds.
#' * To learn more about MoonShineR, see instruction manual: <https://lokpoon.github.io/moonshine_manual/overview.html>
#' @param latitude `numeric`. Latitude in decimal degrees (e.g., `-4.21528`).
#' @param longitude `numeric`. Longitude in decimal degrees (e.g., `-69.94056`).
#' @param site_elev `numeric`. Site elevation in meters (e.g., `0` is sea level). Default is `0`. Elevation correction only applies to moonlight but not sunlight and twilight. Site elevation of a coordinate location can be obtained from <https://www.dcode.fr/earth-elevation>.
#' @param time_zone `character`. Time zone for the location set (e.g., `“EST”`). Remember to change time_zone to correspond it to the location set. For a list of time zone names, enter `OlsonNames(tzdir = NULL)` in R console. Use a time zone without DST to avoid confusion (e.g., use `"EST"` instead of `"America/New_York"`).
#' @param date_start `character`. Starting date of the simulation (`"YYYY-MM-DD"`).
#' @param time_start `character`. Starting time of the simulation (`"hh:mm:ss"`). Default is `"00:00:00"`.
#' @param duration_day `numeric`. Duration of the simulation in days.
#' @param time_interval_minutes `numeric`.The temporal resolution of the simulation in minutes. E.g., `5` calculates the illuminance every 5 minutes. Using small time interval requires longer computation time. Default is `5`.
#' @param darksky_value `numeric`. A baseline illuminance (in lux) added to the model to represent other constant nocturnal light sources (e.g., starlight and airglow). Default is `0.0008`. Change it to zero if a completely dark sky is preferred.
#' @param output_directory `character`. Directory to save the output table (.csv) and plot (.pdf). Ignore output_directory if the two export options are turned OFF (i.e., `export_table = FALSE` and `export_plot = FALSE`).
#' @param export_table `logical`. `TRUE` to export output .csv table to the output_directory. `FALSE` to disable. Default is `FALSE`.
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
#'                     output_directory = NULL, export_table = FALSE)




predict_lux <- function(latitude = NULL, longitude = NULL, site_elev = 0, time_zone = NULL, date_start = NULL, time_start = "00:00:00",
                        duration_day = NULL, time_interval_minutes = 5, darksky_value = 0.0008,
                        output_directory = NULL, export_table = FALSE, export_plot = FALSE) {

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
  if (export_table) {
    utils::write.csv(moon_value_table, paste0(output_directory, "/", "lux_calculator_output.csv"), row.names = TRUE)
  }


  #---------------------------Lunar eclipse warning---------------------------

  # MoonShineR warns the user if an eclipse occurs during a simulation, and it reports the start and end time of the simulation.
  # However, MoonShineR does not simulate the reduction in moon ground illuminance associated with the eclipse.

  if (any(abs(moon_value_table$phase_angle) < 1.5 & moon_value_table$sun_altitude < 0)) { # eclipse defined as a moon with phase angle < 1.5 during nighttime
    print("The calculation is completed. ECLIPSE IN SIMULATION!!!")
    eclipse_list <- (abs(moon_value_table$phase_angle) < 1.5 & moon_value_table$sun_altitude < 0)
    moon_value_table[which(eclipse_list == TRUE),]
  } else {
    print("The calculation is completed. No eclipse in simulation")
  }

  # Return table
  return(moon_value_table)

  #---------------------------END OF SCRIPT---------------------------
}
