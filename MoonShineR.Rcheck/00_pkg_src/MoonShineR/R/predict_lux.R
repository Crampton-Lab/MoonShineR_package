#' Predict moonlight, sunlight, and twilight ground illuminance
#'
#' * predict_lux() predicts ground illuminance from moonlight, sunlight, and/or twilight (in lux) for a specified geographical location and time period. It returns a data frame and automatically displays a plot of the predictions. Optional export of the data frame as a CSV file is available.
#' * The occurrence of lunar eclipses during the simulated period is reported in a console message. The reduction in illuminance during lunar eclipses is not modeled.
#' * To generate LED control schedules for use with MoonShineP, download the stand-alone MoonShineR Moonlight scheduler or Sunlight/twilight scheduler scripts from the main MoonShine GitHub repository: <https://github.com/Crampton-Lab/MoonShine>. These scripts are not included in the CRAN package.
#' * The stand-alone scheduler scripts also provide additional simulation features, including the obstruction of direct moonlight by trees or ridgelines and attenuation by passing clouds.
#' * For further documentation on MoonShineR, the scheduler scripts, and MoonShineP, see <https://lokpoon.github.io/MoonShine_manual/overview.html>.
#' @param latitude `numeric`. Latitude in decimal degrees (e.g., `-4.21528`).
#' @param longitude `numeric`. Longitude in decimal degrees (e.g., `-69.94056`).
#' @param site_elev `numeric`. Site elevation in meters above sea level (default `0`). The elevation correction applies to moonlight predictions but not to sunlight or twilight predictions. Elevation for a given location can be obtained from <https://www.dcode.fr/earth-elevation>.
#' @param time_zone `character`. Time zone for the specified location (e.g., `"EST"` for a fixed UTC-5 offset). Set time_zone consistently with the date and time being simulated. To list recognized time zones in R, enter OlsonNames(). A fixed-offset zone avoids daylight saving time changes, whereas a location-based zone such as `"America/New_York"` applies them; choose according to the intended interpretation of local time.
#' @param date_start `character`. Starting date of the simulation (`"YYYY-MM-DD"`).
#' @param time_start `character`. Starting time of the simulation (`"hh:mm:ss"`). Default is `"00:00:00"`.
#' @param duration_day `numeric`. Duration of the simulation in days.
#' @param time_interval_minutes `numeric` Time interval between successive illuminance predictions, in minutes. For example, 5 calculates illuminance every 5 minutes. Smaller intervals require more computation time. Default is `5`.
#' @param darksky_value `numeric`. A baseline illuminance (in lux) added to the model to represent other approximately constant nocturnal light sources (e.g., starlight and airglow). Default is `0.0008`. Set it to zero to omit this background component.
#' @param output_directory `character`. Directory for exported output files. If export_table = `FALSE`, no CSV table is exported.
#' @param export_table `logical`. If `TRUE`, export the output data frame as a CSV file to output_directory; if `FALSE`, do not export the table. Default is `FALSE`.
#' @param show_progress `logical`. If `TRUE`, display a progress bar; `FALSE` disables it. Default is `TRUE`.
#' @details
#' # Columns found in the output data.frame/.csv table:
#' * The output reports astronomical quantities and predicted illuminance for each time step.
#' * **datetime** `POSIXct`. Date and time in `"YYYY-MM-DD hh:mm:ss"`.
#' * **phase_angle** `numeric`. Lunar phase angle, in degrees.
#' * **fraction** `numeric`. Illuminated fraction of the moon.
#' * **Z_moon** `numeric`. Lunar zenith distance, in degrees (angular distance from directly overhead). Values greater than 90 indicate that the moon is below the horizon.
#' * **distance** `numeric`. Earth–moon distance, in km.
#' * **sun_altitude** `numeric`. Altitude of the sun above the horizon, in degrees. Negative values indicate that the sun is below the horizon.
#' * **"moon_final_lux"** `numeric`. Predicted moonlight illuminance plus darksky_value, for both daytime and nighttime.
#' * **"moon_final_lux_nighttime"** `numeric`. Predicted moonlight illuminance at night; no value is reported during daytime (`sun_altitude` > 0).
#' * **"moonlight_twilight_nighttime"** `numeric`. Sum of moonlight and twilight illuminance at night; no value is reported during daytime (`sun_altitude` > 0).
#' * **"twilight"** `numeric`. Twilight illuminance (`sun_altitude` < 0).
#' * **"sunlight"** `numeric`. Sunlight illuminance (`sun_altitude` > 0).
#' * **"sunlight_twilight"** `numeric`. Sum of sunlight and twilight illuminance.
#' * **"total_illuminance_all"** `numeric`. Sum of moonlight, twilight, and sunlight illuminance.
#' * **"eclipse"** `logical`. Indicator for a lunar eclipse, including penumbral eclipses.
#' # Attribution
#' * Astronomical quantities used in calculating ground illuminance are provided by the R package suncalc (Thieurmel & Elmarhraoui, 2022).
#' * Atmospheric pressure at elevation is provided by the R package RPMODEL (Stocker et al., 2020).
#' @keywords moonlight
#' @import magrittr
#' @export
#' @references
#' * Allen, C. W. (1976). Astrophysical quantities (3rd ed.). Athlone Press.
#' * Austin, R. H., Phillips, B. F., & Webb, D. J. (1976). A method for calculating moonlight illuminance at the Earth's surface. The Journal of Applied Ecology, 13(3), 741–748.
#' * Buratti, B. J., Hillier, J. K., & Wang, M. (1996). The lunar opposition surge: Observations by Clementine. Icarus, 124(2), 490-499.
#' * Hanel, A., Posch, T., Ribas, S. J., Aube, M., Duriscoe, D., Jechow, A., Kollath, Z., Lolkema, D. E., Moore, C., Schmidt, N., Spoelstra, H., Wuchterl, G., & Kyba, C. C. M. (2018). Measuring night sky brightness: Methods and challenges. Journal of Quantitative Spectroscopy and Radiative Transfer, 205, 278-290.
#' * Krisciunas, K., & Schaefer, B. E. (1991). A model of the brightness of moonlight. Publications of the Astronomical Society of the Pacific, 103, 1033-1039.
#' * Laue, E. G. (1970). The measurement of solar spectral irradiance at different terrestrial elevations. Solar Energy, 13(1), 43-57.
#' * Poon, L., Jenks, I. T., & Crampton, W. G. R. (2024). MoonShine: A software-hardware system for simulating moonlight ground illuminance and re-creating artificial moonlight cycles in a laboratory environment. Methods in Ecology and Evolution, 15, 701–715. \doi{10.1111/2041-210X.14299}
#' * Schaefer, B. E. (1990). Telescopic limiting magnitudes. Publications of the Astronomical Society of the Pacific, 102, 212.
#' * Seidelmann, P. K., United States Naval Observatory, & Great Britain (Eds.). (1992). Explanatory supplement to the Astronomical almanac (Rev. ed.). University Science Books.
#' * Stocker, B. D., Wang, H., Smith, N. G., Harrison, S. P., Keenan, T. F., Sandoval, D., Davis, T., & Prentice, I. C. (2020). P-model v1.0: An optimality-based light use efficiency model for simulating ecosystem gross primary production. Geoscientific Model Development, 13(3), 1545-1581.
#' * Thieurmel, B., & Elmarhraoui, A. (2022). suncalc: Compute sun position, sunlight phases, moon position and lunar phase. R package version 0.5.1. <https://CRAN.R-project.org/package=suncalc>
#' @return A data frame containing predicted ground illuminance values for each time step, based on different combinations of moonlight, sunlight, and twilight.
#'   Other columns include various astronomical values.
#' @examples
#' # Predict the nighttime moonlight illuminance in Leticia, Colombia,
#' # for 10.5 days starting on 2023-02-27 at 6 pm (UTC -5).
#'
#' moonlight_output <- predict_lux(latitude = -4.21528, longitude = -69.94056, site_elev = 0,
#'                     time_zone = "EST", date_start = "2023-02-27", time_start = "18:00:00",
#'                     duration_day = 10.5, time_interval_minutes = 5, darksky_value = 0.0008,
#'                     output_directory = NULL, export_table = FALSE, show_progress = TRUE)
#'
#' moonlight_output # Display the completed data frame.




predict_lux <- function(latitude = NULL, longitude = NULL, site_elev = 0, time_zone = NULL, date_start = NULL, time_start = "00:00:00",
                        duration_day = NULL, time_interval_minutes = 5, darksky_value = 0.0008,
                        output_directory = NULL, export_table = FALSE, show_progress = TRUE) {



# Error messages for the arguments

  if (missing(latitude)) {
    stop("Argument 'latitude' is missing! See ??MoonShineR::predict_lux")
  }

  if (missing(longitude)) {
    stop("Argument 'longitude' is missing! See ??MoonShineR::predict_lux")
  }

  if (missing(time_zone)) {
    stop("Argument 'time_zone' is missing! For a list of time zone names, enter OlsonNames(tzdir = NULL) in R console. See ??MoonShineR::predict_lux")
  }

  if (!is.character(time_start)) {
    stop("Argument 'time_start' needs to be type character! See ??MoonShineR::predict_lux")
  }

  if (missing(date_start)) {
    stop("Argument 'date_start' is missing! See ??MoonShineR::predict_lux")
  }

  if (!is.character(date_start)) {
    stop("Argument 'date_start' needs to be type character! See ??MoonShineR::predict_lux")
  }

  if (missing(duration_day)) {
    stop("Argument 'duration_day' is missing! See ??MoonShineR::predict_lux")
  }

  if (missing(duration_day)) {
    stop("Argument 'duration_day' is missing! See ??MoonShineR::predict_lux")
  }
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
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent",
    total = length(time_interval_list),
    clear = FALSE,
    width = 100
  )

  # Fill in empty data frame with suncalc data
  for (i in time_interval_list) {
    moon_value_table[i, "datetime"] <- suncalc::getMoonIllumination(date = date_time_start + (i - 1) * time_interval_minutes * 60)[1, "date"]
    moon_value_table[i, "phase_angle"] <- (suncalc::getMoonIllumination(date = date_time_start + (i - 1) * time_interval_minutes * 60)[1, "phase"]) * (-360) + 180 # phase_angle = angular separation of the sun and Earth, as seen on the moon
    moon_value_table[i, "fraction"] <- (suncalc::getMoonIllumination(date = date_time_start + (i - 1) * time_interval_minutes*60)[1, "fraction"]) # Moon illuminated fraction, not used in calculation. This is calculated only for user to learn about the moon phase.
    moon_value_table[i, "Z_moon"] <- 90 - (REdaS::rad2deg(suncalc::getMoonPosition(date = date_time_start + (i - 1) * time_interval_minutes * 60, lat = latitude, lon = longitude)[1, "altitude"])) # Z_moon = Zenith distance in degree of the moon = 90 - moon altitude
    moon_value_table[i, "distance"] <- suncalc::getMoonPosition(date = date_time_start + (i - 1) * time_interval_minutes * 60, lat = latitude, lon = longitude)[1, "distance"] # distance = moon/Earth distance in km
    moon_value_table[i, "sun_altitude"] <- REdaS::rad2deg(suncalc::getSunlightPosition(date = date_time_start + (i - 1) * time_interval_minutes * 60, lat = latitude, lon = longitude, keep = c("altitude"))[1, "altitude"]) # sun_altitude = altitude of the sun in degree
    #pb$tick() # Update the progress bar
    if(show_progress == TRUE) {pb$tick()}
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
  # The atmospheric pressure at the site elevation relative to sea level
  g <- 9.80665  # gravity constant in m/s^2
  L <- 0.0065   # mean adiabatic lapse rate in K/m
  M <- 0.028963 # molecular weight of dry air in kg/mol
  R <- 8.3145   # universal gas constant in J/(mol*K)
  TK0 <- 298.15 # standard temperature in K (25 degrees Celsius)
  patm0 <- 101325  # standard atmospheric pressure at sea level in Pa

  # Compute atmospheric pressure relative to sea level
  atm_pressure_relative_to_sea <- (patm0 * (1 - L * site_elev / TK0)^(g * M / (R * L))) / 101325


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

  #---------------------------ADD ECLIPSE COLUMN---------------------------
  # MoonShineR warns the user if an eclipse occurs during a simulation, and it reports the start and end time of the simulation.
  # However, MoonShineR does not simulate the reduction in moon ground illuminance associated with the eclipse.

  # Define the condition for eclipse as a logical vector
  eclipse_condition <- abs(moon_value_table$phase_angle) < 1.5 & moon_value_table$sun_altitude < 0

  # Add the new column 'eclipse' to moon_value_table
  moon_value_table$eclipse <- eclipse_condition

  # Check if any eclipse occurs and print a message accordingly
  if (any(eclipse_condition)) {
    message("Calculation completed. ECLIPSE IN THE SIMULATION PERIOD!")
    # You can still use 'which' to index the rows where eclipse occurs if you need
    moon_value_table[which(eclipse_condition == TRUE),]
  } else {
    message("Calculation completed. No lunar eclipse in the simulation period.")
  }

  #---------------------------END OF ILLUMINATION COMPUTATION--------------------

# Save moon table csv file
  if (export_table) {
    utils::write.csv(
      moon_value_table,
      file.path(output_directory, "lux_calculator_output.csv"),
      row.names = TRUE
    )
  }

  return(moon_value_table)

  #---------------------------END OF SCRIPT---------------------------
}
