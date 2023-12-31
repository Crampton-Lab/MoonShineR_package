#' Create a plot from the illuminance prediction dataframe
#'
#' * plot_lux() plots the illuminance prediction dataframe generated by predict_lux().
#' * To learn more about MoonShineR, see instruction manual: <https://lokpoon.github.io/moonshine_manual/overview.html>
#' @param df `character`. Name of the data.frame object created by predict_lux().
#' @param illuminance_type_plot `character`. Choose one type of illuminance to plot. See options in the next section. Default is `"moon_final_lux_nighttime"`.
#' @param plot_y_max `character` `"AUTO"`, or `numeric`. Let the plot y-axis scale automatically or manually set a y-axis upper limit. Affects both the plot in the plot window and the exported .pdf. Default is `"AUTO"`.
#' @param plot_dayttime_gray_mask `logical`. `TRUE` to mask daytime plot line in gray. Affects both the plot in the plot window and the exported .pdf. `FALSE` to disable (plot line always black). Default is `TRUE`.
#' @param plot_ecliipse_mask `logical`. `TRUE` to add a red shade during times of lunar eclipse as a warning that those illuminance prediction might be overestimating. `FALSE` to disable (plot line always black). Default is `TRUE`.
#' @param plot_twilight `character`. Set the twilight period to plot as a gray area. `"astro"` is astronomical twilight (longest). `"nautic"` is nautical twilight (intermediate). `"civil"` is civil twilight (shortest). `"none"` to disable plotting of twilight period. Default is `"astro"`.
#' @param vertical_time_label `logical`. Rotate datetime label to be vertical.
#' @param time_label_interval_hr `numeric`. Set the datetime label in number of hours.
#' @param time_label_shift_hr `numeric`. Shift the x-axis start time by certain number of hours for a more tidy datetime label. Use negative values to shift the start time to a time before the first data point (i.e., add a blank space to the left), as to avoid cutting out data.
#' @details
#' # `illuminance_type_plot` options:
#' * **"moon_final_lux"** plots only the illuminance of moonlight (plus the darksky_value) during both day and night.
#' * **"moon_final_lux_nighttime"** plots only the illuminance of moonlight at night (no value during daytime, when `sun_altitude` > 0 degrees)
#' * **"moonlight_twilight_nighttime"** plots the illuminance of moonlight plus twilight (no value during daytime, when `sun_altitude` > 0 degrees)
#' * **"twilight"** plots only the illuminance of twilight (defined as the light when `sun_altitude` < 0 degrees).
#' * **"sunlight"** plots only the illuminance of sunlight (defined as the light when `sun_altitude` > 0 degrees).
#' * **"sunlight_twilight"** plots only the sum of sunlight and twilight.
#' * **"total_illuminance_all"** plots all illuminance together, calculated as the sum of moonlight, twilight, and sunlight.
#' * Note: The above terms corresponding to the columns headers in the predict_lux() generated dataframe.
#' # Notes:
#' * The magnitude of moonlight and sunlight illuminance diffsers immensely. So depending on the `illuminance_type_plot` selected, the user might want to manually adjust the plot_y_max.
#' * `plot_dayttime_gray_mask` is `TRUE` by default to mask daytime illuminance in gray. This is particularly useful when plotting `"moon_final_lux"` to make it clear that moonlight during daytime should be ignored.
#' * Nighttime is always shaded in dark gray.
#' * Twilight period is shaded in light gray. The period of twilight can be adjusted with `plot_twilight`.
#' @keywords moonlight
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' # Plot a predict_lux generated dataframe named moonlight_output
#'
#' plot_lux(df = moonlight_output, illuminance_type_plot = "total_illuminance_all",
#'          plot_y_max = 0.3,  plot_dayttime_gray_mask = TRUE, plot_eclipse_red_mask = TRUE,
#'          plot_twilight = "astro", vertical_time_label = TRUE, time_label_interval_hr = 24,
#'          time_label_shift_hr = 0)




plot_lux <- function(df = NULL, illuminance_type_plot = "total_illuminance_all", plot_y_max = 0.3,  plot_dayttime_gray_mask = TRUE, plot_eclipse_red_mask = TRUE, plot_twilight = "astro",
                     vertical_time_label = TRUE, time_label_interval_hr = 24, time_label_shift_hr = 0) {

  # Error messages for the arguments

  if (missing(df)) {
    stop("Argument 'df' is missing! See ??MoonShineR::plot_lux")
  }

  print("For all plotting features, see ??MoonShineR::plot_lux")

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
  night_time <- lubridate::as_datetime(night_time$datetime, tz = lubridate::tz(df$datetime))

  # Extract time interval from df
  time_interval_minutes <- as.double(df$datetime[2] - df$datetime[1], unit = "mins")

  # Define twilight period
  if(plot_twilight == "astro"){twilight_angle <- -18}
  if(plot_twilight == "nautic"){twilight_angle <- -12}
  if(plot_twilight == "civil"){twilight_angle <- -6}
  if(plot_twilight == "none"){twilight_angle <- 0}

  after_twilight_time <- dplyr::filter(df, sun_altitude < (twilight_angle)) %>% dplyr::select(datetime)
  after_twilight_time <- lubridate::as_datetime(after_twilight_time$datetime, tz =  lubridate::tz(df$datetime))

  day_time <- dplyr::filter(df, sun_altitude > 0) %>% dplyr::select(datetime)
  day_time <- lubridate::as_datetime(day_time$datetime, tz = lubridate::tz(df$datetime))

  eclipse_time <- dplyr::filter(df, eclipse == TRUE) %>% dplyr::select(datetime)
  eclipse_time <- lubridate::as_datetime(eclipse_time$datetime, tz = lubridate::tz(df$datetime))

  # Time label interval
  time_label <- paste0(time_label_interval_hr, " hour")

  # shift time label
  shift <- time_label_shift_hr * 60 * 60

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
    ggplot2::scale_x_datetime(date_breaks = time_label,  date_labels = "%Y %b %d %H:%M", limits = c(df$datetime[1] + shift, NA)) + # change the date break here for different x-axis label. A more specific format can be specified.
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
        ggplot2::scale_x_datetime(date_breaks = time_label, date_labels = "%Y %b %d %H:%M", limits = c(df$datetime[1] + shift, NA)) + # change the date break here for different x-axis label. A more specific format can be specified.
        ggplot2::labs(x = "", y = "predicted ground illuminance (lx)") }


  if(plot_dayttime_gray_mask == TRUE){
    plot_output <- plot_output +
                    ggplot2::geom_rect(ggplot2::aes(xmin = day_time, # day time mask moonlight regression to gray color
                                      xmax = day_time + lubridate::dminutes(time_interval_minutes),
                                      ymin = 0, ymax = Inf), fill = "white", alpha = 0.85, na.rm = TRUE)}


  if(plot_eclipse_red_mask == TRUE){
    plot_output <- plot_output +
      ggplot2::geom_rect(ggplot2::aes(xmin = eclipse_time, # day time mask moonlight regression to gray color
                                      xmax = eclipse_time + lubridate::dminutes(time_interval_minutes),
                                      ymin = 0, ymax = Inf), fill = "red", alpha = 0.85, na.rm = TRUE) +
      ggplot2::geom_line(data = df, ggplot2::aes(x = datetime, y = eval(as.symbol(illuminance_type_plot))), colour = 'black', linewidth = 0.75)}

  if(vertical_time_label == TRUE){
    plot_output <- plot_output +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))} # rotate date label 90 degree.

  print(plot_output)

}

  #---------------------------END OF SCRIPT---------------------------
