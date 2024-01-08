#' @title plot_inclination_to_overtriage
#'
#' @description Plots the inclination to overtriage for one or multiple symptom checkers
#'
#' @param input A dataframe containing the output of get_inclination_to_overtriage()
#'
#' @return A ggplot object visualizing the inclination to overtriage for either one or multiple symptom checkers
#' @examples
#' data(symptomcheckRdata)
#' inclination_to_overtriage <- get_inclination_to_overtriage(
#'   data = symptomcheckRdata,
#'   triagelevel_correct = "Goldstandard_solution",
#'   triagelevel_advice = "Triage_advice_from_app",
#'   order_triagelevel = c("Emergency", "Non-Emergency", "Self-care"),
#'   apps = "App_name"
#'   )
#' overtriage_plot <- plot_inclination_to_overtriage(inclination_to_overtriage)

#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2

plot_inclination_to_overtriage <- function(input) {
  inclination_to_overtriage_percentage <- NULL
  app <- NULL
  category <- NULL
  value <- NULL
  # Handle errors
  if(!is.list(input)) {
    stop("The input is not a list. Please make sure to run the get_inclination_to_overtriage() function first and use the result as input.")
  }

  # Extract percentage dataframe from list
  input <- as.data.frame(input[2])
  # Rename column for easier handling
  colnames(input)[colnames(input) == "percentage.inclination_to_overtriage_percentage"] <- "inclination_to_overtriage_percentage"

  # Handle inccorect input
  if(!is.data.frame(input)) {
    stop("The inclination to overtriage is not a dataframe. Please make sure to run the get_inclination_to_overtriage() function first and use the result as input.")
  }

  if (ncol(input) > 2) {
    stop("The input list must be the result of using get_inclination_to_overtriage(). You must not use any other input.")
  }

  if (!"inclination_to_overtriage_percentage" %in% names(input)) {
    stop("The 'inclination_to_overtriage_percentage' variable is not found in the input. Please ensure you obtain the accuracy using get_inclination_to_overtriage() function first and use the result as input.")
  }

  # Extract the first column name to check if dataframe contains one or multiple symptom checkers
  first_col_name <- names(input)[1]

  # Check if dataframe contains one or multiple symptom checkers
  if (first_col_name != "inclination_to_overtriage_percentage") {
    first_col_length <- length(input[[first_col_name]])
    names(input)[1] <- "app"

    # Check if the length is 1
    if (first_col_length == 1) {
      message("Only one app in dataset. Visualizing inclination to overtriage for this app.")
    }

    # Use input and ggplot2 to visualize the inclination to overtriage for each app
    output <- input %>%
      mutate(inclination_to_overtriage_percentage_inverted = 100 - inclination_to_overtriage_percentage) %>%
      pivot_longer(cols = !app, names_to = "category", values_to = "value") %>%
      mutate(category = factor(category, levels = c("inclination_to_overtriage_percentage_inverted", "inclination_to_overtriage_percentage"))) %>%
      ggplot(aes(x = app, y = value, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values =
                          c("inclination_to_overtriage_percentage" = "#8EA4D2", "inclination_to_overtriage_percentage_inverted" = "#6279B8"),
                        name = element_blank(),
                        labels = c("Overtriage Error", "Undertriage Error")
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_light() +
      labs(x = "", y = "Inclination to Overtriage") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank()
      )


  } else {
    # Use input and ggplot2 ti visualize the inclination to overtriage across the whole dataset
    output <- input %>%
      mutate(inclination_to_overtriage_percentage_inverted = 100 - inclination_to_overtriage_percentage) %>%
      pivot_longer(cols = everything(), names_to = "category", values_to = "value") %>%
      mutate(category = factor(category, levels = c("inclination_to_overtriage_percentage_inverted", "inclination_to_overtriage_percentage"))) %>%
      ggplot(aes(x = 1, y = value, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values =
                          c("inclination_to_overtriage_percentage" = "#8EA4D2", "inclination_to_overtriage_percentage_inverted" = "#6279B8"),
                        name = element_blank(),  # Renaming the legend title
                        labels = c("Overtriage Error", "Undertriage Error")
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_light() +
      labs(x = "", y = "Inclination to Overtriage") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank()
      )
  }
  return(output)
}
