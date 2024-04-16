#' @title plot_safety_of_advice
#'
#' @description Plots the safety of advice for one or multiple symptom checkers
#'
#' @param input A dataframe containing the output of get_safety_of_advice()
#'
#' @return A ggplot object visualizing the safety of advice for either one or multiple symptom checkers
#' @examples
#' data(symptomcheckRdata)
#' safety_of_advice <- get_safety_of_advice(
#'   data = symptomcheckRdata,
#'   triagelevel_correct = "Goldstandard_solution",
#'   triagelevel_advice = "Triage_advice_from_app",
#'   order_triagelevel = c("Emergency", "Non-Emergency", "Self-care"),
#'   apps = "App_name"
#'   )
#' safety_plot <- plot_safety_of_advice(safety_of_advice)
#'
#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2

plot_safety_of_advice <- function(input) {
  safety_percentage <- NULL
  app <- NULL
  category <- NULL
  value <- NULL
  # Handle errors
  if(!is.list(input)) {
    stop("The input is not a list. Please make sure to run the get_safety_of_advice() function first and use the result as input.")
  }

  # Extract percentage dataframe from list
  input <- as.data.frame(input[2])
  # Rename column for easier handling
  colnames(input)[colnames(input) == "percentage.safety_percentage"] <- "safety_percentage"

  if(!is.data.frame(input)) {
    stop("The safety is not a dataframe. Please make sure to run the get_safety_of_advice() function first and use the result as input.")
  }

  if (ncol(input) > 2) {
    stop("The input list must be the result of using get_safety_of_advice(). You must not use any other input.")
  }

  if (!"safety_percentage" %in% names(input)) {
    stop("The 'safety_percentage' variable is not found in the input. Please ensure you obtain the accuracy using get_safety_of_advice() function first and use the result as input.")
  }

  # Extract the first column name to check if dataframe contains one or multiple symptom checkers
  first_col_name <- names(input)[1]

  # Check if dataframe contains one or multiple symptom checkers
  if (first_col_name != "safety_percentage") {
    first_col_length <- length(input[[first_col_name]])
    names(input)[1] <- "app"

    # Check if the length is 1
    if (first_col_length == 1) {
      message("Only one app in dataset. Visualizing safety of advice for this app.")
    }

    # Use input and ggplot2 to visualize the safety of advice for each app
    output <- input %>%
      mutate(safety_percentage_inverted = 100 - safety_percentage) %>%
      pivot_longer(cols = !app, names_to = "category", values_to = "value") %>%
      mutate(category = factor(category, levels = c("safety_percentage_inverted", "safety_percentage"))) %>%
      ggplot(aes(x = app, y = value, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values =
                          c("safety_percentage" = "#5ba300", "safety_percentage_inverted" = "#b51963"),
                        name = element_blank(),
                        labels = c("Unsafe Advice", "Safe Advice")
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_light() +
      labs(x = "", y = "Safety of Advice") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank()
      )


  } else {
    # Use input and ggplot2 ti visualize the safety of advice across the whole dataset
    output <- input %>%
      mutate(safety_percentage_inverted = 100 - safety_percentage) %>%
      pivot_longer(cols = everything(), names_to = "category", values_to = "value") %>%
      mutate(category = factor(category, levels = c("safety_percentage_inverted", "safety_percentage"))) %>%
      ggplot(aes(x = 1, y = value, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values =
                          c("safety_percentage" = "#5ba300", "safety_percentage_inverted" = "#b51963"),
                        name = element_blank(),  # Renaming the legend title
                        labels = c("Unsafe Advice", "Safe Advice")
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_light() +
      labs(x = "", y = "Safety of Advice") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank()
      )
  }
  return(output)
}
