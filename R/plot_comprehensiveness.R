#' @title plot_comprehensiveness
#'
#' @description Plots the comprehensiveness for one or multiple symptom checkers
#'
#' @param input A dataframe containing the output of get_comprehensiveness()
#'
#' @return A ggplot object visualizing the comprehensiveness for either one or multiple symptom checkers
#' @examples
#' data(symptomcheckRdata)
#' comprehensiveness <- get_comprehensiveness(
#'   data = symptomcheckRdata,
#'   triagelevel_advice = "Triage_advice_from_app",
#'   vector_not_entered = c(NA),
#'   apps = "App_name"
#'   )
#' comprehensiveness_plot <- plot_comprehensiveness(comprehensiveness)

#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2

plot_comprehensiveness <- function(input) {
  comprehensiveness_percentage <- NULL
  app <- NULL
  category <- NULL
  value <- NULL
  # Handle errors
  if(!is.list(input)) {
    stop("The input is not a list. Please make sure to run the get_comprehensiveness() function first and use the result as input.")
  }

  # Extract percentage dataframe from list
  input <- as.data.frame(input[2])
  # Rename column for easier handling
  colnames(input)[colnames(input) == "percentage.comprehensiveness_percentage"] <- "comprehensiveness_percentage"

  if(!is.data.frame(input)) {
    stop("The comprehensiveness is not a dataframe. Please make sure to run the get_comprehensiveness() function first and use the result as input.")
  }

  if (ncol(input) > 2) {
    stop("The input list must be the result of using get_comprehensiveness(). You must not use any other input.")
  }

  if (!"comprehensiveness_percentage" %in% names(input)) {
    stop("The 'comprehensiveness_percentage' variable is not found in the input. Please ensure you obtain the accuracy using get_comprehensiveness() function first and use the result as input.")
  }

  # Extract the first column name to check if dataframe contains one or multiple symptom checkers
  first_col_name <- names(input)[1]

  # Check if dataframe contains one or multiple symptom checkers
  if (first_col_name != "comprehensiveness_percentage") {
    first_col_length <- length(input[[first_col_name]])
    names(input)[1] <- "app"

    # Check if the length is 1
    if (first_col_length == 1) {
      message("Only one app in dataset. Visualizing comprehensiveness for this app.")
    }

    # Use input and ggplot2 to visualize the comprehensiveness for each app
    output <- input %>%
      mutate(comprehensiveness_percentage_inverted = 100 - comprehensiveness_percentage) %>%
      pivot_longer(cols = !app, names_to = "category", values_to = "value") %>%
      mutate(category = factor(category, levels = c("comprehensiveness_percentage_inverted", "comprehensiveness_percentage"))) %>%
      ggplot(aes(x = app, y = value, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values =
                          c("comprehensiveness_percentage" = "#5ba300", "comprehensiveness_percentage_inverted" = "#b51963"),
                        name = element_blank(),
                        labels = c("Advice Not Provided", "Advice Provided")
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_light() +
      labs(x = "", y = "Comprehensiveness") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank()
      )


  } else {
    # Use input and ggplot2 to visualize the comprehensiveness across the whole dataset
    output <- input %>%
      mutate(comprehensiveness_percentage_inverted = 100 - comprehensiveness_percentage) %>%
      pivot_longer(cols = everything(), names_to = "category", values_to = "value") %>%
      mutate(category = factor(category, levels = c("comprehensiveness_percentage_inverted", "comprehensiveness_percentage"))) %>%
      ggplot(aes(x = 1, y = value, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values =
                          c("comprehensiveness_percentage" = "#5ba300", "comprehensiveness_percentage_inverted" = "#b51963"),
                        name = element_blank(),  # Renaming the legend title
                        labels = c("Advice Not Provided", "Advice Provided")
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_light() +
      labs(x = "", y = "Comprehensiveness") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank()
      )
  }
  return(output)
}
