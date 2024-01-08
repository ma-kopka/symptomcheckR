#' @title plot_accuracy_by_triage
#'
#' @description Plots the accuracy on each triage level for one or multiple symptom checkers
#'
#' @param input A dataframe containing the output of get_accuracy_by_triage()
#'
#' @return A ggplot object visualizing the accuracy on each triage level for either one or multiple symptom checkers
#' @examples
#' data(symptomcheckRdata)
#' accuracy_by_triage <- get_accuracy_by_triage(
#'   data = symptomcheckRdata,
#'   correct = "Correct_Triage_Advice_provided_from_app",
#'   triagelevel = "Goldstandard_solution",
#'   apps = "App_name"
#'   )
#' accuracy_triage_plot <- plot_accuracy_by_triage(accuracy_by_triage)

#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2

plot_accuracy_by_triage <- function(input) {
  app <- NULL
  accuracy <- NULL
  category <- NULL
  solution <- NULL
  value <- NULL
  # Handle errors
  if(!is.data.frame(input)) {
    stop("The input is not a dataframe. Please make sure to run the get_accuracy_by_triage() function first and use the result as input.")
  }

  if (ncol(input) > 3) {
    stop("The input dataframe must be the result of using get_accuracy_by_triage(). You must not use any other input.")
  }

  if (!"accuracy" %in% names(input)) {
    stop("The 'accuracy' variable is not found in the input. Please ensure you obtain the accuracy using get_accuracy_by_triage() function first and use the result as input.")
  }

  # Extract the first column name to check if dataframe contains one or multiple symptom checkers
  first_col_name <- names(input)[2]

  # Check if dataframe contains one or multiple symptom checkers and assign new variable names for easier handling
  if (first_col_name != "accuracy") {
    names(input)[1] <- "app"
    names(input)[2] <- "solution"
    app_counts <- input %>% #check if dataframe contains multiple symptom checkers
      group_by(app) %>%
      summarise(count = n(), .groups = "drop")

    # Check if the length is 1 and output message
    if (nrow(app_counts) == 1) {
      message("Only one app in dataset. Visualizing accuracy for this app.")
    }

    # Use input and ggplot2 to visualize the accuracy on each triage level for each app
    output <- input %>%
      mutate(accuracy = 100*accuracy,
             accuracy_inverted = 100 - accuracy) %>%
      pivot_longer(cols = !c(app, solution), names_to = "category", values_to = "value") %>%
      mutate(category = factor(category, levels = c("accuracy_inverted", "accuracy"))) %>%
      ggplot(aes(x = app, y = value, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values =
                          c("accuracy" = "#5ba300", "accuracy_inverted" = "#b51963"),
                        name = element_blank(),  # Renaming the legend title
                        labels = c("Incorrect", "Correct")
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_light() +
      labs(x = "", y = "Accuracy") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(color = "black")
      ) +
      facet_wrap(~solution)


  } else {
    # Use input and ggplot2 to visualize the accuracy on each triage level across the whole dataset
    output <- input %>%
      mutate(accuracy = 100*accuracy,
             accuracy_inverted = 100 - accuracy) %>%
      pivot_longer(cols = !solution, names_to = "category", values_to = "value") %>%
      mutate(category = factor(category, levels = c("accuracy_inverted", "accuracy"))) %>%
      ggplot(aes(x = 1, y = value, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values =
                          c("accuracy" = "#5ba300", "accuracy_inverted" = "#b51963"),
                        name = element_blank(),  # Renaming the legend title
                        labels = c("Incorrect", "Correct")
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_light() +
      labs(x = "", y = "Accuracy") +
      facet_wrap(~solution) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(color = "black")
      )
  }
  return(output)
}
