#' @title get_accuracy
#'
#' @description Calculates the accuracy of one or multiple symptom checkers
#'
#' @param data A dataframe
#' @param correct A string indicating the column name storing if the symptom checker solved the case (TRUE or FALSE)
#' @param apps A string indicating the column name storing the app names (optional)
#'
#' @return A data frame object containing the accuracy of the symptom checker or the accuracy of multiple symptom checkers. Use the apps argument to calculate this metric for multiple symptom checkers.
#' @examples
#' data(symptomcheckRdata)
#' accuracy <- get_accuracy(
#'   data = symptomcheckRdata,
#'   correct = "Correct_Triage_Advice_provided_from_app",
#'   apps = "App_name"
#'   )
#' @export
#' @import dplyr

get_accuracy <- function(data, correct, apps = NULL) {
  # Code input for handling with dplyr
  correct_sym <- sym(correct)

  # Handle errors
  if (!is.data.frame(data)) {
    stop("The first argument must be a data frame.")
  }

  if (!is.character(correct) || !(correct %in% names(data))) {
    stop("The second argument must be a valid column name (as a string) from the data frame indicating if the case was solved correctly.")
  }

  if (!all(data[[correct]] %in% c(TRUE, FALSE, NA))) {
    stop("The correct column must contain only Boolean values (TRUE or FALSE).")
  }

  if (!is.null(apps)) {
    if (!is.character(apps) || !(apps %in% names(data))) {
      stop("The third argument must be a valid column name (as a string) from the data frame indicating the apps in the data frame.")
    }

    # Code input for handling with dplyr
    apps_sym <- sym(apps)
    # Get number of symptom checkers
    n_groups <- data %>% count(!!apps_sym) %>% nrow()

    # Output message if apps provided, but they only contain one symptom checker
    if (n_groups == 1) {
      message("Only one app provided, calculating accuracy for this app")
    }
    # Calculate accuracy for each symptom checker
    output <- data %>%
      group_by(!!apps_sym) %>%
      summarise(accuracy = mean(!!correct_sym, na.rm = TRUE))
  } else {
    # Output message that accuracy is calculated across dataset
    message("No apps vector specified, calculating accuracy across the entire dataset.")

    # Calculate accuracy
    output <- data %>%
      summarise(accuracy = mean(!!correct_sym, na.rm = TRUE))
  }

  return(output)
}
