#' @title get_comprehensiveness
#'
#' @description Calculates the comprehensiveness for one or multiple symptom checkers
#'
#' @param data A dataframe
#' @param triagelevel_advice A string indicating the column name storing the recommendation of a symptom checker for a case
#' @param vector_not_entered A vector indicating the values in which missing values are coded (e.g., as NA or a specified value such as -99)
#' @param apps A string indicating the column name storing the app names
#' @param CI A Boolean (TRUE or FALSE) indicating whether 95\% confidence intervals should be output (optional)
#'
#' @return A list containing both a raw number and the percentage of comprehensiveness for one or multiple symptom checkers
#' @examples
#' data(symptomcheckRdata)
#' comprehensiveness <- get_comprehensiveness(
#'   data = symptomcheckRdata,
#'   triagelevel_advice = "Triage_advice_from_app",
#'   vector_not_entered = c(NA),
#'   apps = "App_name",
#'   CI = TRUE
#'   )
#'
#' @export
#' @import dplyr

get_comprehensiveness <- function(data, triagelevel_advice, vector_not_entered, apps = NULL, CI = FALSE) {
  gave_advice <- NULL
  comprehensiveness_percentage <- NULL
  qnorm <- NULL
  lower_ci <- NULL
  upper_ci <- NULL
  # Handle errors
  if (!is.data.frame(data)) {
    stop("The first argument must be a data frame.")
  }

  if (!is.character(triagelevel_advice) || !(triagelevel_advice %in% names(data))) {
    stop("The second argument must be a valid column name (as a string) from the data frame indicating the advice the symptom checker gave.")
  }

  if (!is.vector(vector_not_entered) || is.list(vector_not_entered)) {
    stop("vector_not_entered must be a vector indicating all values that indicate that a case was not able to be entered (e.g. NA).")
  }

  if (is.null(apps)) {
    message("No apps column specified, calculating accuracy across the entire dataset.")
  }
  # Code input for handling with dplyr
  triagelevel_advice_sym <- sym(triagelevel_advice)

  if (!is.null(apps)) {
    if (!is.character(apps) || !(apps %in% names(data))) {
      stop("The fourth argument must be a valid column name (as a string) from the data frame indicating the apps in the data frame.")
    }
    # Code input for handling with dplyr
    apps_sym <- sym(apps)
    # Get number of symptom checkers
    n_groups <- data %>% count(!!apps_sym) %>% nrow()

    # Output message if apps specified, but contains only one value
    if (n_groups == 1) {
      message("Only one app provided, calculating accuracy for this app")
    }

    # Code missing values accordingly
    data <- data %>%
      mutate(gave_advice = case_when(
        !!triagelevel_advice_sym %in% vector_not_entered ~ "Advice not given",
        TRUE ~ "Advice given"
      ))

    # Calculate raw numbers of comprehensiveness
    raw_numbers <- data %>%
      group_by(!!apps_sym, gave_advice) %>%
      count() %>%
      ungroup()

    # Calculate percentage of comprehensiveness
    if (CI == TRUE) {
    percentage <- data %>%
      group_by(!!apps_sym, gave_advice) %>%
      count() %>%
      ungroup() %>%
      group_by(!!apps_sym) %>%
      summarise(comprehensiveness_percentage = sum(n[gave_advice == "Advice given"]) / sum(n),
                n = sum(n)) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(
        lower_ci = ifelse(CI, comprehensiveness_percentage - qnorm(0.975) * sqrt((comprehensiveness_percentage * (1 - comprehensiveness_percentage))/n), NA_real_),
        upper_ci = ifelse(CI, comprehensiveness_percentage + qnorm(0.975) * sqrt((comprehensiveness_percentage * (1 - comprehensiveness_percentage))/n), NA_real_)
      ) %>%
      mutate(comprehensiveness_percentage = comprehensiveness_percentage*100,
             lower_ci = lower_ci*100,
             upper_ci = upper_ci*100) %>%
      mutate(lower_ci = case_when(
        lower_ci < 0 ~ 0,
        lower_ci > 100 ~ 100,
        TRUE ~ lower_ci),
        upper_ci = case_when(
          upper_ci < 0 ~ 0,
          upper_ci > 100 ~ 100,
          TRUE ~ lower_ci)) %>%
      select(-n)

    } else {
      percentage <- data %>%
        group_by(!!apps_sym, gave_advice) %>%
        count() %>%
        ungroup() %>%
        group_by(!!apps_sym) %>%
        summarise(comprehensiveness_percentage = sum(n[gave_advice == "Advice given"]) / sum(n) * 100) %>%
        ungroup()
    }

  } else {
    # Code missing values accordingly
    data <- data %>%
      mutate(gave_advice = case_when(
        !!triagelevel_advice_sym %in% vector_not_entered ~ "Advice not given",
        TRUE ~ "Advice given"
      ))

    # Calculate raw numbers of comprehensiveness
    raw_numbers <- data %>%
      group_by(gave_advice) %>%
      count()

    # Calculate percentage of comprehensiveness
    if (CI == TRUE) {
      percentage <- data %>%
        group_by(gave_advice) %>%
        count() %>%
        ungroup() %>%
        summarise(comprehensiveness_percentage = sum(n[gave_advice == "Advice given"]) / sum(n),
                  n = sum(n)) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(
          lower_ci = ifelse(CI, comprehensiveness_percentage - qnorm(0.975) * sqrt((comprehensiveness_percentage * (1 - comprehensiveness_percentage))/n), NA_real_),
          upper_ci = ifelse(CI, comprehensiveness_percentage + qnorm(0.975) * sqrt((comprehensiveness_percentage * (1 - comprehensiveness_percentage))/n), NA_real_)
        ) %>%
        mutate(comprehensiveness_percentage = comprehensiveness_percentage*100,
               lower_ci = lower_ci*100,
               upper_ci = upper_ci*100) %>%
        mutate(lower_ci = case_when(
          lower_ci < 0 ~ 0,
          lower_ci > 100 ~ 100,
          TRUE ~ lower_ci),
          upper_ci = case_when(
            upper_ci < 0 ~ 0,
            upper_ci > 100 ~ 100,
            TRUE ~ lower_ci)) %>%
        select(-n)
    } else {
    percentage <- data %>%
      group_by(gave_advice) %>%
      count() %>%
      ungroup() %>%
      summarise(comprehensiveness_percentage = sum(n[gave_advice == "Advice given"]) / sum(n) * 100)
    }

  }
  # Output raw numbers and percentage as a list
  output_combination <- list(raw_numbers = raw_numbers, percentage = percentage)
  return(output_combination)
}
