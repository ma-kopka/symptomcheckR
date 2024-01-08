#' @title get_safety_of_advice
#'
#' @description Calculates the safety of advice for one or multiple symptom checkers
#'
#' @param data A dataframe
#' @param triagelevel_correct A string indicating the column name storing the correct triage solutions
#' @param triagelevel_advice A string indicating the column name storing the recommendation of a symptom checker for a case
#' @param order_triagelevel A vector indicating the order of triage levels. The triage level with highest urgency should be the first value and the triage level with lowest urgency the last value.
#' @param apps A string indicating the column name storing the app names (optional)
#'
#' @return A list containing both a raw number and the percentage of safe advice for one or multiple symptom checkers
#' @examples
#' data(symptomcheckRdata)
#' get_safety_of_advice <- get_safety_of_advice(
#'   data = symptomcheckRdata,
#'   triagelevel_correct = "Goldstandard_solution",
#'   triagelevel_advice = "Triage_advice_from_app",
#'   order_triagelevel = c("Emergency", "Non-Emergency", "Self-care"),
#'   apps = "App_name"
#'   )

#' @export
#' @import dplyr
#' @importFrom stats setNames
#' @importFrom stats na.omit

get_safety_of_advice <- function(data, triagelevel_correct, triagelevel_advice, order_triagelevel, apps = NULL) {
  safety <- NULL
  # Handle errors
  if (!is.data.frame(data)) {
    stop("The first argument must be a data frame.")
  }

  if (!is.character(triagelevel_correct) || !(triagelevel_correct %in% names(data))) {
    stop("The second argument must be a valid column name (as a string) from the data frame indicating the correct triage level for the case.")
  }

  if (!is.character(triagelevel_advice) || !(triagelevel_advice %in% names(data))) {
    stop("The third argument must be a valid column name (as a string) from the data frame indicating the advice the symptom checker gave.")
  }

  if (!is.vector(order_triagelevel) || is.list(order_triagelevel)) {
    stop("order_triagelevel must be a vector.")
  }

  if (is.null(apps)) {
    message("No apps column specified, calculating accuracy across the entire dataset.")
  }

  if (any(is.na(data[[triagelevel_correct]]))) {
    data <- na.omit(data, cols = triagelevel_correct)
    message("The triagelevel_correct column contains NA values. Rows with NA values in the triagelevel_correct column have been removed.")
  }

  if (any(is.na(data[[triagelevel_advice]]))) {
    data <- na.omit(data, cols = triagelevel_advice)
    message("The triagelevel_advice column contains NA values. Rows with NA values in the triagelevel_advice column have been removed.")
  }

  # Get each value in triagelevels_correct and triagelevel_advice for comparison
  unique_triagelevels_correct <- unique(data[[triagelevel_correct]])
  unique_triagelevel_advice <- unique(data[[triagelevel_advice]])

  # Compare if values of triagelevels_correct and triagelevel_advice are the same. If not, stop and output the values misising in either input variable.
  if (!all(order_triagelevel %in% unique_triagelevels_correct)) {
    missing_values <- order_triagelevel[!order_triagelevel %in% unique_triagelevels_correct]
    stop(paste("The following values in order_triagelevel are not present in triagelevel_correct column:", paste(missing_values, collapse = ", ")))
  }

  if (!all(unique_triagelevels_correct %in% order_triagelevel)) {
    extra_values <- unique_triagelevels_correct[!unique_triagelevels_correct %in% order_triagelevel]
    stop(paste("The triagelevel column contains additional values not present in order_triagelevel:", paste(extra_values, collapse = ", ")))
  }

  if (!all(order_triagelevel %in% unique_triagelevel_advice)) {
    missing_values <- order_triagelevel[!order_triagelevel %in% unique_triagelevel_advice]
    stop(paste("The following values in order_triagelevel are not present in triagelevel_advice column:", paste(missing_values, collapse = ", ")))
  }

  if (!all(unique_triagelevel_advice %in% order_triagelevel)) {
    extra_values <- unique_triagelevel_advice[!unique_triagelevel_advice %in% order_triagelevel]
    stop(paste("The triagelevel_advice column contains additional values not present in order_triagelevel:", paste(extra_values, collapse = ", ")))
  }

  if (!all(unique_triagelevels_correct %in% unique_triagelevel_advice)) {
    missing_values <- unique_triagelevels_correct[!unique_triagelevels_correct %in% unique_triagelevel_advice]
    stop(paste("The following values in triagelevel_correct are not present in triagelevel_advice column:", paste(missing_values, collapse = ", ")))
  }

  if (!all(unique_triagelevel_advice %in% unique_triagelevels_correct)) {
    extra_values <- unique_triagelevel_advice[!unique_triagelevel_advice %in% unique_triagelevels_correct]
    stop(paste("The triagelevel_advice column contains additional values not present in triagelevel_correct:", paste(extra_values, collapse = ", ")))
  }

  # Assigns numbers (for later comparison) to input triage levels and store in recode vector
  recode_vector <- setNames(seq_along(order_triagelevel), order_triagelevel)

  # Recodes triage levels in both triage input variables with numbers (for later comparison)
  data[[triagelevel_correct]] <- recode_vector[data[[triagelevel_correct]]]
  data[[triagelevel_advice]] <- recode_vector[data[[triagelevel_advice]]]

  # Code input for handling with dplyr
  triagelevel_correct_sym <- sym(triagelevel_correct)
  triagelevel_advice_sym <- sym(triagelevel_advice)

  # Check if apps input is empty
  if (!is.null(apps)) {
    if (!is.character(apps) || !(apps %in% names(data))) {
      stop("The fifth argument must be a valid column name (as a string) from the data frame indicating the apps in the data frame.")
    }

    # Code input for handling with dplyr
    apps_sym <- sym(apps)
    # Get number of symptom checkers
    n_groups <- data %>% count(!!apps_sym) %>% nrow()

    # Output message if apps provided, but they only contain one symptom checker
    if (n_groups == 1) {
      message("Only one app provided, calculating accuracy for this app")
    }

    # If advices triage level is of lower urgency (higher number) than solution, code advice as unsafe. Otherwise (same or higher urgency), code as safe
    data <- data %>%
      rowwise() %>%
      mutate(safety = case_when(
        !!triagelevel_advice_sym > !!triagelevel_correct_sym ~ "unsafe advice",
        TRUE ~ "safe advice")) %>%
      ungroup()

    # Obtain raw numbers on safety
    raw_numbers <- data %>%
      group_by(!!apps_sym, safety) %>%
      count() %>%
      ungroup()

    # Obtain percentage of safe advice
    percentage <- data %>%
      group_by(!!apps_sym, safety) %>%
      count() %>%
      ungroup() %>%
      group_by(!!apps_sym) %>%
      summarise(safety_percentage = sum(n[safety == "safe advice"]) / sum(n) * 100) %>%
      ungroup()


  } else {
    # If advices triage level is of lower urgency (higher number) than solution, code advice as unsafe. Otherwise (same or higher urgency), code as safe
    data <- data %>%
      rowwise() %>%
      mutate(safety = case_when(
        !!triagelevel_advice_sym > !!triagelevel_correct_sym ~ "unsafe advice",
        TRUE ~ "safe advice")) %>%
      ungroup()

    # Obtain raw numbers on safety
    raw_numbers <- data %>%
      group_by(safety) %>%
      count()

    # Obtain percentage of safe advice
    percentage <- data %>%
      group_by(safety) %>%
      count() %>%
      ungroup() %>%
      summarise(safety_percentage = sum(n[safety == "safe advice"]) / sum(n) * 100)
  }
  # Output raw numbers and percentage as a list
  output_combination <- list(raw_numbers = raw_numbers, percentage = percentage)
  return(output_combination)
}
