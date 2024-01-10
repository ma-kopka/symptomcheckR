#' @title plot_performance_single
#'
#' @description Plots all performance metrics for one symptom checker
#'
#' @param data A dataframe
#' @param triagelevel_correct A string indicating the column name storing the correct triage solutions
#' @param triagelevel_advice A string indicating the column name storing the recommendation of a symptom checker for a case
#' @param order_triagelevel A vector indicating the order of triage levels. The triage level with highest urgency should be the first value and the triage level with lowest urgency the last value.
#' @param vector_not_entered A vector indicating the values in which missing values are coded (e.g., as NA or a specified value such as -99)
#'
#' @return A ggplot object visualizing all performance metrics for one symptom checker
#' @examples
#' data(symptomcheckRdata)
#' \dontrun{
#' performance_plot <- plot_performance_single(
#'   data = symptomcheckRdata,
#'   triagelevel_correct = "Goldstandard_solution",
#'   triagelevel_advice = "Triage_advice_from_app",
#'   order_triagelevel = c("Emergency", "Non-Emergency", "Self-care"),
#'   vector_not_entered = c(NA)
#'   )
#' }
#'
#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import ggpubr
#' @importFrom stats setNames
#' @importFrom stats na.omit


plot_performance_single <- function(data, triagelevel_correct, triagelevel_advice, order_triagelevel, vector_not_entered) {
  . <- NULL
  solution <- NULL
  # Split dataset for different metrics (assign input dataset to new variable)
  data_first <- data

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

  if (!is.vector(vector_not_entered) || is.list(vector_not_entered)) {
    stop("vector_not_entered must be a vector indicating all values that indicate that a case was not able to be entered (e.g. NA).")
  }

  if (!is.vector(order_triagelevel) || is.list(order_triagelevel)) {
    stop("order_triagelevel must be a vector.")
  }

  if (any(is.na(data[[triagelevel_correct]]))) {
    data_withoutna <- na.omit(data, cols = triagelevel_correct)
    unique_triagelevels_correct <- unique(data_withoutna[[triagelevel_correct]])
  } else {
    unique_triagelevels_correct <- unique(data[[triagelevel_correct]])
  }

  data_withoutna <- data
  if (any(is.na(data[[triagelevel_correct]]))) {
    data_withoutna <- na.omit(data_withoutna, cols = triagelevel_correct)
  }
  unique_triagelevels_correct <- unique(data_withoutna[[triagelevel_correct]])

  if (any(is.na(data_withoutna[[triagelevel_advice]]))) {
    data_withoutna <- na.omit(data_withoutna, cols = triagelevel_advice)
  }
  unique_triagelevel_advice <- unique(data_withoutna[[triagelevel_advice]])

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

  # Get new column, check if advice triage level is equal to solution triage level. Using this column, get accuracy using get_accuracy() and plot it
  data <- data %>%
    mutate(correct = case_when(
      .data[[triagelevel_correct]] == .data[[triagelevel_advice]] ~ TRUE,
      TRUE ~ FALSE))

  # Get accuracy using get_accuracy() and plot it
  accuracy_plot <- get_accuracy(data = data, correct = "correct", apps = NULL) %>%
    plot_accuracy()

  # Get accuracy by triage level using get_accuracy_by_triage() and plot it
  accuracy_by_triage_plot <- get_accuracy_by_triage(data = data, correct = "correct", triagelevel = triagelevel_correct, apps = NULL) %>%
    rename(solution = names(.)[1]) %>%
    mutate(solution = names(recode_vector)[match(solution, recode_vector)]) %>%
    plot_accuracy_by_triage()

  # Get safety and plot it
  safety_plot <- get_safety_of_advice(data = data_first, triagelevel_correct = triagelevel_correct, triagelevel_advice = triagelevel_advice, order_triagelevel = order_triagelevel, apps = NULL) %>%
    plot_safety_of_advice()

  # Get comprehensiveness and plot it
  comprehensiveness_plot <- get_comprehensiveness(data = data_first, triagelevel_advice = triagelevel_advice, vector_not_entered = vector_not_entered, apps = NULL) %>%
    plot_comprehensiveness()

  # Get inclination to overtriage and plot it
  overtriage_plot <- get_inclination_to_overtriage(data = data_first, triagelevel_correct = triagelevel_correct, triagelevel_advice = triagelevel_advice, order_triagelevel = order_triagelevel, apps = NULL) %>%
    plot_inclination_to_overtriage()

  # Combine first row of plots (accuracy and accuracy by triage)
  combined_plot_1 <- ggarrange(
    accuracy_plot + theme(legend.position = "bottom", axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) + labs(title = "Accuracy"),
    accuracy_by_triage_plot + theme(legend.position = "bottom", axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) + labs(title = "Accuracy by Triage Level"),
    ncol = 2,
    nrow = 1,
    widths = c(0.7, 1.4)
  )

  # Combine second row of plots (safety, comprehensiveness and overtriage)
  combined_plot_2 <- ggarrange(
    safety_plot + theme(legend.position = "bottom", axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) + labs(title = "Safety of Advice"),
    comprehensiveness_plot + theme(legend.position = "bottom", axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) + labs(title = "Comprehensiveness"),
    overtriage_plot + theme(legend.position = "bottom", axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) + labs(title = "Inclination to Overtriage"),
    ncol = 3,
    nrow = 1,
    widths = c(1, 1, 1)
  )

  # Combine both plot combinations and save in one big plot
  combined_plot <- ggarrange(
    combined_plot_1, combined_plot_2,
    nrow = 2
  )
  return(combined_plot)
}
