#' @title get_ccs_by_triage
#'
#' @description Calculates the Capability Comparison Score (CCS) for each symptom checker and each triage level
#'
#' @param data A dataframe
#' @param correct A string indicating the column name storing if the symptom checker solved the case (TRUE or FALSE)
#' @param vignettes A string indicating the column name storing the vignette or vignette number
#' @param apps A string indicating the column name storing the app names
#' @param triagelevel A string indicating the column name storing the correct triage solutions
#'
#' @return A data frame object containing the capability comparison score for each symptom checker on each triage level.
#' @examples
#' data(symptomcheckRdata)
#' ccs <- get_ccs_by_triage(
#'   data = symptomcheckRdata,
#'   correct = "Correct_Triage_Advice_provided_from_app",
#'   vignettes = "Vignette_id",
#'   apps = "App_name",
#'   triagelevel = "Goldstandard_solution"
#'   )

#' @export
#' @import dplyr
#' @importFrom stats setNames

get_ccs_by_triage <- function(data, correct, vignettes, apps, triagelevel) {
  item_difficulty <- NULL
  ccs <- NULL
  # Code input for handling with dplyr
  correct_sym <- sym(correct)
  vignettes_sym <- sym(vignettes)

  # Handle errors
  if (!is.data.frame(data)) {
    stop("The first argument must be a data frame.")
  }

  if (!(correct %in% names(data))) {
    stop("The second argument must be a valid column name (as a string) from the data frame indicating if the case was solved correctly.")
  }

  if (!all(data[[correct]] %in% c(TRUE, FALSE, NA))) {
    stop("The correct column must contain only Boolean values (TRUE or FALSE).")
  }

  if (!(vignettes %in% names(data))) {
    stop("The third argument must be a valid column name (as a string) from the data frame referencing each vignette.")
  }

  if (!(triagelevel %in% names(data))) {
    stop("The third argument must be a valid column name (as a string) from the data frame indicating the correct triage level for the case.")
  }

  if (!is.null(apps)) {
    if (!is.character(apps) || !(apps %in% names(data))) {
      stop("The fourth argument must be a valid column name (as a string) from the data frame indicating the apps in the data frame.")
    }

    # Code input for handling with dplyr
    apps_sym <- sym(apps)
    triage_sym <- sym(triagelevel)
    # Get number of symptom checkers
    n_groups <- data %>% count(!!apps_sym) %>% nrow()

    # Stop if only one app is provided (as item difficulty cannot be calculated). Output a message if less than 4 apps are provided
    if (n_groups < 2) {
      stop("CCS cannot be calculated with only one app.")
    }
    if (n_groups < 4) {
      message("Less than 4 apps provided. CCS might not be reliable.")
    }

    # Calculate item difficulty, join it to the dataset and calculate the CCS from there
    output <- data %>%
      group_by(!!vignettes_sym) %>%
      summarise(item_difficulty = mean(!!correct_sym, na.rm = TRUE))%>%
      setNames(c(vignettes, "item_difficulty")) %>%
      select(!!vignettes_sym, item_difficulty) %>%
      full_join(data, by = vignettes) %>% #joining ID and raw data
      filter(!is.na(!!correct_sym)) %>%
      group_by(!!triage_sym, !!apps_sym) %>%
      summarise(ccs = ((((sum(!!correct_sym*(1-item_difficulty)) - sum((1-!!correct_sym)*(item_difficulty))) / length(item_difficulty)) + 1) / 2)*100) %>%
      arrange(desc(ccs))
  } else {
    # If no app is provided, output error message
    stop("CCS cannot be calculated with only one app.")
  }
  return(output)
}
