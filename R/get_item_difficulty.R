#' @title get_item_difficulty
#'
#' @description Calculates the item difficulty for each case / vignette
#'
#' @param data A dataframe
#' @param correct A string indicating the column name storing if the symptom checker solved the case (TRUE or FALSE)
#' @param vignettes A string indicating the column name storing the vignette or vignette number
#'
#' @return A data frame object containing the item difficulty for each vignette
#' @examples
#' data(symptomcheckRdata)
#' item_difficulty <- get_item_difficulty(
#'   data = symptomcheckRdata,
#'   correct = "Correct_Triage_Advice_provided_from_app",
#'   vignettes = "Vignette_id"
#'   )

#' @export
#' @import dplyr
#'
get_item_difficulty <- function(data, correct, vignettes) {
  item_difficulty <- NULL
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

  # Get number of observations for vignettes
  number_vignette_observations <- data %>% group_by(!!vignettes_sym) %>% count() %>% ungroup() %>%  pull(n)

  # If a vignette has one or no observations, the item difficulty cannot be calculated. Stop and output warning
  if (any(number_vignette_observations < 2) | any(is.na(number_vignette_observations))) {
    stop("At least one vignette has only one or no observations. Item Difficulty cannot be calculated with only one observation. Please delete this vignette and try again.")
  } else { #Calculate item difficulty
    output <- data %>%
      group_by(!!vignettes_sym) %>%
      summarise(item_difficulty = mean(!!correct_sym, na.rm = TRUE))%>%
      setNames(c("vignettes", "item_difficulty")) %>%
      select("vignettes", item_difficulty) %>%
      arrange("vignettes")
  }
  return(output)
}
