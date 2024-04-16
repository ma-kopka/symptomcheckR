#' @title get_irr
#'
#' @description Calculates the inter-rater reliability of multiple raters using a two-way, absolute agreement, average-measures, mixed intra-class correlation
#'
#' @param data A dataframe
#' @param ratings A vector indicating the column names storing the ratings of reach rater
#' @param order_triagelevel A vector indicating the order of triage levels. The triage level with highest urgency should be the first value and the triage level with lowest urgency the last value.
#'
#' @return A list containing the inter-rater reliability
#' @examples
#' \dontrun{
#' #' irr <- get_irr(
#'   data = df,
#'   ratings = c("datarater1", "datarater2", "datarater3"),
#'   order_triagelevel = c("Emergency", "Non-Emergency", "Self-care"),
#'   )
#'   }

#' @export
#' @import dplyr
#' @import irr
#' @importFrom stats setNames
#' @importFrom stats na.omit

get_irr <- function(data, ratings, order_triagelevel) {
  safety <- NULL
  # Handle errors
  if (!is.data.frame(data)) {
    stop("The first argument must be a data frame.")
  }


  if (!is.character(ratings) || !all(ratings %in% names(data))) {
    stop("The second argument must be valid column names (as a character vector) from the data frame indicating the columns in which each raters' data is stored.")
  }

  if (length(ratings) < 2) {
    stop("The 'ratings' argument must contain more than one column.")
  }


  if (!is.vector(order_triagelevel) || is.list(order_triagelevel)) {
    stop("order_triagelevel must be a vector.")
  }

  # Recode NAs to the lowest category
  if (any(sapply(data[, ratings, drop = FALSE], function(x) any(is.na(x))))) {
    levels_with_na <- c("NA", order_triagelevel)
    labels_with_na <- 0:length(order_triagelevel)
  } else {
    levels_with_na <- order_triagelevel
    labels_with_na <- 1:length(order_triagelevel)
  }

  rating_data <- data[, ratings]
  invalid_data <- lapply(rating_data, function(col) {
    if (any(!col %in% c(order_triagelevel, NA))) {
      return(names(rating_data)[!col %in% c(order_triagelevel, NA)])
    } else {
      return(NULL)
    }
  })

  invalid_data <- Filter(Negate(is.null), invalid_data)
  if (length(invalid_data) > 0) {
    error_message <- sapply(names(invalid_data), function(name) {
      sprintf("Column '%s' contains values not in 'order_triagelevel' or NA.", name)
    })
    stop(paste(error_message, collapse = " "))
  }


  # Create a numeric representation of the ratings
  data_processed <- data %>%
    mutate(across(all_of(ratings),
                  ~as.numeric(factor(., levels = levels_with_na, labels = labels_with_na)),
                  .names = "rater{.col}_numeric"))


  # Select only the numeric rater columns
  numeric_columns <- grep("rater.*_numeric", names(data_processed), value = TRUE)
  data_for_icc <- select(data_processed, all_of(numeric_columns))

  # Calculate the inter-rater reliability
  icc_result <- icc(data_for_icc, model = "twoway", type = "agreement", unit = "average")
  return(icc_result)
}
