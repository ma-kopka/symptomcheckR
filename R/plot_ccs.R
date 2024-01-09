#' @title plot_ccs
#'
#' @description Plots the Capability Comparison Score (CCS) for multiple symptom checkers
#'
#' @param input A dataframe containing the output of get_ccs()
#'
#' @return A ggplot object visualizing the CCS for multiple symptom checkers
#' @examples
#' data(symptomcheckRdata)
#' ccs <- get_ccs(
#'   data = symptomcheckRdata,
#'   correct = "Correct_Triage_Advice_provided_from_app",
#'   vignettes = "Vignette_id",
#'   apps = "App_name"
#'   )
#' ccs_plot <- plot_ccs(ccs)

#' @export
#' @import dplyr
#' @import tidyr
#' @import ggplot2

plot_ccs <- function(input) {
  ccs <- NULL
  posneg <- NULL
  app <- NULL
  ccs_sizeadjusted <- NULL
  # Handle errors
  if(!is.data.frame(input)) {
    stop("The input is not a dataframe. Please make sure to run the get_ccs() function first and use the result as input.")
  }

  if (ncol(input) != 2) {
    stop("The input dataframe must be the result of using get_ccs(). You must not use any other input.")
  }

  if (!"ccs" %in% names(input)) {
    stop("The 'ccs' variable is not found in the input. Please ensure you obtain the accuracy using get_ccs() function first and use the result as input.")
  }

  # Rename column for easier handling
  names(input)[1] <- "app"

  # Use input and ggplot2 to visualize the CCS
  output <- input %>%
    mutate(ccs_sizeadjusted = ccs-50,
           posneg = case_when(
             ccs < 50 ~ "negative",
             TRUE ~ "positive"
           ),
           posneg = factor(posneg, levels = c("positive", "negative"))) %>%
    ggplot(aes(x = app, y = ccs_sizeadjusted, fill = posneg)) +
    geom_bar(stat = "identity", aes(fill = posneg)) +
    scale_fill_manual(values =
                        c("negative" = "#b51963", "positive" = "#5ba300"),
                      name = element_blank(),  # Renaming the legend title
                      labels = c("Better Than Average", "Worse Than Average")
    ) +
    scale_y_continuous(labels = function(x) x + 50) +
    geom_hline(yintercept = 0, color = "black") +  # Add a line at the new "zero"
    theme_light() +
    labs(x = "", y = "Capability Comparison Score") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(color = "black"))

  return(output)
}
