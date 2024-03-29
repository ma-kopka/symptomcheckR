% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_comprehensiveness.R
\name{plot_comprehensiveness}
\alias{plot_comprehensiveness}
\title{plot_comprehensiveness}
\usage{
plot_comprehensiveness(input)
}
\arguments{
\item{input}{A dataframe containing the output of get_comprehensiveness()}
}
\value{
A ggplot object visualizing the comprehensiveness for either one or multiple symptom checkers
}
\description{
Plots the comprehensiveness for one or multiple symptom checkers
}
\examples{
data(symptomcheckRdata)
comprehensiveness <- get_comprehensiveness(
  data = symptomcheckRdata,
  triagelevel_advice = "Triage_advice_from_app",
  vector_not_entered = c(NA),
  apps = "App_name"
  )
comprehensiveness_plot <- plot_comprehensiveness(comprehensiveness)
}
