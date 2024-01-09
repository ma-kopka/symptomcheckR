---
output: github_document
---

## `symptomcheckR` package

### R package for easy analysis and visualization of symptom checker performance metrics

<!-- badges: start -->

<!-- badges: end -->

The `symptomcheckR` package can be used to analyze the performance of symptom checkers across various metrics. Since many studies report different metrics, we aimed to standardize performance reporting by developing more reliable metrics that future studies should include. They can be found in [this study](https://doi.org/10.1177/20552076231194929). To make it easier for researchers and other stakeholders to report these metrics and compare different symptom checkers, we developed this package.

### Installation

```{r eval=FALSE}
remotes::install_github("ma-kopka/symptomcheckR")
```

### Usage

First, load the package:

```{r eval=FALSE}
library("symptomcheckR")
```

Now you can load the data set to test the commands:

```{r eval=FALSE}
data(symptomcheckRdata)
```

This will load the data set. You can run all commands from this package now. These include the following metrics:

Accuracy:

```{r eval=FALSE}
accuracy_value <- get_accuracy(symptomcheckRdata, 
                               correct = "Correct_Triage_Advice_provided_from_app", 
                               apps = "App_name")
plot_accuracy(accuracy_value)
```

Accuracy by triage level:

```{r eval=FALSE}
get_accuracy_by_triage(
  symptomcheckRdata,
  correct = "Correct_Triage_Advice_provided_from_app", 
  triagelevel = "Goldstandard_solution",
  apps = "App_name")
plot_accuracy_by_triage(accuracy_value_by_triage)
```

Safety of advice:

```{r eval=FALSE}
safety <- get_safety_of_advice(
  data = symptomcheckRdata, 
  triagelevel_correct = "Goldstandard_solution",
  triagelevel_advice = "Triage_advice_from_app",
  order_triagelevel = c("Emergency", "Non-Emergency", "Self-care"),
  apps = "App_name")
plot_safety_of_advice(safety)
```

Comprehensiveness:

```{r eval=FALSE}
comprehensiveness <- get_comprehensiveness(
  data = symptomcheckRdata, 
  triagelevel_advice = "Triage_advice_from_app", 
  vector_not_entered = c(NA),
  apps = "App_name")
plot_comprehensiveness(comprehensiveness)

```

Inclination to overtriage:

```{r eval=FALSE}
inclination_to_overtriage <- get_inclination_overtriage(
  data = symptomcheckRdata, 
  triagelevel_correct = "Goldstandard_solution",
  triagelevel_advice = "Triage_advice_from_app",
  order_triagelevel = c("Emergency", "Non-Emergency", "Self-care"),
  apps = "App_name")
plot_inclination_overtriage(inclination_to_overtriage)

```

Item Difficulty of each vignette:

```{r eval=FALSE}
get_item_difficulty(
  data = symptomcheckRdata, 
  correct = "Correct_Triage_Advice_provided_from_app",
  vignettes = "Vignette_id")

```

Capability Comparison Score:

```{r eval=FALSE}
ccs <- get_ccs(
  data = symptomcheckRdata,
  correct = "Correct_Triage_Advice_provided_from_app",
  vignettes = "Vignette_id",
  apps = "App_name")
plot_ccs(ccs)
```

Capability Comparison Score on each triage level:

```{r eval=FALSE}
get_ccs_by_triage <- get_ccs_by_triage(
  data = symptomcheckRdata,
  correct = "Correct_Triage_Advice_provided_from_app",
  vignettes = "Vignette_id",
  apps = "App_name",
  triage = "Goldstandard_solution")
```

Users can also get a performance overview for one or multple symptom checkers. To get the overview (and publication-ready figures) for a single symptom checker, you can use the following command:

```{r eval=FALSE}
df_individual <- symptomcheckRdata %>%
  filter(App_name == "Ask NHS")

plot_performance_single(
  data = df_individual, 
  triagelevel_correct = "Goldstandard_solution",
  triagelevel_advice = "Triage_advice_from_app",
  order_triagelevel = c("Emergency", "Non-Emergency", "Self-care"),
  vector_not_entered = c(NA)) 
```

To compare multiple symptom checkers (and produce publication-ready figures), you can use the following command:

```{r eval=FALSE}
plot_performance_multiple(
  data = symptomcheckRdata, 
  triagelevel_correct = "Goldstandard_solution",
  triagelevel_advice = "Triage_advice_from_app",
  order_triagelevel = c("Emergency", "Non-Emergency", "Self-care"),
  vector_not_entered = c(NA),
  vignettes = "Vignette_id",
  apps = "App_name")
```
