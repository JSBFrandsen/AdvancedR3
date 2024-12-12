#' Descriptive stats
#'
#' @param data
#'
#' @return A data frame/tibble
descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(
      c(value),
      list(
        mean = mean,
        sd = sd,
        iqr = IQR,
        q25 = ~ quantile(.x, probs = 1 / 4),
        q75 = ~ quantile(.x, probs = 3 / 4)
      )
    )) %>%
    dplyr::mutate(dplyr::across(
      where(is.numeric),
      ~ round(.x, digits = 1)
    ))
}
#' Plot distributions of value
#'
#' @param data
#'
#' @return a plot figure
plot_distributions <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}
#' Columns to snakecase
#'
#' @param data
#' @param columns
#'
#' @return altered names for specific columns
column_values_to_snakecase <- function(data, columns) {
  data %>%
    dplyr::mutate(dplyr::across({{ columns }}, snakecase::to_snake_case))
}
#' Make the dataset wide format by metabolite
#'
#' @param data
#'
#' @return a wide format dataset
metabolites_to_wider <- function(data) {
  data %>% tidyr::pivot_wider(
    names_from = metabolite,
    values_from = value,
    values_fn = mean,
    names_prefix = "metabolite_"
  )
}
#' Create recipe specifications
#'
#' @param data
#' @param metabolite_variable
#'
#' @return specification
create_recipe_spec <- function(data, metabolite_variable) {
  recipes::recipe(data) %>%
    recipes::update_role({{metabolite_variable}},
      age,
      gender,
      new_role = "predictor"
    ) %>%
    recipes::update_role(class,
      new_role = "outcome"
    ) %>%
    recipes::step_normalize(rsample::starts_with("metabolite_"))
}
#' Create model workflow
#'
#' @param model_specs
#' @param recipe_specs
#'
#' @return workflow
create_model_workflow = function(model_specs,
                                 recipe_specs) {
    workflows::workflow() %>%
        workflows::add_model(model_specs) %>%
        workflows::add_recipe(recipe_specs)
}
#' Tidy model output
#'
#' @param workflow_fitted_model
#'
#' @return data frame
tidy_model_output = function(workflow_fitted_model) {
    workflow_fitted_model %>%
        workflows::extract_fit_parsnip(workflow_fitted_model) %>%
        broom::tidy(exponentiate = T)
}
