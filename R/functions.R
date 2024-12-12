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
    recipes::update_role({{ metabolite_variable }},
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
create_model_workflow <- function(model_specs,
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
tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model %>%
    workflows::extract_fit_parsnip(workflow_fitted_model) %>%
    broom::tidy(exponentiate = T)
}
#' Split dataset by metabolites
#'
#' @param data
#'
#' @return list of data frames
split_by_metabolite <- function(data) {
  data %>%
    column_values_to_snakecase(metabolite) %>%
    dplyr::group_split(metabolite) %>%
    purrr::map(metabolites_to_wider)
}
#' Generate results
#'
#' @param data
#'
#' @return
generate_model_results <- function(data) {
  create_model_workflow(
    parsnip::logistic_reg() %>%
      parsnip::set_engine("glm"),
    data %>%
      create_recipe_spec(tidyselect::starts_with("metabolite_"))
  ) %>%
    parsnip::fit(data) %>%
    tidy_model_output()
}
#' Calculate estimates
#'
#' @param data
#'
#' @return a data frame
calculate_estimates <- function(data) {
  model_estimates <- data %>%
    split_by_metabolite() %>%
    purrr::map(generate_model_results) %>%
    purrr::list_rbind() %>%
    dplyr::filter(stringr::str_detect(term, "metabolite_"))
  data %>%
    dplyr::select(metabolite) %>%
    dplyr::mutate(term = metabolite) %>%
    column_values_to_snakecase(term) %>%
    dplyr::mutate(term = stringr::str_c("metabolite_", term)) %>%
    unique() %>%
    dplyr::right_join(model_estimates, by = "term")
}
#' Visualize odds-ratios
#'
#' @param data
#'
#' @return ggplot object
plot_estimates <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(
    x = estimate,
    y = metabolite,
    xmin = estimate - std.error,
    xmax = estimate + std.error
  )) +
    ggplot2::geom_pointrange() +
    ggplot2::coord_fixed(xlim = c(0, 5)) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Odds-ratio") +
    ggplot2::ylab("Metabolites")
}
