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
                q25 = ~ quantile(.x, probs = 1/4),
                q75 = ~ quantile(.x, probs = 3/4)
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
