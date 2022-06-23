
#' Summarize a vector
#'
#'@description
#'`all_summary()` provide a vector of summary statistics including.
#'
#' * Minimum
#' * Median
#' * Mean
#' * Maximum
#' * Standard deviation
#' * Inter quarterly range
#' * Sample size
#'
#' @param x A numeric vector
#'
#' @return
#' A list of summary statistics.
#'
#' @seealso [summary()] for base summary statistics.
#'
#' @export
#'
#' @examples
#' all_summary(iris$Sepal.Length)
#'
all_summary <- function(x) {
  c(
    Min = min(x),
    Median = stats::median(x),
    Mean = mean(x),
    Max = max(x),
    SD = stats::sd(x),
    Iqr = stats::IQR(x),
    N = length(x)
  )
}


#' Summary statistics for a dataframe
#'
#' @description
#' `sum_tab()` create a data frame of summary statistics for all numeric column in a
#'   dataframe. For grouping data, `sum_tab()` create a list of summary statistics
#'   dataframe for each of the group.
#'
#' @param .data A dataframe
#' @param .class Column name for group wise summary statistics. Default is  `NULL`.
#'
#' @return A dataframe or a list of dataframe of summary statistics.
#' @export
#'
#' @examples
#' sum_tab(iris, Species)
#'
sum_tab <- function(.data, .class) {
  lab <- rlang::enquo(.class)
  if(is.null(lab)) {
    .data <- .data %>%
      dplyr::select_if(is.numeric)
    sum_res <- apply(.data, 2, all_summary)
  }else {
    .data <- .data %>%
      dplyr::mutate_if(is.character, as.factor)
    sum_res <- .data %>%
      dplyr::group_by(!!lab) %>%
      dplyr::select_if(is.numeric) %>%
      dplyr::group_map(~apply(.x, 2, all_summary))
    names(sum_res) <- levels(.data[,rlang::as_label(lab)])
  }
  return(sum_res)
}

