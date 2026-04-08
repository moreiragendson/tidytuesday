#' Rescale a numeric column to human-readable values with a suffix
#'
#' Adds two new columns to a data frame: one with the rescaled numeric value
#' and one with the corresponding suffix (`"K"`, `"M"`, `"B"`, `"T"`, or
#' `""`). Supports dplyr-style renaming (`new_name = old_name`) to control
#' the output column names.
#'
#' @param data A data frame.
#' @param ... A single column specification, optionally renamed. Use bare
#'   column names as in dplyr (e.g. `value` or `new_value = value`).
#'   Exactly one column must be provided.
#'
#' @return The input data frame with two additional columns:
#'   \describe{
#'     \item{`<name>`}{Rescaled numeric value.}
#'     \item{`<name>_suffix`}{Character suffix indicating the scale:
#'       `"T"` (>= 1e12), `"B"` (>= 1e9), `"M"` (>= 1e6), `"K"` (>= 1e3),
#'       or `""` for values below 1,000.}
#'   }
#'   The original column is preserved. The output column name defaults to the
#'   source column name unless a rename is provided.
#'
#' @examples
#' df <- tibble::tibble(gdp = c(420, 1.5e3, 8e6, 3.2e9, 1.1e12))
#'
#' # Use source column name as output prefix
#' rescale_number(df, gdp)
#'
#' # Rename output columns
#' rescale_number(df, gdp_fmt = gdp)
#'
#' @importFrom rlang enquos as_name
#' @importFrom dplyr pull mutate case_when
#'
#' @export
rescale_number <- function(data, ...) {
  cols <- rlang::enquos(...)

  if (length(cols) != 1) {
    stop("`rescale_number()` requires exactly one column.", call. = FALSE)
  }

  col_quo  <- cols[[1]]
  col_name <- rlang::as_name(col_quo)

  out_name   <- names(cols)
  if (is.null(out_name) || nchar(out_name) == 0) out_name <- col_name

  value_col  <- out_name
  suffix_col <- paste0(out_name, "_suffix")

  raw <- dplyr::pull(data, !!col_quo)

  data |>
    dplyr::mutate(
      !!value_col := dplyr::case_when(
        raw >= 1e12 ~ raw / 1e12,
        raw >= 1e9  ~ raw / 1e9,
        raw >= 1e6  ~ raw / 1e6,
        raw >= 1e3  ~ raw / 1e3,
        TRUE        ~ raw
      ),
      !!suffix_col := dplyr::case_when(
        raw >= 1e12 ~ "T",
        raw >= 1e9  ~ "B",
        raw >= 1e6  ~ "M",
        raw >= 1e3  ~ "K",
        TRUE        ~ ""
      )
    )
}