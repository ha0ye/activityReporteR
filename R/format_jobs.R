#' @export
format_jobs <- function(df)
{
    to_format <- df %>%
        dplyr::mutate(date = lubridate::parse_date_time(.data$date_start, "my"),
                      date_end = tidyr::replace_na(date_end, "present"),
                      Dates = glue::glue("{date_start} - {date_end}")) %>%
        dplyr::arrange(dplyr::desc(.data$date)) %>%
        dplyr::select(Institution, Position, Dates)
}
