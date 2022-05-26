#' @export
format_conferences <- function(df)
{
    df %>%
        dplyr::mutate(
            date_line = ifelse(.data$date_start == .data$date_end, .data$date_start, glue("{date_start}---{date_end}")),
            date = .data$date_end,
            to_print = glue("* ({source}) {title}, {date_line}")) %>%
        dplyr::select(.data$date, .data$to_print)
}
