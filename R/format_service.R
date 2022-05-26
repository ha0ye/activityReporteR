#' @export
format_service <- function(df, locale = "library")
{
    to_format <- df %>%
        dplyr::mutate(date = lubridate::parse_date_time(.data$start_time, "my"),
               role_print = ifelse(is.na(.data$role), "", paste(",", .data$role))) %>%
        dplyr::filter(locale == {{locale}}) %>%
        dplyr::arrange(dplyr::desc(.data$date))

    if (NROW(to_format) == 0)
    {
        return(NULL)
    }

    to_format %>%
        dplyr::mutate(to_print = glue("* {name}{role_print}, {start_time} - {end_time}", .na = "")) %>%
        dplyr::select(.data$date, .data$to_print)
}
