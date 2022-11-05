#' @export
format_service <- function(df, locale = NULL)
{
    if (!is.null(locale))
    {
        df <- filter(df, locale == {{locale}})
    }

    to_format <- df %>%
        dplyr::mutate(date = lubridate::parse_date_time(.data$date_start, "my"),
               role_print = ifelse(is.na(.data$role), "", paste(",", .data$role))) %>%
        dplyr::arrange(dplyr::desc(.data$date))

    if (NROW(to_format) == 0)
    {
        return(NULL)
    }

    to_format %>%
        dplyr::mutate(to_print = glue("{name}{role_print}, {date_start} - {date_end}", .na = "")) %>%
        dplyr::select(.data$date, .data$to_print)
}
