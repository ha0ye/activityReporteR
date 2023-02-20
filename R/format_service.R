#' @export
format_service <- function(df, locale = NULL, group = FALSE)
{
    if (!is.null(locale))
    {
        df <- filter(df, locale == {{locale}})
    }

    if (NROW(df) == 0)
    {
        return(NULL)
    }

    to_format <- df %>%
        dplyr::mutate(date = lubridate::parse_date_time(.data$date_start, "my"),
                      role_print = replace_na(role, ""),
                      date_end = replace_na(date_end, "present"),
                      date_print = ifelse(date_start == date_end,
                                          date_start,
                                          paste0(date_start, " - ", date_end)),
                      role_info = glue::glue("{role_print}, {date_print}")) %>%
        dplyr::arrange(dplyr::desc(.data$date))


    if (group)
    {
        to_format <- to_format %>%
            group_by(name) %>%
            summarize(name = first(name),
                      role_info = paste(role_info, collapse = "; "),
                      date = first(date))
    }

    to_format %>%
        dplyr::mutate(to_print = glue::glue("{name}, {role_info}", .na = "")) %>%
        dplyr::select(date, to_print)
}
