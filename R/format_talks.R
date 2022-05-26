#' @export
format_talks <- function(df)
{
    df %>%
        dplyr::mutate(notes_line = ifelse(is.na(.data$other_notes), "", glue("  \n{other_notes}", .trim = FALSE)),
                      to_print = glue('"{title}", {type}, {event_info}, ({format}), {date}{notes_line}\n', .trim = FALSE)) %>%
        dplyr::select(.data$date, .data$to_print)
}

