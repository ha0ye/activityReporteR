#' @export
print_data <- function(df, empty_message = "(none)\n")
{
    if (is.null(df) || NROW(df) == 0)
    {
        cat(empty_message)
        return(invisible())
    }

    df %>%
        dplyr::arrange(dplyr::desc(.data$date)) %>%
        dplyr::pull(.data$to_print) %>%
        cat(sep = "\n")
}

#' @export
format_author <- function(df, pattern = "H\\. Ye", replacement = "**H\\. Ye**")
{
    dplyr::mutate(df, to_print = sub({{pattern}}, {{replacement}}, .data$to_print))
}
