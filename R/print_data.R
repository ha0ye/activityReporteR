#' @export
print_data <- function(df, empty_message = "(none)\n", prefix = "* ", sep = "\n")
{
    if (is.null(df) || NROW(df) == 0)
    {
        cat(empty_message)
        return(invisible())
    }

    df %>%
        dplyr::arrange(dplyr::desc(.data$date)) %>%
        dplyr::mutate(to_print = glue("{prefix}{.data$to_print}")) %>%
        dplyr::pull(.data$to_print) %>%
        cat(sep = sep)
}

#' @export
format_author <- function(df,
                          replacement = c("H\\. Ye" = "**H\\. Ye**",
                                          "Ye, H\\." = "**Ye, H\\.**"))
{
    dplyr::mutate(df, to_print =
                      stringr::str_replace_all(.data$to_print,
                                               {{replacement}}))
}
