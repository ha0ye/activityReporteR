#' @export
format_research <- function(df, type = "paper", my_bib)
{
    if (!is.null(type))
    {
        to_format <- df %>%
            dplyr::filter(type == {{type}})
    } else {
        to_format <- df
    }
    format_fun <- switch(type,
                         "paper" = format_research_paper,
                         "other" = format_research_other)

    if (NROW(to_format) == 0)
    {
        return(NULL)
    }

    format_fun(to_format, my_bib)
}

#' @export
format_research_paper <- function(to_format, my_bib)
{
    to_format %>%
        dplyr::mutate(status_line = ifelse(.data$status == "published", "", glue(" [**{status}**]")),
               to_print = purrr::map_chr(.data$bib_id, ~ format_ref(my_bib[.x])),
               to_print = glue("{to_print}{status_line}\n", .trim = FALSE)) %>%
        dplyr::select(.data$date, .data$to_print)
}

#' @export
format_research_other <- function(to_format, my_bib, ...)
{
    to_format %>%
        dplyr::mutate(to_print = purrr::map_chr(.data$bib_id, ~ format_ref(my_bib[.x])),
                      to_print = glue("{to_print}\n", .trim = FALSE)) %>%
        dplyr::select(.data$date, .data$to_print, ...)
}

#' @export
format_ref <- function(ref)
{
    to_print <- utils::capture.output(print(ref)) %>%
        paste(collapse = " ")
}


