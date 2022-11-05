#' @export
format_training <- function(df, type = "uf-training")
{
    to_format <- df %>%
        dplyr::filter(type == {{type}})

    format_fun <- switch(type,
                         "uf-training" = format_training_standard,
                         "library-training" = format_training_standard,
                         "other" = format_training_other)

    if (NROW(to_format) == 0)
    {
        return(NULL)
    }

    format_fun(to_format)
}

#' @export
format_training_standard <- function(df)
{
    df %>%
        dplyr::mutate(to_print = glue("{title}, {date}")) %>%
        dplyr::select(date, .data$to_print)
}

#' @export
format_training_other <- function(df)
{
    df %>%
        dplyr::mutate(to_print = glue("({source}) {title}, {date}")) %>%
        dplyr::select(date, .data$to_print)
}
