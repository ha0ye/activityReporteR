#' @export
in_period <- function(dat, start_date, end_date, date_cols = "date")
{
    dat %>%
        dplyr::select(dplyr::all_of(date_cols)) %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(date_cols), check_date_col,
                                    date_start = start_date, date_end = end_date)) %>%
        apply(1, any)
}

#' @export
check_date_col <- function(date_col, date_start, date_end)
{
    # check all dates in the date column (in case of range)
    m <- gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", date_col)
    all_dates <- regmatches(date_col, m)
    purrr::map(all_dates, ~ .x >= date_start &
                   .x <= date_end) %>%
        purrr::map_lgl(any)
}
