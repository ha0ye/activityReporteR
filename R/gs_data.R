#' @export
get_service_data <- function(url,
                             report_start_date = Sys.Date() - lubridate::years(1),
                             report_end_date = Sys.Date())
{
    googlesheets4::read_sheet(url, sheet = "service", col_types = "c") %>%
        dplyr::filter(is.na(.data$end_time) |
                          lubridate::parse_date_time(.data$end_time, "my") >= report_start_date &
                          lubridate::parse_date_time(.data$end_time, "my") <= report_end_date)
}

#' @export
get_gs_data <- function(url,
                        sheet = "teaching",
                        report_start_date = NULL,
                        report_end_date = NULL,
                        date_cols = "date")
{
    dat <- googlesheets4::read_sheet(url, sheet = sheet, col_types = "c")

    if (is.null(report_start_date) || is.null(report_end_date))
    {
        return(dat)
    }

    in_period <- in_period(dat, report_start_date, report_end_date, date_cols)
    dplyr::filter(dat, in_period)
}
