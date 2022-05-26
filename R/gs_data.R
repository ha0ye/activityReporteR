#' @export
get_service_data <- function(url,
                             report_start_date = Sys.Date() - years(1),
                             report_end_date = Sys.Date())
{
    googlesheets4::read_sheet(url, sheet = "service", col_types = "c") %>%
        dplyr::filter(is.na(end_time) |
                   parse_date_time(end_time, "my") >= report_start_date &
                   parse_date_time(end_time, "my") <= report_end_date)
}

#' @export
get_gs_data <- function(url,
                        sheet = "teaching",
                        report_start_date = Sys.Date() - years(1),
                        report_end_date = Sys.Date(),
                        date_cols = "date")
{
    dat <- googlesheets4::read_sheet(data_gs_url, sheet = sheet, col_types = "c")

    in_period <- in_period(dat, report_start_date, report_end_date, date_cols)

    dplyr::filter(dat, in_period)
}

