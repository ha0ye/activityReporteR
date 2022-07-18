#' Download a REDCap database and associated dictionary
#'
#' @param db name of database
#' @param data_folder local folder in which to store files
#' @param url url for the REDCap API
#' @return (none)
#'
#' @export

download_redcap_data <- function(db = c("refstats", "searches"),
                                 data_folder = "data",
                                 url = "https://redcap.ctsi.ufl.edu/redcap/api/")
{
    get_file <- function(content_type = "record",
                         file_name = paste0(db, "_raw.csv"),
                         format = "csv")
    {
        # construct query
        query <- list(token = get_redcap_token(db),
                      content = content_type,
                      format = format,
                      type = "flat")

        # try to get data
        resp <- httr::POST(url = url, body = query)

        if (resp$status_code != 200)
        {
            stop("Failed to retrieve data.\n", resp)
        }

        writeBin(httr::content(resp, "raw"),
                 here::here(data_folder, file_name))
    }

    db <- match.arg(db)

    get_file()
    get_file("metadata", paste0(db, "_dict.csv"))

    invisible()
}

#' @export
read_redcap_data <- function(db = "refstats",
                             report_start_date = Sys.Date() - lubridate::years(1),
                             report_end_date = Sys.Date(),
                             date_cols = "date",
                             librarian = "Hao Ye",
                             data_file = here::here("data", paste0(db, "_clean.csv")))
{
    dat <- utils::read.csv(data_file)

    in_period <- in_period(dat, report_start_date, report_end_date, date_cols)

    dplyr::filter(dat, in_period,
                  librarian == {{librarian}})
}

#' @export
get_redcap_token <- function(db)
{
    Sys.getenv(paste0(db, "_token"))
}

#' @export
convert_redcap_fields <- function(db = "refstats", field = "librarian",
                           raw_file = here::here("data", paste0(db, "_raw.csv")),
                           dict_file = here::here("data", paste0(db, "_dict.csv")),
                           out_file = here::here("data", paste0(db, "_clean.csv")))
{
    # read in data
    dat <- utils::read.csv(raw_file)
    dict <- utils::read.csv(dict_file)

    # grab field values
    tx <- dict %>%
        dplyr::filter(.data$field_name == field) %>%
        dplyr::pull(.data$select_choices_or_calculations)

    # construct field lookup table
    LUT <- tx %>%
        stringr::str_split("\\|", simplify = TRUE) %>%
        t() %>%
        trimws() %>%
        tibble::enframe(name = NULL) %>%
        tidyr::separate(.data$value, c("key", "value"), sep = ", ")

    # apply field lookup table
    idx <- match(dplyr::pull(dat, field), LUT$key)
    dat[[field]] <- LUT$value[idx]

    # write cleaned up data
    utils::write.csv(dat, out_file)
}
