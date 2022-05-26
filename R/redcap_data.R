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


    resp <- REDCapR::redcap_read_oneshot(url, token)
    dat <- resp$data

    get_file()
    get_file("metadata", paste0(db, "_dict.csv"))
}

read_redcap_data <- function(db = "refstats",
                             report_start_date = Sys.Date() - years(1),
                             report_end_date = Sys.Date(),
                             date_cols = "date",
                             librarian = "Hao Ye",
                             data_file = here::here("data", paste0(db, "_clean.csv")))
{
    dat <- read.csv(data_file)

    in_period <- in_period(dat, report_start_date, report_end_date, date_cols)

    dplyr::filter(dat, in_period,
                  librarian == {{librarian}})
}

get_redcap_token <- function(db)
{
    Sys.getenv(paste0(db, "_token"))
}
