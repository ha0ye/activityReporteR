#' @export
get_zenodo_stats <- function()
{
    get_zenodo_depositions() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(data = list(get_zenodo_record(id)),
                      views = purrr::pluck(data, "stats", "version_views", .default = NA),
                      downloads = purrr::pluck(data, "stats", "version_downloads", .default = NA))
}

#' @export
get_zenodo_depositions <- function()
{
    resp <- make_zenodo_query()
    dat <- httr::content(resp)

    tibble::tibble(conceptdoi = purrr::map_chr(dat, purrr::pluck, "conceptdoi"),
                   doi = purrr::map_chr(dat, purrr::pluck, "doi"),
                   doi_url = purrr::map_chr(dat, purrr::pluck, "doi_url"),
                   id = purrr::map_chr(dat, purrr::pluck, "id"),
                   title = purrr::map_chr(dat, purrr::pluck, "title"))
}

#' @export
get_zenodo_record <- function(id)
{
    path = paste0("api/records/", id)

    url <- httr::modify_url(url = "https://zenodo.org/",
                            path = path,
                            query = list(access_token = get_zenodo_token()))

    resp <- curl::curl_fetch_memory(url)
    if (resp$status_code != 200)
    {
        warning("-- QUERY: ", url, "\n-- HAD STATUS: ", resp$status_code)
        return(invisible(NA))
    }
    invisible(jsonlite::fromJSON(rawToChar(resp$content)))
}


#' Send a query to the Zenodo API
#'
#' @param path the path to the endpoint
#' @param token access token for Zenodo API
#' @param ... additional arguments
#' @return response from the API
#'
#' @export
make_zenodo_query <- function(path = "api/deposit/depositions",
                              token = get_zenodo_token(),
                              ...)
{
    # setup params for access and any other arguments
    params <- list(access_token = token,
                   ...)

    resp <- httr::GET(url = "https://zenodo.org/",
                      path = path,
                      query = params)

    if (resp$status_code != 200)
    {
        stop("Failed to retrieve data.\n", resp)
    }

    invisible(resp)
}


#' Download a REDCap database and associated dictionary
#'
#' @param db name of database
#' @param data_folder local folder in which to store files
#' @param url url for the REDCap API
#' @return (none)
#'
#' @export

#' @export
get_zenodo_token <- function(env_var = "zenodo_token")
{
    token <- Sys.getenv(env_var, unset = NA)
    if (is.na(token))
    {
        stop("zenodo token not found! You may need to create one at https://zenodo.org/account/settings/applications/tokens/new/")
    }

    invisible(token)
}
