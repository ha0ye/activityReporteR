#' @export
format_grants <- function(df, status = "Funded", format_author = c("Hao Ye", "**Hao Ye**"))
{
    df %>%
        dplyr::filter(status == {{status}}) %>%
        dplyr::arrange(dplyr::desc(.data$date_end)) %>%
        dplyr::mutate(date = .data$date_start,
               pi_line = glue("PI: {pi}."),
               copi_line = ifelse(is.na(.data$co_pi), "", glue(" Co-PI: {co_pi}.")),
               investigator_line = ifelse(is.na(.data$other_investigators), "", glue(" Investigators: {other_investigators}")),
               author_line = glue("{pi_line}{copi_line}{investigator_line}"),
               to_print = glue("Grant Period: {date_start}---{date_end}",
                               "Title: {title}",
                               "Funding Agency: {funder}",
                               "Status: {status}",
                               "Total Award: {total_award}",
                               "{description}",
                               "{author_line}\n",
                               .sep = "  \n",
                               .trim = FALSE)) %>%
        dplyr::select(.data$date, .data$to_print)
}
