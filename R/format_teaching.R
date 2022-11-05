#' @export
format_teaching <- function(df, type = "standalone")
{
    to_format <- df %>%
        dplyr::filter(type == {{type}}) %>%
        dplyr::mutate(duration = as.numeric(.data$duration_min) / 60,
               duration_whole_hour = .data$duration == round(.data$duration),
               duration = ifelse(.data$duration_whole_hour,
                                 as.character(round(.data$duration)),
                                 as.character(.data$duration)))
    format_fun <- switch(type,
                         "standalone" = format_teaching_standalone,
                         "course-integrated" = format_teaching_course_integrated,
                         "course" = format_teaching_course,
                         "other" = format_teaching_other)

    if (NROW(to_format) == 0)
    {
        return(NULL)
    }

    format_fun(to_format)
}

#' @export
format_teaching_standalone <- function(df)
{
    df %>%
        dplyr::group_by(.data$title) %>%
        dplyr::arrange(dplyr::desc(.data$date)) %>%
        dplyr::mutate(print_header = glue('"{title}", {duration}-hr workshop')) %>%
        dplyr::summarize(
            to_print = paste(
                glue("{date}, {num_attendees} attendees"),
                collapse = "\n"),
            date = dplyr::first(.data$date)
        ) %>%
        dplyr::mutate(to_print = glue("{title}\n\n{to_print}\n", .trim = FALSE)) %>%
        dplyr::select(.data$date, .data$to_print)
}

#' @export
format_teaching_course_integrated <- function(df)
{
    df %>%
        dplyr::arrange(dplyr::desc(date)) %>%
        dplyr::mutate(other_instructor = ifelse(is.na(.data$other_instructor), "", glue("co-instructor with {other_instructor}, ")),
               print_header = glue('{course_id}: {title}, {date}, {duration}-hr guest lecture ({session}), {other_instructor}{num_attendees} attendees'),
               print_instructor = glue("  \nCourse Instructor: {instructor}"),
               to_print = glue("{print_header}  \n{print_instructor}\n", .trim = FALSE)) %>%
        dplyr::select(.data$date, .data$to_print)
}

#' @export
format_teaching_course <- function(df)
{
    stop("NEED TO IMPLEMENT THIS")
}

#' @export
format_teaching_other <- function(df)
{
    df %>%
        dplyr::mutate(title = ifelse(is.na(.data$session), .data$title, glue("{title}: {session}")),
               instructor_info = ifelse(is.na(.data$instructor) | .data$instructor == "Hao Ye", "", glue("Instructor: {instructor}")),
               coinstructor_info = ifelse(is.na(.data$other_instructor), "", glue("co-instructors: {other_instructor}")),
               event_info = ifelse(is.na(.data$event_info), "", glue("{event_info}, ")),
               conjunction = ifelse(nchar(.data$instructor_info) != 0 & nchar(.data$coinstructor_info) != 0,
                                    "; ",
                                    ""),
               instructor_line = ifelse(nchar(.data$instructor_info) == 0 & nchar(.data$coinstructor_info) == 0,
                                        "",
                                        glue("  \n{instructor_info}{conjunction}{coinstructor_info}", .trim = FALSE)),
               to_print = glue('"{title}", {event_info}({format}), {date}{instructor_line}\n', .trim = FALSE)) %>%
        dplyr::select(.data$date, .data$to_print)
}
