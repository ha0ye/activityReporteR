print_data <- function(df, empty_message = "(none)\n")
{
    if (is.null(df) || NROW(df) == 0)
    {
        cat(empty_message)
        return(invisible())
    }
    
    df %>%
        arrange(desc(date)) %>%
        pull(to_print) %>% 
        cat(sep = "\n")
}

format_author <- function(df, pattern = "H\\. Ye", replacement = "**H\\. Ye**")
{
    mutate(df, to_print = sub({{pattern}}, {{replacement}}, to_print))
}