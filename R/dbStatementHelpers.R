read_sql_schema <- function(file) {
    sql_schema_txt <- read_file(file)
    sql_schema <- sql_schema_txt %>%
        stringr::str_remove_all("\\n") %>%
        stringr::str_remove_all("\\t") %>%
        stringr::str_remove_all("\\r") %>%
        stringr::str_split(pattern = ";") %>%
        purrr::flatten() %>%
        purrr::discard(function(x) nchar(x) == 0) %>%
        purrr::map(~{
            stringr::str_c(., ";")
        })
    return(sql_schema)
}

dbExecuteList <- function(conn, x) {
    dbWithTransaction(conn, {
        purrr::walk(x, dbExecute, conn = conn)
    })
}

