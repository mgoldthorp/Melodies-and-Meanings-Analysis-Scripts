pivot_responses_wide_test <- function(d) {
    tidyr::pivot_wider(
        data = d,
        id_cols = tidyselect::everything(),
        names_prefix = "R",
        names_from = "response_order",
        values_from = "response"
    )
}

pivot_responses_long_test <- function(d) {
    tidyr::pivot_longer(
        data = d,
        cols = c("R1", "R2", "R3"),
        names_prefix = "R",
        names_to = "response_order",
        values_to = "response"
    )
}
