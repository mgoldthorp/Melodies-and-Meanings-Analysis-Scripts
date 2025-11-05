pivot_responses_wide <- function(d) {
    tidyr::pivot_wider(
        data = d,
        id_cols = tidyselect::everything(),
        names_prefix = "R",
        names_from = "response_order",
        values_from = "responses_revised"
    )
}

pivot_responses_long <- function(d) {
    tidyr::pivot_longer(
        data = d,
        cols = c("R1", "R2", "R3"),
        names_prefix = "R",
        names_to = "response_order",
        values_to = "responses_revised"
    )
}
