load_to_list <- function(files) {
  X <- new.env()
  lapply(files, load, envir = X)
  return(as.list(X))
}


generate_response_profiles <- function(d) {
  xtabs(~ responses_revised + cue, data = d)
}

correlate_binary_response_profiles <- function(x) {
  as.dist(cor(x > 0))
}


simulate_null_cor <- function(wide_assoc) {
  splits  <- lapply(wide_assoc, generate_splits, by = "cue")
  mixed <- list(
    do.call('rbind', lapply(c(splits[[1]], splits[[2]]), `[[`, 1)),
    do.call('rbind', lapply(c(splits[[1]], splits[[2]]), `[[`, 2))
  )
  repsim <- lapply(
    mixed,
    function(d) {
      correlate_binary_response_profiles(
        generate_response_profiles(
          pivot_responses_long(d)
        )
      )
    }
  )
  return(cor(repsim[[1]], repsim[[2]], method = "spearman"))
}


pivot_responses_wide <- function(d) {
  tidyr::pivot_wider(
    data = d,
    id = tidyselect::everything(),
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
