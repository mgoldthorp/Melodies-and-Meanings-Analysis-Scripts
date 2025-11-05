generate_response_profiles_test <- function(.data) {
    x <- ifelse(xtabs(~ response + cue, data = .data)>1,1,0)
    return(x)
}
