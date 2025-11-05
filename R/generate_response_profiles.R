generate_response_profiles <- function(.data) {
    x <- ifelse(xtabs(~ responses_revised + cue, data = .data)>1,1,0)
    return(x)
}
