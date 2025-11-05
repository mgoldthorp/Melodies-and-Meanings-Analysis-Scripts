#' Get words used in study 1 or study 2
#'
#' @param conn A `DBIConnection` object, as returned by `dbConnect()`
#' @param study_id `numeric`, 1 or 2.
#' @returns `data.frame`
dbInsertDecision <- function(conn, subject_id, decision_id, researcher_id) {
  res <- dbSendStatement(conn, '
    INSERT INTO subject_decisions (subject_id, decision_id, researcher_id, timestamp)
    VALUES (?, ?, ?, datetime("now"));
  ')
  on.exit(dbClearResult(res))
  dbBind(
    res,
    lapply(list(subject_id, decision_id, researcher_id), as.numeric)
  )
  return(dbGetRowsAffected(res))
}


dbInsertMappings <- function(conn, cue_response_id, kuperman_id, subtlex_id, cue_id, revision, researcher_id) {
  res <- dbSendStatement(conn, '
    INSERT INTO response_map (cue_response_id, kuperman_id, subtlex_id, cue_id, revision, researcher_id, timestamp)
    VALUES (?, ?, ?, ?, ?, ?, datetime("now"));
  ')
  on.exit(dbClearResult(res))
  dbBind(
    res,
    list(
      as.numeric(cue_response_id),
      as.numeric(kuperman_id),
      as.numeric(subtlex_id),
      as.numeric(cue_id),
      as.character(revision),
      as.numeric(researcher_id)
    )
  )
  return(dbGetRowsAffected(res))
}




dbInsertIndex <- function(conn,id, response_id, revision_k, revision_s, researcher_id) {
  res <- dbSendStatement(conn, '
    INSERT INTO response_revisions (id, response_id, revision_k, revision_s, researcher_id, timestamp )
    VALUES (?, ?, ?, ?, ?, datetime("now"));
  ')
  on.exit(dbClearResult(res))
  dbBind(
    res,
    lapply(list(id, response_id, revision_k, revision_s,researcher_id), as.numeric)
  )
  return(dbGetRowsAffected(res))
}
