dbSafeQuery <- function(db_path, f, ...) {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn))
  return(f(conn, ...))
}


dbSelectResponses <- function(conn, subject_id) {
  res <- dbSendQuery(conn,
    'SELECT
      r.subject_id AS subject_id,
      c.cue AS cue,
      r.response AS response,
      r.response_order AS response_order
    FROM responses r
    LEFT JOIN cues AS c ON c.id = r.cue_id
    WHERE subject_id = ?;'
  )
  on.exit(dbClearResult(res))
  dbBind(res, list(subject_id))
  return(dbFetch(res))
}


dbSelectSubjects <- function(conn) {
  dbGetQuery(conn, '
    SELECT
      s.id AS subject_id,
      sl.hash AS lock_hash
    FROM subjects AS s
    LEFT JOIN subject_decisions sd ON sd.subject_id = s.id
    LEFT JOIN subject_locks sl ON sl.subject_id = s.id
    WHERE
      (decision_id IS NULL AND
      lock_hash IS NULL) OR
      decision_id = 3;
  ')
}

dbSelectOneUnmappedCueResponse <- function(conn) {
  dbGetQuery(conn, '
    SELECT
      cr.id AS cue_response_id,
      c.cue as cue,
      r.response as response
    FROM cues_responses AS cr
    LEFT JOIN cues AS c ON c.id = cr.cue_id
    LEFT JOIN responses AS r ON r.id = cr.response_id
    LEFT JOIN response_map AS rm ON rm.cue_response_id = cr.id
    LEFT JOIN response_locks rl ON rl.cue_response_id = cr.id
    WHERE rm.id IS NULL AND rl.hash IS NULL
    LIMIT 1;
  ')
}

dbSelectOneCueResponse <- function(conn, cue_response_id) {
  res <- dbSendQuery(conn, '
    SELECT
      cr.id AS cue_response_id,
      c.cue as cue,
      r.response as response
    FROM cues_responses AS cr
    LEFT JOIN cues AS c ON c.id = cr.cue_id
    LEFT JOIN responses AS r ON r.id = cr.response_id
    LEFT JOIN response_locks rl ON rl.cue_response_id = cr.id
    WHERE cr.id=? IS NULL AND rl.hash IS NULL
    LIMIT 1;
  ')
  on.exit(dbClearResult(res))
  dbBind(res, list(cue_response_id))
  return(dbFetch(res))
}


dbSelectMetadata <- function(conn, pattern = "") {
  res <- dbSendQuery(conn, '
    SELECT word, kuperman_id, subtlex_id, cue_id
    FROM words_meta
    WHERE word LIKE ?;
  ')
  on.exit(dbClearResult(res))
  dbBind(res, list(pattern))
  return(dbFetch(res))
}

dbGetKuperman <- function(conn) {
  dbGetQuery(conn, '
    SELECT * FROM kuperman
  ')
}

dbGetSUBTLEX <- function(conn) {
  dbGetQuery(conn, '
    SELECT * FROM subtlex
  ')
}

dbGetResponses <- function(conn) {
  dbGetQuery(conn, '
    SELECT
      r.id,
      r.cue_id,
      c.cue,
      r.response,
      kuperman_id,
      subtlex_id,
      rl.hash AS lock_hash
    FROM responses AS r
    LEFT JOIN response_locks rl ON rl.response_id = r.id
    LEFT JOIN cues c ON c.id = r.cue_id
    WHERE
      (kuperman_id IS NULL OR
      subtlex_id IS NULL) AND
      lock_hash IS NULL;
  ')
}




dbSelectResearcherIDs <- function(conn, selectize_placeholder = FALSE) {
  researchers <- dbReadTable(conn, "researchers")
  r_ids <- c(researchers$id)
  names(r_ids) <- str_remove(researchers$email, fixed("@lsu.edu"))
  if (selectize_placeholder) {
    placeholder <- ""
    names(placeholder) <- "..."
    r_ids <- c(placeholder, r_ids)
  }
  return(r_ids)
}
