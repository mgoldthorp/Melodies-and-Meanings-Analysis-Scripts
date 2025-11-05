dbLockSubject <- function(conn, subject_id, researcher_id) {
  hash <- paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
  invalid_hash <- TRUE
  while (invalid_hash) {
    invalid_hash <- tryCatch(
      {
        dbInsertSubjectLock(conn, hash, subject_id, researcher_id)
        FALSE
      },
      error = function(cond) {
        TRUE
      }
    )
  }
  return(hash)
}


dbInsertSubjectLock <- function(conn, hash, subject_id, researcher_id) {
  res <- dbSendStatement(conn, '
        INSERT INTO subject_locks (hash, subject_id, researcher_id, timestamp)
        VALUES (?, ?, ?, datetime("now"));
  ')
  on.exit(dbClearResult(res))
  dbBind(res, list(hash, as.numeric(subject_id), as.numeric(researcher_id)))
}


dbIsLockedResponse <- function(conn, cue_response_id) {
  res <- dbSendQuery(conn, '
    SELECT count(*) FROM response_locks WHERE cue_response_id=?;
  ')
  on.exit(dbClearResult(res))
  dbBind(res, list(cue_response_id))
  return(as.vector(unname(unlist(dbFetch(res) > 0))))
}


dbLockResponse <- function(conn, cue_response_id, researcher_id) {
  invalid_hash <- TRUE
  while (invalid_hash) {
    hash <- paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
    invalid_hash <- tryCatch(
      {
        dbInsertResponseLock(conn, hash, cue_response_id, researcher_id)
        FALSE
      },
      error = function(cond) {
        TRUE
      }
    )
  }
  return(hash)
}


dbInsertResponseLock <- function(conn, hash, cue_response_id, researcher_id) {
  res <- dbSendStatement(conn, '
        INSERT INTO response_locks (hash, cue_response_id, researcher_id, timestamp)
        VALUES (?, ?, ?, datetime("now"));
  ')
  on.exit(dbClearResult(res))
  dbBind(res, list(hash, as.numeric(cue_response_id), as.numeric(researcher_id)))
}


dbUnlockSubject <- function(conn, hash) {
  res <- dbSendStatement(conn, '
    DELETE FROM subject_locks WHERE hash=?;
  ')
  on.exit(dbClearResult(res))
  dbBind(res, list(hash))
  return(dbGetRowsAffected(res))
}

dbUnlockResponse <- function(conn, hash) {
  res <- dbSendStatement(conn, '
    DELETE FROM response_locks WHERE hash=?;
  ')
  on.exit(dbClearResult(res))
  dbBind(res, list(hash))
  return(dbGetRowsAffected(res))
}
