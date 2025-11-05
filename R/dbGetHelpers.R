#' Get words used in study 1 or study 2
#'
#' @param conn A `DBIConnection` object, as returned by `dbConnect()`
#' @param study_id `numeric`, 1 or 2.
#' @returns `data.frame`
dbGetWords <- function(conn, study_id) {
    res <- dbSendQuery(conn,
        'SELECT
            w.id word_id,
            w.word
        FROM words w
        INNER JOIN studies_words s ON w.id = s.word_id
        WHERE study_id = ?;'
    )
    on.exit(dbClearResult(res))
    dbBind(res, list(study_id))
    return(dbFetch(res))
}

#' Get model RSMs used in study 1 or study 2
#'
#' There are six model RSMs in seach study: categorical, exp48, glove, sm8,
#' word2vec, and wordnet.
#'
#' @param conn A `DBIConnection` object, as returned by `dbConnect()`
#' @param study_id `numeric`, 1 or 2.
#' @returns `data.frame`
dbGetModelRSMs <- function(conn, study_id) {
    res <- dbSendQuery(conn,
        'SELECT code, data
        FROM model_rsms
        WHERE study_id = ?;'
    )
    on.exit(dbClearResult(res))
    dbBind(res, list(study_id))
    return(dbFetch(res))
}


dbGetModelEmbeddings <- function(conn, tau, use_data_cache = TRUE, overwrite_cache = FALSE) {
    cache <- file.path("data_cache", sprintf("rsl_model_embeddings_tau-%d.rds", tau * 100))
    if (use_data_cache && !overwrite_cache && file.exists(cache)) {
        embd <- readRDS(cache)
    } else {
        embd <- purrr::map(1:2, function(study_id) {
            model_rsms <- dbGetModelRSMs(conn, study_id)
            model_embd <- purrr::map(model_rsms$data, low_rank_rsm, tau = tau)
            names(model_embd) <- model_rsms$code
            return(model_embd)
        })
        names(embd) <- paste("study", seq_along(embd), sep = "_")
        if (use_data_cache) {
            saveRDS(embd, cache)
        }
    }
    return(embd)
}

#' Get neural RSMs used in study 1 or study 2
#'
#' There are six model RSMs in seach study: categorical, exp48, glove, sm8,
#' word2vec, and wordnet.
#'
#' @param conn A `DBIConnection` object, as returned by `dbConnect()`
#' @param study_id `numeric`, 1 or 2.
#' @param subject_id `numeric`
#' @returns `data.frame`
#' AND subject_id = ?
dbGetNeuralRSMs <- function(conn, study_id, subject_id = NULL) {
    if (is.null(subject_id)) {
        dbGetNeuralRSMsAllSubjects(conn, study_id)
    } else {
        dbGetNeuralRSMsOneSubject(conn, study_id, subject_id)
    }
}

dbGetNeuralRSMsAllSubjects <- function(conn, study_id) {
    res <- dbSendQuery(conn,
        'SELECT
            n.region_id,
            n.subject_id,
            r.hemisphere hemi,
            r.abbreviation abbrev,
            n.data
        FROM neural_rsms n
        LEFT JOIN regions r ON n.region_id = r.id
        WHERE study_id = ?
        ORDER BY subject_id, region_id;'
    )
    on.exit(dbClearResult(res))
    dbBind(res, list(study_id))
    return(dbFetch(res))
}

dbGetNeuralRSMsOneSubject <- function(conn, study_id, subject_id) {
    res <- dbSendQuery(conn,
        'SELECT
            n.region_id,
            r.hemisphere hemi,
            r.abbreviation abbrev,
            n.data
        FROM neural_rsms n
        LEFT JOIN regions r ON n.region_id = r.id
        WHERE study_id = ? AND subject_id = ?
        ORDER BY region_id;'
    )
    on.exit(dbClearResult(res))
    dbBind(res, list(study_id, subject_id))
    return(dbFetch(res))
}

#' Get potential confounds for words in study 1 or study 2
#'
#' A selection of variables from the tables `hoffman_semantic_diversity`,
#' `Glasgow_semantic_attributes`, `elp_lexical_attributes`, and
#' `binder_lexical_attributes`.
#'
#' @param conn A `DBIConnection` object, as returned by `dbConnect()`
#' @param study_id `numeric`, 1 or 2.
#' @returns `data.frame`
dbGetConfounds <- function(conn, study_id) {
    res <- dbSendQuery(conn,
        'SELECT
            sw.word_id,
            w.word,
            hsd.bnc_contexts hsd_bnc_contexts,
            hsd.semantic_diversity hsd_semantic_diversity,
        	gsa.concreteness_mean gsa_concreteness,
    	    gsa.imageability_mean gsa_imageability,
        	gsa.familiarity_mean gsa_familiarity,
        	gsa.aoa_mean gsa_aoa,
        	elp.log_freq_hal elp_log_freq_hal,
        	elp.concreteness elp_concreteness,
        	elp.semantic_neighborhood_density elp_sem_density,
        	elp.aoa elp_aoa,
        	elp.rt_mean elp_rt_mean,
        	elp.rt_zscore elp_rt_zscore,
        	elp.naming_rt_mean elp_naming_rt_mean,
        	elp.naming_rt_zscore elp_naming_rt_zscore,
        	bla.frequency_log10 bla_log_freq,
        	bla.unigram_freq bla_unigram_freq,
        	bla.bigram_freq bla_bigram_freq,
        	bla.trigram_freq bla_trigram_freq,
        	bla.imageability bla_imageability
        FROM studies_words sw
            INNER JOIN words w ON sw.word_id = w.id
            INNER JOIN binder_lexical_attributes bla ON sw.word_id = bla.word_id
            INNER JOIN elp_lexical_attributes elp ON sw.word_id = elp.word_id
            INNER JOIN hoffman_semantic_diversity hsd ON sw.word_id = hsd.word_id
            LEFT JOIN glasgow_semantic_attributes gsa ON sw.word_id = gsa.word_id
        WHERE study_id = ?;'
    )
    on.exit(dbClearResult(res))
    dbBind(res, list(study_id))
    return(dbFetch(res))
}

dbGetNeuralRSMKey <- function(conn, study_id) {
    res <- dbSendQuery(conn,
        'SELECT
            n.region_id,
            n.subject_id,
            r.hemisphere hemi,
            r.abbreviation abbrev
        FROM neural_rsms n
        LEFT JOIN regions r ON n.region_id = r.id
        WHERE study_id = ?
        ORDER BY subject_id, region_id;'
    )
    on.exit(dbClearResult(res))
    dbBind(res, list(study_id))
    return(dbFetch(res))
}


dbGetNeuralEmbeddings <- function(conn, tau, use_data_cache = TRUE, overwrite_cache = FALSE) {
    cache <- file.path("data_cache", sprintf("rsl_neural_embeddings_tau-%d.rds", tau * 100))
    if (use_data_cache && !overwrite_cache && file.exists(cache)) {
        embd <- readRDS(cache)
    } else {
        embd <- purrr::map(1:2, function(study_id) {
            neural_rsms <- dbGetNeuralRSMs(conn, study_id)
            neural_embd <- progressr::with_progress(low_rank_rsms(neural_rsms$data, tau = tau))
            names(neural_embd) <- neural_rsms$code
            return(neural_embd)
        })
        names(embd) <- paste("study", seq_along(embd), sep = "_")
        neural_rsm_key <- purrr::map(1:2, dbGetNeuralRSMKey, conn = conn)
        embd <- purrr::map2(embd, neural_rsm_key, ~{
            g <- group_by(.y, region_id, abbrev, hemi)
            k <- group_keys(g)
            r <- split(.x, .y$region_id)
            names(r) <- paste(k$abbrev, k$hemi, sep = "_")
            return(r)
        })
        if (use_data_cache) {
            saveRDS(embd, cache)
        }
    }
    return(embd)
}

#' Get all ratings and attributes from Binder lab
#'
#' A selection of variables from the tables `binder_lexical_attributes`,
#' `binder_conceptual_attributes`, and `binder_categories`.
#'
#' @param conn A `DBIConnection` object, as returned by `dbConnect()`
#' @param study_id `numeric`, 1 or 2.
#' @returns `data.frame`
dbGetBinderRatings <- function(conn, study_id) {
    res <- dbSendQuery(conn,
        'SELECT
            sw.word_id,
            w.word,
        	bla.*,
        	bca.*,
        	bcat.*
        FROM studies_words sw
            INNER JOIN words w ON sw.word_id = w.id
            INNER JOIN binder_lexical_attributes bla ON sw.word_id = bla.word_id
            INNER JOIN binder_conceptual_attributes bca ON sw.word_id = bca.word_id
            INNER JOIN binder_categories bcat ON sw.word_id = bcat.word_id
        WHERE study_id = ?;'
    )
    on.exit(dbClearResult(res))
    dbBind(res, list(study_id))
    return(dbFetch(res))
}

dbGetTmp <- function(conn, study_id) {
    res <- dbSendQuery(conn,
        'SELECT
            sw.word_id,
            w.word,
            blex.word_id word_id_bla,
            blex.length,
            blex.frequency,
            blex.frequency_log10,
            blex.orth_neighborhood,
            blex.orth_neighborhood_freq,
            blex.unigram_freq,
            blex.bigram_freq,
            blex.trigram_freq,
            blex.imageability,
            bcon.word_id word_id_bcon,
            bcon.vision,
            bcon.bright,
            bcon.dark,
            bcon.color,
            bcon.pattern,
            bcon.large,
            bcon.small,
            bcon.motion,
            bcon.fast,
            bcon.slow,
            bcon.shape,
            bcon.touch,
            bcon.temperature,
            bcon.texture,
            bcon.weight,
            bcon.pain,
            bcon.audition,
            bcon.loud,
            bcon.low,
            bcon.high,
            bcon.sound,
            bcon.taste,
            bcon.smell,
            bcon.head,
            bcon.upperlimb,
            bcon.lowerlimb,
            bcon.manipulation,
            bcon.landmark,
            bcon.path,
            bcon.scene,
            bcon.near,
            bcon.toward,
            bcon.away,
            bcon.time,
            bcon.duration,
            bcon.long,
            bcon.short,
            bcon.caused,
            bcon.consequential,
            bcon.human,
            bcon.benefit,
            bcon.harm,
            bcon.pleasant,
            bcon.unpleasant,
            bcon.drive,
            bcon.needs,
            bcon.attention,
            bcon.arousal,
            bcat.word_id word_id_bcat,
            bcat.superordinate,
            bcat.basic,
            bcat.kmeans28
        FROM studies_words sw
            LEFT JOIN words w ON sw.word_id = w.id
            LEFT JOIN binder_lexical_attributes blex ON sw.word_id = blex.word_id
            LEFT JOIN binder_conceptual_attributes bcon ON sw.word_id = bcon.word_id
            LEFT JOIN binder_categories bcat ON sw.word_id = bcat.word_id
        WHERE study_id = ?;'
    )
    on.exit(dbClearResult(res))
    dbBind(res, list(study_id))
    return(dbFetch(res))
}
