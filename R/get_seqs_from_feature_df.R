#' Title
#'
#' @param feature_df
#' @param origin
#' @param concat
#' @param make_revcomp
#'
#' @return
#' @export
#'
#' @examples
get_seqs_from_feature_df <- function(feature_df,
                                     origin,
                                     concat = T,
                                     make_revcomp = F,
                                     min_to_max_boundary = F,
                                     return = c("sequences", "df_wide", "df_long"),
                                     matches_to_origin_and_feature = list(c(F,T),c(F,F)),
                                     order_features = F) {

    # for alignment to origin, do not have revcomp computed
    # for appending a ref genome, do have the revcomp computed

    # set min_to_max_boundary to TRUE to have one whole sequence to add to genome file (instead of concat exons)

    return <- match.arg(return, c("sequences", "df_wide", "df_long"), several.ok = T)

    # prepare boundaries
    if (any(grepl("<|>", feature_df$range))) {
        message("'<' or '>' found in range column. Those will be removed.")
    }

    boundaries <- lapply(feature_df$range, function(x) strsplit(strsplit(gsub("<|>", "", x), ",")[[1]], "\\.\\."))
    if (order_features) {
        temp_order <- order(sapply(boundaries, function(x) min(as.numeric(unlist(x)))))
        boundaries <- boundaries[temp_order]
        feature_df <- feature_df[temp_order,]
    }

    if (min_to_max_boundary) {
        boundaries <- lapply(boundaries, function(x) list(c(min(unlist(x)), max(unlist(x)))))
        for (i in seq_along(boundaries)) {
            feature_df$range[i] <- paste0(min(boundaries[[i]][[1]]), "..", max(boundaries[[i]][[1]]))
        }
    }

    sequences <- NULL
    if ("sequences" %in% return) {
        # concat the sequence from segments
        sequences <- purrr::pmap(list(x = boundaries, revcomp = feature_df$complement,
                                      value = feature_df$value, range = feature_df$range), function(x,revcomp,value,range) {
                                          #seq <- paste(unlist(lapply(x, function(y) substr(origin, y[1], y[2]))), collapse = "")
                                          seq <- unlist(lapply(x, function(y) substr(origin, y[1], y[2])))
                                          if (revcomp && make_revcomp) {
                                              seq <- as.character(Biostrings::reverseComplement(Biostrings::DNAStringSet(seq)))
                                              seq <- rev(seq) # if no pasting above, separate seq have to be reversed here
                                          }
                                          if (concat) {
                                              seq <- paste(seq, collapse = "")
                                          } else {
                                              # w/o concat, name each subseq individually
                                              if (revcomp && make_revcomp) {
                                                  # check ?!
                                                  names(seq) <- paste0(value, "___", rev(strsplit(range, ",")[[1]]))
                                              } else {
                                                  names(seq) <- paste0(value, "___", strsplit(range, ",")[[1]])
                                              }
                                          }
                                          return(seq)
                                      })

        if (anyDuplicated(feature_df$value) && "Feature" %in% names(feature_df)) {
            names(sequences) <- make.unique(paste0(feature_df$value, "___", feature_df$Feature))
            # range could be added in addition or subfeature
        } else {
            names(sequences) <- make.unique(feature_df$value)
        }
    }

    dfs <- purrr::pmap(list(x = boundaries, revcomp = feature_df$complement,
                            value = feature_df$value, range = feature_df$range), function(x,revcomp,value,range) {
                                seq <- unlist(lapply(x, function(y) substr(origin, y[1], y[2])))
                                if (revcomp && make_revcomp) {
                                    seq <- as.character(Biostrings::reverseComplement(Biostrings::DNAStringSet(seq)))
                                    seq <- rev(seq) # if no pasting above, separate seq have to be reversed here
                                }
                                seq <- paste(seq, collapse = "")
                                df <- data.frame(position = unlist(seq2(sapply(x, "[", 1), sapply(x, "[", 2)), use.names = F), seq = strsplit(seq, "")[[1]])
                                names(df)[2] <- value
                                return(df)
                            })
    df0 <- data.frame(position = seq(1, nchar(origin)), origin = strsplit(origin, "")[[1]]) # origin column
    dfs <- join_chunkwise(data_frame_list = dfs,
                          join_fun = dplyr::full_join,
                          join_by = "position",
                          max_final_size = 5)
    df0 <- purrr::reduce(c(list(df0), dfs), dplyr::left_join, by = "position")

    ## assign matches and mismatches
    if (any(unlist(matches_to_origin_and_feature))) {
        match_mismatch_list <- lapply(stats::setNames(names(df0)[-c(1,2)], names(df0)[-c(1,2)]), function(x) df0[,x] == df0[,"origin"]) # subject seq in df (not df.original) may already contain "insertion"; for overlapping pattern where one receives an insertion and the other not, this is relevant
        if (any(sapply(matches_to_origin_and_feature, "[", 1))) {
            # do it outside of loop below to avoid double calculation
            matches <- purrr::pmap_lgl(match_mismatch_list, function(...) any_false(unlist(list(...))))
        }
        df0 <- purrr::map(setNames(matches_to_origin_and_feature, sapply(matches_to_origin_and_feature, function(x) paste(ifelse(x, "match", "base"), collapse = "_"))), function(y) {
            if (any(y)) {
                df0_match <- df0
                if (y[2]) {
                    # matches/mismatches to patterns
                    for (i in names(match_mismatch_list)) {
                        df0_match[,i] <- ifelse(match_mismatch_list[[i]], "match", "mismatch")
                    }
                }
                if (y[1]) {
                    # matches/mismatches to subject
                    df0_match[which(matches),"origin"] <- "match"
                    df0_match[which(!matches),"origin"] <- "mismatch"
                }
                return(df0_match)
            } else {
                return(df0)
            }
        })
    } else {
        df0 <- list("base_base" = df0)
    }

    df0_long <- NULL
    if ("df_long" %in% return) {
        df0_long <- purrr::map(df0, function(x) tidyr::pivot_longer(x, cols = -position, names_to = "seq.name", values_to = "seq"))
        if (order_features) {
            df0_long <- purrr::map(df0_long, function(x) {
                x$seq.name <- factor(x$seq.name, levels = names(df0[[1]])[-c(1,2)]) # do it like this in case duplicate column names have been altered; order should have been maintained as defined at the beginning
                return(x)
            })
        } else {
            df0_long <- purrr::map(df0_long, function(x) {
                x$seq.name <- as.factor(x$seq.name)
                return(x)
            })
        }
    }

    if (!"df_wide" %in% return) {
        df0 <- NULL
    }

    return(list(sequences = sequences, df_wide = df0, df_long = df0_long))
}

any_false <- function(x) {
    if (all(is.na(x))) {
        return(T)
    } else if (any(!x[which(!is.na(x))])) {
        return(F)
    } else if (all(x[which(!is.na(x))])) {
        return(T)
    } else {
        stop("Logical error.")
    }
}

