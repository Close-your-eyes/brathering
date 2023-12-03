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
                                     origin = origin,
                                     concat = T,
                                     make_revcomp = F,
                                     min_to_max_boundary = F) {

  # for alignment to origin, do not have revcomp computed
  # for appending a ref genome, do have the revcomp computed

    # set min_to_max_boundary to TRUE to have one whole sequence to add to genome file (instead of concat exons)

  # prepare boundaries
  boundaries <- lapply(feature_df$range, function(x) strsplit(strsplit(x, ",")[[1]], "\\.\\."))
  if (min_to_max_boundary) {
      boundaries <- lapply(boundaries, function(x) list(c(min(unlist(x)), max(unlist(x)))))
      for (i in seq_along(boundaries)) {
          feature_df$range[i] <- paste0(min(boundaries[[i]][[1]]), "..", max(boundaries[[i]][[1]]))
      }
  }
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
  return(sequences)
}
