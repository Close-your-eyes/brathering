#' Match strings using ordered rules and optional global assignment
#'
#' Match each query to a candidate using exact equality, extracted keys,
#' positional prefixes, string distances, or shared contiguous substrings.
#' Unresolved queries advance through the supplied modes and distance methods.
#' Successful matches remain fixed. Candidate reuse can be disabled, with
#' either greedy or global assignment within each stage.
#'
#' @param x Character vector of queries, without missing values.
#' @param candidates Character vector of candidate strings, without missing
#'   values. Candidate identity is its index: duplicate strings at different
#'   indices are treated as separate candidates.
#' @param mode Character vector of matching modes, in execution order. Choices
#'   are `"exact"`, `"key"`, `"prefix"`, `"distance"`, and `"substring"`.
#'   Defaults to `c("exact", "distance")`. Explicit order is respected; put
#'   `"exact"` first to reserve identical strings before approximate matching.
#' @param method Character vector of methods accepted by
#'   [stringdist::stringdistmatrix()], tried in order within each distance
#'   mode. Defaults to `"lv"`. Each method is a separate stage.
#' @param start Positive integer giving the first character position compared
#'   in prefix mode. Positions are one-based. Defaults to `1L`.
#' @param n Positive integer giving the number of characters compared in prefix
#'   mode. Defaults to `4L`. Both strings must contain the full requested range.
#' @param max_dist Nonnegative maximum distance, inclusive. Supply a scalar,
#'   one value per distance method in order, or a named vector containing every
#'   requested method. Defaults to `0`, allowing exact distance matches only.
#'   An infinite threshold requires an explicit finite `unmatched_cost` for
#'   that method. Distance units depend on the method.
#' @param min_chars Positive integer giving the minimum shared contiguous
#'   substring length. Defaults to `8L`.
#' @param ties How to handle unresolved ties: `"NA"` (default) leaves them
#'   unmatched; `"first"` permits arbitrary selection only at the final stage.
#'   Global ties are then resolved by query order and candidate index, with
#'   rejection ordered after real candidates. Positive `min_gap` requirements
#'   still apply. Earlier ties always advance to the next stage.
#' @param reuse_candidates Logical. If `TRUE` (default), queries are matched
#'   independently and may select the same candidate index. If `FALSE`, each
#'   candidate index may be selected once.
#' @param ... Additional arguments passed to every call to
#'   [stringdist::stringdistmatrix()], such as `q = 3` or `p = 0.1`.
#' @param preprocess Function applied separately to both input vectors before
#'   scoring and key extraction. It must preserve lengths and return character
#'   vectors without missing values. Defaults to [identity()]. Original input
#'   strings are retained in the output. Arguments after `...` must be named
#'   exactly when supplied.
#' @param key Key-extraction function, required when `mode` includes `"key"`.
#'   It receives preprocessed strings and must return one character key per
#'   input. Missing and empty keys never match. Defaults to `NULL`.
#' @param candidate_filter Either `NULL` (all pairs permitted), a logical
#'   matrix with `length(x)` rows and `length(candidates)` columns, or a
#'   function `function(x, candidates)` returning such a matrix. The function
#'   receives original strings before preprocessing. Missing matrix entries
#'   are not allowed. `FALSE` forbids a pair in every mode, including exact
#'   and key matching.
#' @param assignment Assignment policy when reuse is disabled: `"global"`
#'   (default) minimizes total cost within each stage, including costs for
#'   rejection; `"greedy"` processes queries in input order. Ignored when
#'   `reuse_candidates = TRUE`. Global assignment requires the clue package.
#' @param min_gap Finite nonnegative minimum gap between the two best rowwise
#'   costs, before availability and score thresholds are applied. Used only in
#'   distance and substring modes. Supply a scalar or a uniquely named vector
#'   of method/mode settings; see Details. Defaults to `0`. A rowwise tie is
#'   still ambiguous unless final-stage tie selection is enabled.
#' @param min_coverage Minimum fraction of the shorter string covered by the
#'   longest shared contiguous substring, between 0 and 1. Defaults to `0.5`.
#'   Both this requirement and `min_chars` must be satisfied.
#' @param unmatched_cost Optional finite nonnegative rejection cost, supplied
#'   as a scalar or a uniquely named vector of method/mode settings. A real
#'   pair must cost strictly less than this value. `NULL` uses stage-specific
#'   defaults; see the Unmatched costs section. Also caps acceptance under
#'   greedy assignment and candidate reuse.
#' @param tolerance Finite nonnegative relative numerical tolerance used to
#'   compare row costs and global objective values for ties. Defaults to
#'   `1e-10`. It does not relax distance, coverage, margin, or rejection limits.
#'
#' @details
#' Each distance method expands into a separate stage. For example,
#' `mode = c("exact", "distance", "substring")` with
#' `method = c("lv", "osa")` runs exact matching, Levenshtein distance, OSA
#' distance, and then substring matching. Fallbacks search all eligible
#' candidates, not only the preceding stage's tied candidates.
#'
#' Exact and key matches score 0. Prefix mode compares `n` characters starting
#' at `start`. Distance scores are smaller for better matches. Substring
#' scores are the lengths of the longest exact contiguous shared runs, so
#' larger scores are better. Substring mode does not allow gaps between
#' matching characters.
#'
#' For named `min_gap` and `unmatched_cost` vectors, lookup prefers the method
#' name, then mode name, then `".default"`. If none is present, the documented
#' default is used. Unnamed settings must be scalar.
#'
#' Confidence statistics include all structurally permitted candidates, even
#' those reserved earlier, before score thresholds are applied. Removing a
#' competitor therefore cannot manufacture a larger gap. The gap is
#' second-best minus best cost, or best minus second-best shared length in
#' substring mode. It is infinite when only one finite-cost candidate exists.
#' A large gap is not a probability or a guarantee of correctness.
#'
#' @section Global assignment and ambiguity:
#' Global assignment uses [clue::solve_LSAP()] with one private dummy candidate
#' per query to represent rejection. It optimizes only the current stage;
#' earlier accepted matches remain fixed. It may select a row's second-best
#' candidate if that improves the total result. `min_gap` screens rowwise
#' evidence; inspect `selected_rank` and `assignment_gap` for the resulting
#' global choice.
#'
#' Rowwise ties advance to a fallback unless final-stage tie selection is
#' enabled. The function also checks for equal global optima by forbidding
#' each chosen edge and solving again. Ambiguous assignments advance to the
#' next stage; assignments shared by all optimal solutions can be accepted.
#' These extra solves and the pairwise matrices make this implementation
#' suitable for modest string collections rather than very large collections.
#'
#' @section Result summary:
#' On completion, a message reports the number of matched queries, unmatched
#' queries (including unresolved ties), and distinct candidate indices used.
#' If final-stage tie-breaking selected any matches, their count is also
#' reported. The summary includes empty-input cases. Use [suppressMessages()]
#' to silence the message; the returned data frame is unaffected.
#'
#' @section Optional dependencies:
#' Distance stages use the optional stringdist package. Global assignment
#' without candidate reuse uses the optional clue package when eligible pairs
#' are present. Dependencies are checked only when their respective branches
#' are reached. Missing packages are installed through brathering's shared
#' optional-dependency helper.
#'
#' @section Unmatched costs:
#' Internal pair costs and default rejection penalties are:
#' * Exact, key, and prefix: pair cost 0; rejection penalty 1.
#' * Distance: raw string distance; rejection penalty `max_dist + 1`.
#' * Substring: query length minus shared length; rejection penalty
#'   `max(1, query length - min_chars + 1)`.
#'
#' A pair must cost strictly less than its rejection penalty, independently
#' of other thresholds and filters. Substring rejection costs are measured
#' in unmatched query characters, not in the reported shared-length score.
#'
#' @returns A data frame with one row per query, in input order. It contains:
#' * `query`, `index`, `match`: original query, selected candidate index, and
#'   original candidate string. Unresolved indices and matches are `NA`.
#' * `score`: selected-pair score, or `NA` when unmatched.
#' * `status`: `"matched"`, `"unmatched"`, `"tie"`, or `"tie_first"`.
#' * `reason`: `"accepted"`, `"no_candidates"`, `"no_match"`, `"filtered"`,
#'   `"above_threshold"`, `"below_threshold"`, `"low_margin"`,
#'   `"candidate_taken"`, `"unmatched_cost"`, `"ambiguous"`, or
#'   `"ambiguous_assignment"`.
#' * `mode`, `method`, `stage`: successful stage, or last attempted stage.
#' * `best_index`, `best_score`, `runner_up_score`, `gap`, `n_best`: rowwise
#'   best candidate and competition statistics before availability and score
#'   threshold exclusions. `n_best` counts tied best candidates.
#' * `selected_rank`: selected candidate's rank among structurally permitted
#'   candidates; tied costs share a rank.
#' * `assignment_gap`: increase in the global objective if the chosen edge
#'   is forbidden, in internal cost units. Zero indicates an alternative
#'   optimum; `Inf` indicates no feasible alternative. Available for global
#'   assignment, and not a confidence probability.
#' * `shared_substring`, `coverage`: substring evidence for the selected pair.
#' * `best_substring`, `best_coverage`: substring evidence for the rowwise
#'   best pair, including rejected queries.
#' * `trace`: list-column of data frames recording attempted stages for each
#'   query, including `query_index` and the screened `available_count`.
#'
#' Fields that do not apply are `NA`. For unmatched rows, consult `trace` for
#' earlier rejection reasons. An unmatched global row with otherwise eligible
#' choices assigned elsewhere is reported as `"candidate_taken"`.
#' The `"stages"` attribute contains the expanded stage plan. Empty input
#' vectors are supported.
#'
#' @seealso [stringdist::stringdistmatrix()], [clue::solve_LSAP()]
#' @export
#' @examples
#' # Exact matching preserves query order and leaves missing strings unmatched.
#' match_strings(c("beta", "absent"), c("alpha", "beta"),
#'               mode = "exact")
#'
#' # Supply a key extractor for structured strings.
#' queries <- c("A:old label", "B:short label")
#' pool <- c("B:expanded label", "A:updated label")
#' get_key <- function(z) sub(":.*$", "", z)
#' match_strings(queries, pool, mode = c("exact", "key"), key = get_key)
#'
#' # A custom filter can require the same group before any matching stage.
#' same_group <- function(x, candidates) {
#'   outer(get_key(x), get_key(candidates), "==")
#' }
#' match_strings(queries, pool, mode = "key", key = get_key,
#'               candidate_filter = same_group)
#'
#' # Inspect the exact contiguous run and its coverage.
#' match_strings("blue sky", "a clear blue sky", mode = "substring",
#'               min_chars = 8)[, c("match", "shared_substring", "coverage")]
#'
#' if (requireNamespace("stringdist", quietly = TRUE)) {
#'   # Levenshtein ties; OSA resolves the transposition in the next stage.
#'   res <- match_strings("abc", c("axy", "acb"), mode = "distance",
#'                        method = c("lv", "osa"), max_dist = c(lv = 2, osa = 2))
#'   res$trace[[1]]
#' }
#'
#' if (requireNamespace("stringdist", quietly = TRUE) &&
#'     requireNamespace("clue", quietly = TRUE)) {
#'   # Consider both queries together, with no candidate reuse.
#'   match_strings(c("abc", "abd"), c("abd", "axy"), mode = "distance",
#'                 max_dist = 2, unmatched_cost = 3,
#'                 reuse_candidates = FALSE, assignment = "global")
#' }
match_strings <- function(x,
                          candidates,
                          mode = c("exact", "distance"),
                          method = "lv",
                          start = 1L,
                          n = 4L,
                          max_dist = 0,
                          min_chars = 8L,
                          ties = c("NA", "first"),
                          reuse_candidates = TRUE,
                          ...,
                          preprocess = identity,
                          key = NULL,
                          candidate_filter = NULL,
                          assignment = c("global", "greedy"),
                          min_gap = 0,
                          min_coverage = 0.5,
                          unmatched_cost = NULL,
                          tolerance = 1e-10) {
  ties <- match.arg(ties)
  assignment <- match.arg(assignment)
  check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
  strings <- function(z) is.character(z) && !anyNA(z)
  scalar <- function(z) is.numeric(z) && length(z) == 1L && !is.na(z)
  pos_integer <- function(z) scalar(z) && is.finite(z) && z >= 1 && z == floor(z)
  check(strings(x) && strings(candidates), "x and candidates must be character vectors without NA.")
  check(strings(mode) && length(mode) > 0L &&
          all(mode %in% c("exact", "key", "prefix", "distance", "substring")),
        "Unknown or empty mode; use exact, key, prefix, distance, or substring.")
  check(is.logical(reuse_candidates) && length(reuse_candidates) == 1L &&
          !is.na(reuse_candidates), "reuse_candidates must be TRUE or FALSE.")
  check(is.function(preprocess), "preprocess must be a function.")
  check(scalar(tolerance) && is.finite(tolerance) && tolerance >= 0,
        "tolerance must be a finite nonnegative number.")
  if ("prefix" %in% mode) check(pos_integer(start) && pos_integer(n), "start and n must be positive integers.")
  if ("substring" %in% mode) {
    check(pos_integer(min_chars), "min_chars must be a positive integer.")
    check(scalar(min_coverage) && is.finite(min_coverage) &&
            min_coverage >= 0 && min_coverage <= 1, "min_coverage must be between 0 and 1.")
  }
  # Named stage settings prefer a method name, then mode, then .default.
  validate_setting <- function(z, label) {
    check(is.numeric(z) && length(z) > 0L && !anyNA(z) &&
            all(is.finite(z)) && all(z >= 0), paste(label, "must contain finite nonnegative numbers."))
    check((is.null(names(z)) && length(z) == 1L) ||
            (!is.null(names(z)) && all(nzchar(names(z))) && !anyNA(names(z)) &&
               !anyDuplicated(names(z))), paste(label, "must be a scalar or uniquely named vector."))
    allowed <- c("exact", "key", "prefix", "distance", "substring", ".default",
                 "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
    if (!is.null(names(z))) check(all(names(z) %in% allowed), paste("Unknown name in", label))
  }
  validate_setting(min_gap, "min_gap")
  if (!is.null(unmatched_cost)) validate_setting(unmatched_cost, "unmatched_cost")
  setting <- function(z, m, meth, default) {
    if (is.null(z)) return(default)
    if (is.null(names(z))) return(unname(z))
    pick <- c(meth[!is.na(meth)], m, ".default")
    pick <- pick[pick %in% names(z)]
    if (length(pick)) unname(z[pick[1L]]) else default
  }
  a <- preprocess(x)
  b <- preprocess(candidates)
  check(strings(a) && strings(b) && length(a) == length(x) && length(b) == length(candidates),
        "preprocess must preserve lengths and return character vectors without NA.")
  if ("key" %in% mode) {
    check(is.function(key), "Supply a key function when using mode = 'key'.")
    ak <- key(a)
    bk <- key(b)
    check(is.character(ak) && is.character(bk) && length(ak) == length(a) && length(bk) == length(b),
          "key must return one character key per string; NA or empty keys are allowed and never match.")
  }
  allowed <- matrix(TRUE, length(a), length(b))
  if (!is.null(candidate_filter)) {
    allowed <- if (is.function(candidate_filter)) candidate_filter(x, candidates) else candidate_filter
    check(is.matrix(allowed) && is.logical(allowed) && !anyNA(allowed) &&
            identical(dim(allowed), c(length(a), length(b))),
          "candidate_filter must supply a logical query-by-candidate matrix without NA.")
  }
  if ("distance" %in% mode) {
    methods <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
    check(strings(method) && length(method) > 0L && all(method %in% methods), "Invalid stringdist method.")
    check(is.numeric(max_dist) && length(max_dist) > 0L && !anyNA(max_dist) && all(max_dist >= 0),
          "max_dist must contain nonnegative numbers.")
    if (!is.null(names(max_dist))) {
      check(!anyDuplicated(names(max_dist)) && all(method %in% names(max_dist)),
            "Named max_dist must contain every requested method.")
      limits <- unname(max_dist[method])
    } else {
      check(length(max_dist) %in% c(1L, length(method)), "max_dist must be scalar or have one value per method.")
      limits <- rep(max_dist, length.out = length(method))
    }
  }
  stages <- do.call(rbind, lapply(mode, function(m) {
    if (m == "distance") data.frame(mode = m, method = method, limit = limits)
    else data.frame(mode = m, method = NA_character_, limit = NA_real_)
  }))
  rownames(stages) <- NULL
  for (s in seq_len(nrow(stages))) {
    if (stages$mode[s] == "distance") {
      penalty <- setting(unmatched_cost, "distance", stages$method[s], stages$limit[s] + 1)
      check(is.finite(penalty), "An infinite max_dist requires an explicit finite unmatched_cost for that method.")
    }
  }
  nr <- length(a)
  nc <- length(b)
  out <- data.frame(query = x, index = rep(NA_integer_, nr), match = rep(NA_character_, nr),
                    score = rep(NA_real_, nr), n_best = integer(nr),
                    status = rep("unmatched", nr), reason = rep("no_candidates", nr),
                    mode = rep(NA_character_, nr), method = rep(NA_character_, nr),
                    stage = rep(NA_integer_, nr), best_index = rep(NA_integer_, nr),
                    best_score = rep(NA_real_, nr), runner_up_score = rep(NA_real_, nr),
                    best_substring = rep(NA_character_, nr), best_coverage = rep(NA_real_, nr),
                    gap = rep(NA_real_, nr), selected_rank = rep(NA_integer_, nr),
                    assignment_gap = rep(NA_real_, nr), shared_substring = rep(NA_character_, nr),
                    coverage = rep(NA_real_, nr), stringsAsFactors = FALSE)
  history <- list()
  finish <- function() {
    out$trace <- I(lapply(seq_len(nr), function(i) {
      rows <- lapply(history, function(h) h[h$query_index == i, , drop = FALSE])
      if (length(rows)) do.call(rbind, rows) else data.frame()
    }))
    attr(out, "stages") <- stages
    matched <- !is.na(out$index)
    summary <- sprintf(
      "match_strings(): %d/%d queries matched; %d unmatched (%d unresolved ties); %d/%d candidates used.",
      sum(matched), nr, sum(!matched), sum(out$status == "tie"),
      length(unique(out$index[matched])), nc
    )
    tie_breaks <- sum(out$status == "tie_first")
    if (tie_breaks > 0L) {
      summary <- paste0(summary, " Matches selected by tie-breaking: ", tie_breaks, ".")
    }
    message(summary)
    out
  }
  if (!nr || !nc) return(finish())
  near <- function(u, v) abs(u - v) <= tolerance * max(1, abs(u), abs(v))
  # Longest contiguous shared run. The first maximum in the query wins.
  shared <- function(u, v) {
    u <- strsplit(u, "", fixed = TRUE)[[1L]]
    v <- strsplit(v, "", fixed = TRUE)[[1L]]
    previous <- integer(length(v) + 1L)
    best <- end <- 0L
    for (k in seq_along(u)) {
      current <- c(0L, (previous[seq_along(v)] + 1L) * (u[k] == v))
      longest <- max(current)
      if (longest > best) { best <- longest; end <- k }
      previous <- current
    }
    list(length = best, text = if (best) paste0(u[seq.int(end - best + 1L, end)], collapse = "") else "")
  }
  used <- rep(FALSE, nc)
  for (s in seq_len(nrow(stages))) {
    pending <- which(is.na(out$index))
    if (!length(pending)) break
    m <- stages$mode[s]
    meth <- stages$method[s]
    aa <- a[pending]
    np <- length(pending)
    same <- function(u, v, empty_keys = FALSE) {
      yes <- outer(u, v, "==")
      yes[is.na(yes)] <- FALSE
      if (empty_keys) {
        yes[is.na(u) | !nzchar(u), ] <- FALSE
        yes[, is.na(v) | !nzchar(v)] <- FALSE
      }
      yes
    }
    common <- matrix(NA_character_, np, nc)
    coverage <- matrix(NA_real_, np, nc)
    if (m %in% c("exact", "key", "prefix")) {
      if (m == "exact") yes <- same(aa, b)
      if (m == "key") yes <- same(ak[pending], bk, empty_keys = TRUE)
      if (m == "prefix") {
        end <- start + n - 1
        yes <- same(substr(aa, start, end), substr(b, start, end))
        yes[nchar(aa) < end, ] <- FALSE
        yes[, nchar(b) < end] <- FALSE
      }
      score <- cost <- matrix(Inf, np, nc)
      score[yes] <- cost[yes] <- 0
      threshold <- yes
      penalty <- rep(setting(unmatched_cost, m, meth, 1), np)
    } else if (m == "distance") {
      brathering:::.ensure_packages(c("stringdist"))
      score <- cost <- stringdist::stringdistmatrix(aa, b, method = meth, ...)
      cost[!is.finite(cost)] <- Inf
      threshold <- is.finite(cost) & cost <= stages$limit[s]
      penalty <- rep(setting(unmatched_cost, m, meth, stages$limit[s] + 1), np)
    } else {
      score <- matrix(0, np, nc)
      for (r in seq_len(np)) for (j in seq_len(nc)) {
        z <- shared(aa[r], b[j])
        score[r, j] <- z$length
        common[r, j] <- z$text
      }
      shorter <- outer(nchar(aa), nchar(b), pmin)
      coverage <- score / pmax(1, shorter)
      cost <- nchar(aa) - score
      threshold <- score >= min_chars & coverage >= min_coverage
      penalty <- setting(unmatched_cost, m, meth, pmax(1, nchar(aa) - min_chars + 1))
      penalty <- rep(penalty, length.out = np)
    }
    # Confidence statistics include already-reserved candidates. Removing a
    # competitor must not manufacture a large gap or conceal an original tie.
    structure <- allowed[pending, , drop = FALSE]
    raw_cost <- cost
    raw_cost[!structure] <- Inf
    accepted <- structure & threshold & is.finite(cost)
    decision <- matrix(Inf, np, nc)
    log <- data.frame(query_index = pending, stage = s, mode = m, method = meth,
                      index = rep(NA_integer_, np), score = rep(NA_real_, np),
                      n_best = integer(np), status = rep("unmatched", np), reason = rep("no_match", np),
                      best_index = rep(NA_integer_, np), best_score = rep(NA_real_, np),
                      runner_up_score = rep(NA_real_, np), gap = rep(NA_real_, np),
                      best_substring = rep(NA_character_, np), best_coverage = rep(NA_real_, np),
                      selected_rank = rep(NA_integer_, np), assignment_gap = rep(NA_real_, np),
                      shared_substring = rep(NA_character_, np), coverage = rep(NA_real_, np),
                      available_count = integer(np), stringsAsFactors = FALSE)
    final_first <- ties == "first" && s == nrow(stages)
    for (r in seq_len(np)) {
      order_j <- order(raw_cost[r, ], seq_len(nc))
      order_j <- order_j[is.finite(raw_cost[r, order_j])]
      if (length(order_j)) {
        best <- raw_cost[r, order_j[1L]]
        log$best_index[r] <- order_j[1L]
        log$best_score[r] <- score[r, order_j[1L]]
        if (m == "substring") {
          log$best_substring[r] <- common[r, order_j[1L]]
          log$best_coverage[r] <- coverage[r, order_j[1L]]
        }
        log$n_best[r] <- sum(vapply(raw_cost[r, order_j], function(z) near(z, best), logical(1)))
        log$gap[r] <- Inf
        if (length(order_j) > 1L) {
          log$runner_up_score[r] <- score[r, order_j[2L]]
          log$gap[r] <- raw_cost[r, order_j[2L]] - best
        }
      }
      if (!any(structure[r, ])) { log$reason[r] <- "filtered"; next }
      if (!any(accepted[r, ])) {
        log$reason[r] <- if (m == "distance") "above_threshold" else if (m == "substring") "below_threshold" else "no_match"
        next
      }
      available <- accepted[r, ] & !used
      if (!any(available)) { log$reason[r] <- "candidate_taken"; next }
      available <- available & cost[r, ] < penalty[r]
      log$available_count[r] <- sum(available)
      if (!any(available)) { log$reason[r] <- "unmatched_cost"; next }
      if (log$n_best[r] > 1L && !final_first) {
        log$status[r] <- "tie"; log$reason[r] <- "ambiguous"; next
      }
      gap_floor <- if (m %in% c("distance", "substring")) setting(min_gap, m, meth, 0) else 0
      if (log$gap[r] < gap_floor) { log$reason[r] <- "low_margin"; next }
      decision[r, available] <- cost[r, available]
      log$reason[r] <- "candidate_taken"
    }
    selected <- rep(NA_integer_, np)
    global_tie <- rep(FALSE, np)
    if (reuse_candidates || assignment == "greedy") {
      for (r in seq_len(np)) {
        values <- decision[r, ]
        if (!reuse_candidates) values[used] <- Inf
        j <- which(is.finite(values))
        if (!length(j)) next
        win <- j[vapply(values[j], function(v) near(v, min(values[j])), logical(1))]
        if (length(win) > 1L && !final_first) {
          log$status[r] <- "tie"; log$reason[r] <- "ambiguous"; next
        }
        selected[r] <- win[1L]
        if (!reuse_candidates) used[selected[r]] <- TRUE
      }
    } else if (any(is.finite(decision))) {
      brathering:::.ensure_packages(c("clue"))
      # Each row has a private dummy candidate representing rejection. Scale
      # costs so a forbidden edge is more expensive than rejecting all rows.
      scale <- max(1, penalty, decision[is.finite(decision)])
      big <- np + 1
      real <- decision / scale
      real[!is.finite(real)] <- big
      dummy <- matrix(big, np, np)
      diag(dummy) <- penalty / scale
      weights <- cbind(real, dummy)
      solve <- function(w) {
        j <- as.integer(clue::solve_LSAP(w))
        list(index = j, value = sum(w[cbind(seq_len(np), j)]))
      }
      optimum <- solve(weights)
      total_tolerance <- tolerance * max(1 / scale, abs(optimum$value))
      for (r in seq_len(np)) {
        j <- optimum$index[r]
        alt <- weights
        alt[r, j] <- big
        alternative <- solve(alt)
        delta <- max(0, alternative$value - optimum$value)
        global_tie[r] <- delta <= total_tolerance
        if (j <= nc && is.finite(decision[r, j])) {
          # An infeasible alternative means this edge is forced.
          log$assignment_gap[r] <- if (any(alt[cbind(seq_len(np), alternative$index)] >= big)) Inf else delta * scale
        }
      }
      if (final_first && any(global_tie)) {
        # Among equal optima, choose lexicographically by query order and
        # candidate index; consider the unmatched option after real candidates.
        fixed <- weights
        chosen <- optimum
        for (r in seq_len(np)) {
          options <- which(fixed[r, ] < big)
          options <- options[options < chosen$index[r]]
          for (j in options) {
            trial <- fixed
            trial[r, ] <- big
            trial[r, j] <- fixed[r, j]
            proposed <- solve(trial)
            if (abs(proposed$value - optimum$value) <= total_tolerance) {
              chosen <- proposed
              break
            }
          }
          j <- chosen$index[r]
          fixed[r, -j] <- big
        }
        optimum <- chosen
      }
      for (r in seq_len(np)) {
        j <- optimum$index[r]
        if (global_tie[r] && !final_first) {
          log$status[r] <- "tie"; log$reason[r] <- "ambiguous_assignment"
        } else if (j <= nc && is.finite(decision[r, j])) {
          selected[r] <- j
          if (global_tie[r]) log$assignment_gap[r] <- 0
        }
      }
      used[selected[!is.na(selected)]] <- TRUE
    }
    for (r in which(!is.na(selected))) {
      j <- selected[r]
      log$index[r] <- j
      log$score[r] <- score[r, j]
      log$selected_rank[r] <- 1L + sum(raw_cost[r, ] < cost[r, j] &
                                       abs(raw_cost[r, ] - cost[r, j]) > tolerance * max(1, abs(cost[r, j])))
      log$status[r] <- if (log$n_best[r] > 1L || global_tie[r]) "tie_first" else "matched"
      log$reason[r] <- "accepted"
      if (m == "substring") {
        log$shared_substring[r] <- common[r, j]
        log$coverage[r] <- coverage[r, j]
      }
      out$index[pending[r]] <- j
      out$match[pending[r]] <- candidates[j]
    }
    fields <- intersect(names(out), names(log))
    out[pending, fields] <- log[, fields, drop = FALSE]
    history[[length(history) + 1L]] <- log
  }
  finish()
}
