#' Add tags to files conveniently
#' Features: Choose or add tags interactively;
#' Colored tag display (green = assigned);
#' Option to skip or append to already-tagged videos;
#' Stores results in a CSV
#'
#' @param filevector vector of filenames
#' @param output_file output csv file
#'
#' @returns nothing, file on disk
#' @export
#'
#' @examples
tagging_system <- function(filevector, output_file = "tags.csv") {

    videos <- filevector
    # Load existing data if available
    if (file.exists(output_file)) {
        tag_data <- read.csv(output_file, stringsAsFactors = FALSE)
    } else {
        tag_data <- data.frame(video = character(), tags = character(), stringsAsFactors = FALSE)
    }

    # Extract all known tags
    available_tags <- unique(unlist(strsplit(paste(tag_data$tags, collapse = ","), ",")))
    available_tags <- sort(available_tags[available_tags != ""])

    prev_file <- file.path(tempdir(), "tagging_temp_file.csv")
    write.csv(tag_data, prev_file, row.names = FALSE)

    # -----------------------------
    # ASK WHETHER TO SKIP OR APPEND EXISTING VIDEOS
    # -----------------------------
    cat("\nSome videos may already be tagged.\n")
    cat("Would you like to:\n")
    cat("[1] Skip existing videos\n")
    cat("[2] Append new tags to existing videos\n")
    repeat {
        choice <- trimws(readline("Enter choice (1 or 2): "))
        if (choice %in% c("1", "2")) {
            skip_existing <- (choice == "1")
            break
        } else {
            cat("Invalid choice. Please enter 1 or 2.\n")
        }
    }

    # -----------------------------
    # MAIN TAGGING LOOP
    # -----------------------------
    stop_tagging <- FALSE  # control flag for graceful exit

    for (i in seq_along(videos)) {
        video_name <- videos[i]
        cat("\n====================================\n")
        cat("Video:", video_name, "\n")
        cat("====================================\n")

        # If video already exists in data
        if (video_name %in% tag_data$video) {
            existing_tags <- unique(unlist(strsplit(tag_data$tags[tag_data$video == video_name], ",")))
            if (skip_existing) {
                cat("Already tagged. Skipping...\n")
                next
            } else {
                cat("Existing tags:", paste(existing_tags, collapse = ", "), "\n")
            }
        } else {
            existing_tags <- character()
        }

        current_tags <- sort(existing_tags)

        repeat {
            # Show available tags with color coding
            # if (length(available_tags) > 0) {
            #     cat("\nAvailable tags:\n")
            #     for (j in seq_along(available_tags)) {
            #         tag_name <- available_tags[j]
            #         if (tag_name %in% current_tags) {
            #             cat(sprintf("[%d] %s\n", j, crayon::green$bold(tag_name)))  # already assigned
            #         } else {
            #             cat(sprintf("[%d] %s\n", j, crayon::red(tag_name)))      # not yet assigned
            #         }
            #     }
            # } else {
            #     cat("\nNo tags available yet.\n")
            # }

            # Pretty-print tags in multiple columns with aligned formatting
            max_per_col <- 15
            n_tags <- length(available_tags)
            n_cols <- ceiling(n_tags / max_per_col)

            cat("\nAvailable tags:\n")

            if (n_tags == 0) {
                cat("  (none yet)\n")
            } else {
                # Prepare all tag labels first (without color) to compute width
                labels_plain <- sprintf("[%d] %s", seq_along(available_tags), available_tags)
                max_width <- max(nchar(labels_plain)) + 2  # +2 for padding

                for (row in 1:max_per_col) {
                    line_elems <- c()
                    for (col in 0:(n_cols - 1)) {
                        j <- row + col * max_per_col
                        if (j <= n_tags) {
                            tag_name <- available_tags[j]
                            # Color-coded tag text
                            if (tag_name %in% current_tags) {
                                label <- sprintf("[%d] %s", j, crayon::green$bold(tag_name))
                            } else {
                                label <- sprintf("[%d] %s", j, crayon::red(tag_name))
                            }
                            # Pad label (padding added based on plain length to avoid ANSI color width issues)
                            plain_len <- nchar(labels_plain[j])
                            spaces <- paste(rep(" ", max_width - plain_len), collapse = "")
                            line_elems <- c(line_elems, paste0(label, spaces))
                        }
                    }
                    cat(paste(line_elems, collapse = ""), "\n")
                }
            }


            # Menu options
            cat("\nOptions:\n")
            cat("[n] Add new tag\n")
            cat("[d] Done tagging this video\n")
            cat("[q] Quit program (save and exit loop)\n")
            cat("Select tag number or option: ")

            choice <- trimws(readline())

            if (choice == "q") {
                cat("\nStopping tagging after saving progress...\n")

                # Update tag data
                tag_data <- update_tag_data(
                    tag_data = tag_data,
                    video_name = video_name,
                    current_tags = current_tags)

                write.csv(tag_data, output_file, row.names = FALSE)
                cat("Progress saved to", output_file, "\n")
                stop_tagging <- TRUE
                break
            } else if (choice == "d") {
                break
            } else if (choice == "n") {
                new_tag <- trimws(readline("Enter new tag name: "))
                if (nchar(new_tag) > 0) {
                    available_tags <- unique(c(available_tags, new_tag))
                    current_tags <- unique(c(current_tags, new_tag))
                    cat("Added new tag:", green$bold(new_tag), "\n")
                }
            } else if (suppressWarnings(!is.na(as.numeric(choice)))) {
                idx <- as.numeric(choice)
                if (idx > 0 && idx <= length(available_tags)) {
                    tag <- available_tags[idx]
                    current_tags <- unique(c(current_tags, tag))
                    cat("Added tag:", green$bold(tag), "\n")
                } else {
                    cat("Invalid number.\n")
                }
            } else {
                cat("Invalid input.\n")
            }
        }

        # If user chose to quit, stop main loop
        if (stop_tagging) break

        # Update tag data
        tag_data <- update_tag_data(
          tag_data = tag_data,
          video_name = video_name,
          current_tags = current_tags)

        # Save CSV
        # done after loop iteration
        write.csv(tag_data, output_file, row.names = FALSE)
        cat("\nTags saved for", video_name, ":", paste(current_tags, collapse = ", "), "\n")
    }

    # cat("\nRestore previous file?\n")
    # cat("Would you like to:\n")
    # cat("[1] Skip existing videos\n")
    # cat("[2] Append new tags to existing videos\n")
    # repeat {
    #     choice <- trimws(readline("Enter choice (1 or 2): "))
    #     if (choice %in% c("1", "2")) {
    #         skip_existing <- (choice == "1")
    #         break
    #     } else {
    #         cat("Invalid choice. Please enter 1 or 2.\n")
    #     }
    # }

    cat("\n Tagging complete. Progress saved to", output_file, "\n")
}


update_tag_data <- function(tag_data, video_name, current_tags) {
    if (video_name %in% tag_data$video) {
        tag_data$tags[tag_data$video == video_name] <- paste(unique(current_tags), collapse = ",")
    } else {
        tag_data <- rbind(tag_data, data.frame(video = video_name, tags = paste(current_tags, collapse = ",")))
    }
    return(tag_data)
}
