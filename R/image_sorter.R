#!/usr/bin/env Rscript

# Load without launching:
#   source("image_sorter.R")
#   image_sorter("images", "keep", "reject", rate = 2, select_key = "a")
#
# Or run from a terminal:
#   Rscript image_sorter.R --input ./images --folder-a ./keep \
#     --folder-b ./reject --rate 2 --select-key a --mode move

#' Sort images into two folders using timed keyboard selection
#'
#' Display local images in a Shiny app at a requested rate. A single press of
#' the selection key, or a click on the selection button, marks the current
#' image for folder A. Images not selected go to folder B.
#'
#' @param input_dir Character scalar. Existing directory containing the images.
#'   Defaults to \code{"images"} relative to the R working directory.
#' @param folder_a Character scalar. Destination for selected images.
#'   Created, including missing parent directories, if necessary.
#' @param folder_b Character scalar. Destination for unselected images.
#'   Created, including missing parent directories, if necessary.
#' @param rate Numeric scalar greater than zero and at most 60. Requested images
#'   per second; fractional values are allowed, for example \code{0.5}.
#' @param select_key Character scalar. A single printable character, such as
#'   \code{"a"} or \code{"7"}, or a browser keyboard key name:
#'   \code{"Space"}, \code{"Enter"}, \code{"ArrowLeft"}, \code{"ArrowRight"},
#'   \code{"ArrowUp"}, \code{"ArrowDown"}, \code{"Escape"}, \code{"Tab"},
#'   \code{"Backspace"}, \code{"Delete"}, \code{"Insert"}, \code{"Home"},
#'   \code{"End"}, \code{"PageUp"}, \code{"PageDown"}, or \code{"F1"} through
#'   \code{"F24"}. Matching ignores letter case. \code{"Space"},
#'   \code{"Spacebar"}, and a literal space select the space bar. Ctrl, Alt,
#'   and Meta key combinations are ignored. Letters are recommended because
#'   browsers or operating systems may reserve special keys.
#' @param mode Character scalar, either \code{"move"} (the default) or
#'   \code{"copy"}. Copy mode leaves the original images in place.
#' @param recursive Logical scalar. Whether to include input subdirectories.
#'   Images already within either destination directory are excluded.
#' @param host Character scalar. Address on which Shiny listens. The default
#'   \code{"127.0.0.1"} keeps the app local to this computer.
#' @param port Numeric scalar. Whole-number port from 1 through 65535.
#' @param launch_browser Logical scalar. Whether to open the app in a browser.
#'
#' @details
#' Install the \pkg{shiny} package before calling this function. Sourcing this
#' file only defines functions: it does not start the app, inspect image
#' directories, or move files. Call \code{image_sorter()} to start the app.
#'
#' The app starts paused. Press Start to begin automatic advancement. Selection
#' remains latched for the current image after the key or button is released,
#' and resets for every new image. Holding the selection key does not select
#' later images.
#' The on-screen button always works, regardless of \code{select_key}.
#' A selection made while paused is retained when playback resumes.
#'
#' At the end of each interval the selected image is moved or copied to A;
#' otherwise it goes to B. Selecting an image does not advance it immediately.
#' Pausing leaves the current image unsorted; resuming starts a fresh interval.
#' Keep the browser tab visible and focused. Timing is approximate: rendering,
#' file operations, and browser/server latency can reduce the effective rate.
#'
#' Supported extensions are JPG, JPEG, PNG, GIF, WebP, and BMP, subject to
#' browser support. The queue is a sorted snapshot of paths taken at launch.
#' Output files use their original basenames; existing names receive a numbered
#' suffix. Recursive input subdirectory structure is not reproduced in outputs.
#' Destination directories must differ from one another and from the input
#' directory. Test with \code{mode = "copy"} before moving important files.
#'
#' Transfer failures are shown in the activity log and processing continues
#' with the next image. The function blocks the R console until Stop is clicked,
#' the browser session ends, or execution is interrupted in R. Stop leaves the
#' current and remaining images unsorted; previous transfers are not undone.
#' This is a local, single-operator app: do not sort the same queue in multiple
#' browser tabs or concurrent instances.
#'
#' Command-line use is also supported, for example:
#' \preformatted{
#' Rscript image_sorter.R --input ./images --rate 2 --select-key a --mode copy
#' }
#' Use \code{Rscript image_sorter.R --help} for all command-line options.
#'
#' @returns Invisibly \code{NULL} after the app stops. The function performs
#'   file-system side effects according to \code{mode}; it does not return the
#'   images or an activity table.
#' @seealso \code{\link[shiny]{runApp}},
#'   \url{https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key}
#' @examples
#' \dontrun{
#' # For standalone use, load this file once:
#' source("image_sorter.R")
#'
#' # Tap A once to select the visible image; otherwise it goes to reject.
#' image_sorter(
#'   input_dir = "images",
#'   folder_a = "keep",
#'   folder_b = "reject",
#'   rate = 3,
#'   select_key = "a",
#'   mode = "copy"
#' )
#'
#' # Use Enter instead, at one image every two seconds.
#' image_sorter(
#'   input_dir = "images",
#'   folder_a = "keep",
#'   folder_b = "reject",
#'   rate = 0.5,
#'   select_key = "Enter",
#'   mode = "copy",
#'   recursive = TRUE
#' )
#' }
#' @export
image_sorter <- function(input_dir = "images",
                         folder_a = "folder_a",
                         folder_b = "folder_b",
                         rate = 2,
                         select_key = "Space",
                         mode = c("move", "copy"),
                         recursive = FALSE,
                         host = "127.0.0.1",
                         port = 3838L,
                         launch_browser = TRUE) {
    scalar_string <- function(value) {
        is.character(value) && length(value) == 1L &&
            !is.na(value) && nzchar(value)
    }

    for (argument in c("input_dir", "folder_a", "folder_b", "select_key", "host")) {
        if (!scalar_string(get(argument, inherits = FALSE))) {
            stop(argument, " must be a nonempty character scalar.", call. = FALSE)
        }
    }
    if (!is.numeric(rate) || length(rate) != 1L || !is.finite(rate) ||
        rate <= 0 || rate > 60) {
        stop("rate must be a number greater than 0 and no more than 60.", call. = FALSE)
    }
    if (!is.numeric(port) || length(port) != 1L || !is.finite(port) ||
        port != floor(port) || port < 1 || port > 65535) {
        stop("port must be a whole number from 1 to 65535.", call. = FALSE)
    }
    for (argument in c("recursive", "launch_browser")) {
        value <- get(argument, inherits = FALSE)
        if (!is.logical(value) || length(value) != 1L || is.na(value)) {
            stop(argument, " must be TRUE or FALSE.", call. = FALSE)
        }
    }
    mode <- match.arg(mode)
    port <- as.integer(port)

    named_keys <- c(
        "space", "spacebar", "enter", "arrowleft", "arrowright", "arrowup",
        "arrowdown", "escape", "tab", "backspace", "delete", "insert",
        "home", "end", "pageup", "pagedown", paste0("f", 1:24)
    )
    printable_key <- nchar(select_key, type = "chars") == 1L &&
        !grepl("[[:cntrl:]]", select_key)
    if (!printable_key && !tolower(select_key) %in% named_keys) {
        stop(
            "select_key must be one printable character or a supported key name ",
            "(for example 'Space', 'Enter', or 'ArrowRight').",
            call. = FALSE
        )
    }
    if (tolower(select_key) %in% c("space", "spacebar")) select_key <- " "
    key_label <- if (identical(select_key, " ")) "Space" else select_key

    require_package <- function(package) {
        if (!requireNamespace(package, quietly = TRUE)) {
            stop(
                "Package '", package, "' is required. Install it with:\n",
                "  install.packages(\"", package, "\")",
                call. = FALSE
            )
        }
    }

    absolute_path <- function(path, must_work = FALSE) {
        path <- path.expand(path)
        if (must_work && !dir.exists(path)) {
            stop("Folder does not exist: ", path, call. = FALSE)
        }
        normalizePath(path, winslash = "/", mustWork = must_work)
    }

    path_is_inside <- function(path, directory) {
        path <- paste0(normalizePath(path, winslash = "/", mustWork = FALSE), "/")
        directory <- paste0(normalizePath(directory, winslash = "/", mustWork = FALSE), "/")
        startsWith(path, directory)
    }

    unique_destination <- function(directory, filename) {
        candidate <- file.path(directory, filename)
        if (!file.exists(candidate)) return(candidate)

        extension <- tools::file_ext(filename)
        stem <- if (nzchar(extension)) {
            substr(filename, 1L, nchar(filename) - nchar(extension) - 1L)
        } else {
            filename
        }

        number <- 1L
        repeat {
            suffix <- sprintf("_%03d", number)
            renamed <- if (nzchar(extension)) {
                paste0(stem, suffix, ".", extension)
            } else {
                paste0(stem, suffix)
            }
            candidate <- file.path(directory, renamed)
            if (!file.exists(candidate)) return(candidate)
            number <- number + 1L
        }
    }

    transfer_file <- function(source, destination, mode) {
        if (identical(mode, "copy")) {
            ok <- file.copy(source, destination, overwrite = FALSE, copy.mode = TRUE)
            if (!ok) stop("Could not copy file.")
            return(invisible(destination))
        }

        # file.rename is fast on one filesystem. Copy + remove handles destinations
        # on a different filesystem.
        if (!file.rename(source, destination)) {
            copied <- file.copy(source, destination, overwrite = FALSE, copy.mode = TRUE)
            if (!copied) stop("Could not move or copy file.")
            if (!file.remove(source)) {
                file.remove(destination)
                stop("Copied the file but could not remove the source; rolled back the copy.")
            }
        }
        invisible(destination)
    }

    content_type <- function(path) {
        extension <- tolower(tools::file_ext(path))
        switch(
            extension,
            jpg = "image/jpeg",
            jpeg = "image/jpeg",
            png = "image/png",
            gif = "image/gif",
            webp = "image/webp",
            bmp = "image/bmp",
            "application/octet-stream"
        )
    }

    require_package("shiny")

    input_dir <- absolute_path(input_dir, must_work = TRUE)
    folder_a <- absolute_path(folder_a)
    folder_b <- absolute_path(folder_b)

    if (identical(folder_a, folder_b)) {
        stop("folder_a and folder_b must be different folders.", call. = FALSE)
    }
    if (identical(input_dir, folder_a) || identical(input_dir, folder_b)) {
        stop("Destination folders cannot be the input folder.", call. = FALSE)
    }

    dir.create(folder_a, recursive = TRUE, showWarnings = FALSE)
    dir.create(folder_b, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(folder_a) || !dir.exists(folder_b)) {
        stop("Could not create one or both destination folders.", call. = FALSE)
    }
    folder_a <- absolute_path(folder_a, must_work = TRUE)
    folder_b <- absolute_path(folder_b, must_work = TRUE)
    if (identical(folder_a, folder_b)) {
        stop("folder_a and folder_b must be different folders.", call. = FALSE)
    }

    image_pattern <- "\\.(jpe?g|png|gif|webp|bmp)$"
    images <- list.files(
        input_dir,
        pattern = image_pattern,
        full.names = TRUE,
        recursive = recursive,
        ignore.case = TRUE
    )

    # If destinations live under the input directory, never put their existing
    # contents into this run's queue.
    images <- images[
        !vapply(images, path_is_inside, logical(1), directory = folder_a) &
            !vapply(images, path_is_inside, logical(1), directory = folder_b)
    ]
    images <- images[!dir.exists(images)]
    images <- sort(normalizePath(images, winslash = "/", mustWork = TRUE))

    js <- "
  $(function() {
    const selectionKey = $('#selectA').attr('data-select-key').toLowerCase();
    let currentIndex = null;
    let selected = false;
    let presses = 0;

    Shiny.addCustomMessageHandler('current_image', function(message) {
      currentIndex = message.active ? message.index : null;
      selected = false;
      $('#selectA').removeClass('selected').attr('aria-pressed', 'false')
        .prop('disabled', currentIndex === null).text('SELECT FOLDER A');
    });

    function selectCurrentImage() {
      if (currentIndex === null || selected) return;
      selected = true;
      // Include the image index so a delayed press cannot select the next image.
      Shiny.setInputValue('select_a', {index: currentIndex, press: ++presses},
        {priority: 'event'});
      $('#selectA').addClass('selected').attr('aria-pressed', 'true')
        .text('SELECTED: FOLDER A');
    }

    function isSelectionKey(event) {
      const keyEvent = event.originalEvent || event;
      return !keyEvent.ctrlKey && !keyEvent.altKey && !keyEvent.metaKey &&
        !keyEvent.isComposing && typeof keyEvent.key === 'string' &&
        keyEvent.key.toLowerCase() === selectionKey;
    }

    $(document).on('keydown', function(event) {
      if (isSelectionKey(event)) {
        event.preventDefault();
        const keyEvent = event.originalEvent || event;
        if (!keyEvent.repeat) selectCurrentImage();
      }
    });
    $(document).on('keyup', function(event) {
      if (isSelectionKey(event)) event.preventDefault();
    });
    $('#selectA').on('click', selectCurrentImage);
  });
  "

    ui <- shiny::fluidPage(
        shiny::tags$head(
            shiny::tags$title("Timed image sorter"),
            shiny::tags$style(shiny::HTML("
        body { background: #111827; color: #f9fafb; }
        .container-fluid { max-width: 1100px; padding: 20px; }
        .panel { background: #1f2937; border-radius: 12px; padding: 16px; margin-bottom: 14px; }
        .image-frame { height: min(65vh, 680px); display: flex; align-items: center;
                       justify-content: center; background: #030712; border-radius: 10px;
                       overflow: hidden; }
        .image-frame img { max-width: 100%; max-height: min(65vh, 680px);
                           object-fit: contain; }
        #selectA { width: 100%; min-height: 88px; font-size: 24px; font-weight: 700;
                 color: #fff; background: #2563eb; border: 0; border-radius: 10px;
                 touch-action: none; user-select: none; }
        #selectA.selected { background: #16a34a; }
        .status-a { color: #86efac; font-weight: 700; }
        .status-b { color: #fca5a5; font-weight: 700; }
        .muted { color: #9ca3af; }
        .btn { margin-right: 8px; }
        #progress { margin-top: 8px; }
      ")),
            shiny::tags$script(shiny::HTML(js))
        ),
        shiny::fluidRow(
            shiny::column(
                8,
                shiny::div(class = "panel image-frame", shiny::imageOutput("image", height = "100%"))
            ),
            shiny::column(
                4,
                shiny::div(
                    class = "panel",
                    shiny::h3("Timed image sorter"),
                    shiny::uiOutput("state"),
                    shiny::div(class = "muted", shiny::textOutput("filename")),
                    shiny::uiOutput("progress"),
                    shiny::hr(),
                    shiny::actionButton("toggle", "Start", class = "btn-success"),
                    shiny::actionButton("stop", "Stop", class = "btn-danger"),
                    shiny::hr(),
                    shiny::tags$button(id = "selectA", type = "button", disabled = "disabled",
                                       `aria-pressed` = "false", `data-select-key` = select_key,
                                       "SELECT FOLDER A"),
                    shiny::p(class = "muted", style = "margin-top: 10px;",
                             paste0("Click once or tap ", key_label,
                                    " to select this image for A. No press = B.")),
                    shiny::hr(),
                    shiny::strong("Speed: "), paste0(rate, " image(s)/second"), shiny::br(),
                    shiny::strong("Operation: "), mode, shiny::br(),
                    shiny::strong("Folder A: "), shiny::tags$span(title = folder_a, basename(folder_a)), shiny::br(),
                    shiny::strong("Folder B: "), shiny::tags$span(title = folder_b, basename(folder_b))
                ),
                shiny::div(class = "panel", shiny::verbatimTextOutput("log", placeholder = TRUE))
            )
        )
    )

    server <- function(input, output, session) {
        total <- length(images)
        current_index <- shiny::reactiveVal(1L)
        running <- shiny::reactiveVal(FALSE)
        selected_for_a <- shiny::reactiveVal(FALSE)
        next_tick <- shiny::reactiveVal(as.POSIXct(NA_real_, origin = "1970-01-01"))
        count_a <- shiny::reactiveVal(0L)
        count_b <- shiny::reactiveVal(0L)
        count_errors <- shiny::reactiveVal(0L)
        messages <- shiny::reactiveVal(character())

        add_message <- function(message) {
            new_messages <- c(messages(), message)
            messages(tail(new_messages, 8L))
        }

        current_file <- shiny::reactive({
            index <- current_index()
            if (index < 1L || index > total) return(NULL)
            images[[index]]
        })

        shiny::observe({
            index <- current_index()
            session$sendCustomMessage("current_image", list(index = index, active = index <= total))
        })

        shiny::observeEvent(input$select_a, {
            press <- input$select_a
            if (current_index() <= total && isTRUE(press$index == current_index())) {
                selected_for_a(TRUE)
            }
        }, ignoreNULL = TRUE, priority = 10)

        shiny::observeEvent(input$toggle, {
            if (total == 0L || current_index() > total) return()
            new_state <- !running()
            running(new_state)
            if (new_state) {
                next_tick(Sys.time() + (1 / rate))
                shiny::updateActionButton(session, "toggle", label = "Pause")
            } else {
                shiny::updateActionButton(session, "toggle", label = "Resume")
            }
        })

        shiny::observeEvent(input$stop, {
            running(FALSE)
            selected_for_a(FALSE)
            shiny::stopApp()
        })

        # Poll more frequently than the requested frame interval so reactive updates
        # do not cause a visible burst when the UI changes.
        shiny::observe({
            shiny::invalidateLater(max(10L, min(100L, floor(500 / rate))), session)
            if (!running()) return()
            due <- next_tick()
            if (is.na(due) || Sys.time() < due) return()

            source <- current_file()
            if (is.null(source)) {
                running(FALSE)
                return()
            }

            route_a <- selected_for_a()
            destination_dir <- if (route_a) folder_a else folder_b
            route_name <- if (route_a) "A" else "B"
            destination <- unique_destination(destination_dir, basename(source))

            tryCatch({
                transfer_file(source, destination, mode)
                if (route_a) count_a(count_a() + 1L) else count_b(count_b() + 1L)
                add_message(sprintf("%s -> %s", basename(source), route_name))
            }, error = function(error) {
                count_errors(count_errors() + 1L)
                add_message(sprintf("ERROR: %s (%s)", basename(source), conditionMessage(error)))
            })

            new_index <- current_index() + 1L
            selected_for_a(FALSE)
            current_index(new_index)
            if (new_index > total) {
                running(FALSE)
                shiny::updateActionButton(session, "toggle", label = "Finished")
                add_message("Finished processing the queue.")
            } else {
                next_tick(Sys.time() + (1 / rate))
            }
        })

        output$image <- shiny::renderImage({
            source <- current_file()
            if (is.null(source) || !file.exists(source)) return(NULL)
            list(
                src = source,
                contentType = content_type(source),
                alt = basename(source)
            )
        }, deleteFile = FALSE)

        output$filename <- shiny::renderText({
            source <- current_file()
            if (is.null(source)) "No image remaining" else basename(source)
        })

        output$state <- shiny::renderUI({
            if (total == 0L) return(shiny::tags$p("No supported images found."))
            if (current_index() > total) return(shiny::tags$p("Finished."))
            status <- if (running()) "" else "Paused — "
            if (selected_for_a()) {
                shiny::tags$p(class = "status-a", paste0(status, "SELECTED: current image goes to folder A"))
            } else {
                shiny::tags$p(class = "status-b", paste0(status, "NOT SELECTED: current image goes to folder B"))
            }
        })

        output$progress <- shiny::renderUI({
            processed <- count_a() + count_b()
            shiny::tagList(
                shiny::tags$strong(sprintf("Processed %d of %d", processed, total)),
                shiny::tags$br(),
                sprintf("A: %d | B: %d | errors: %d", count_a(), count_b(), count_errors())
            )
        })

        output$log <- shiny::renderText({
            if (length(messages()) == 0L) "Activity will appear here." else paste(messages(), collapse = "\n")
        })

        session$onSessionEnded(function() shiny::stopApp())
    }

    cat(sprintf(
        "Found %d image(s). Open http://%s:%d in a browser.\n",
        length(images), host, port
    ))

    shiny::runApp(
        shiny::shinyApp(ui = ui, server = server),
        host = host,
        port = port,
        launch.browser = launch_browser
    )
    invisible(NULL)
}

# Internal CLI adapter; source() does not invoke it.
.image_sorter_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
    usage <- paste(
        "Usage:",
        "  Rscript image_sorter.R [options]",
        "",
        "Options:",
        "  --input PATH       Folder containing images (default: images)",
        "  --folder-a PATH    Destination for selected images (default: folder_a)",
        "  --folder-b PATH    Destination for unselected images (default: folder_b)",
        "  --rate N           Images per second; may be fractional (default: 2)",
        "  --select-key KEY   Selection key, e.g. a, Enter, Space (default: Space)",
        "  --mode move|copy   Move or copy source files (default: move)",
        "  --recursive        Include images in input subfolders",
        "  --host HOST        Shiny bind address (default: 127.0.0.1)",
        "  --port PORT        Shiny port (default: 3838)",
        "  --no-browser       Do not automatically open a browser",
        "  --help             Show this help",
        "",
        "From R:",
        "  source(\"image_sorter.R\")",
        "  image_sorter(\"images\", \"keep\", \"reject\", select_key = \"a\")",
        sep = "\n"
    )
    if (any(args %in% c("--help", "-h"))) {
        cat(usage, "\n")
        return(invisible(NULL))
    }

    value_options <- c(
        "--input" = "input_dir",
        "--folder-a" = "folder_a",
        "--folder-b" = "folder_b",
        "--rate" = "rate",
        "--select-key" = "select_key",
        "--mode" = "mode",
        "--host" = "host",
        "--port" = "port"
    )
    options <- list()
    index <- 1L
    while (index <= length(args)) {
        argument <- args[[index]]
        if (identical(argument, "--recursive")) {
            options$recursive <- TRUE
            index <- index + 1L
            next
        }
        if (identical(argument, "--no-browser")) {
            options$launch_browser <- FALSE
            index <- index + 1L
            next
        }
        if (!argument %in% names(value_options)) {
            stop("Unknown option: ", argument, "\n\n", usage, call. = FALSE)
        }
        if (index == length(args) || startsWith(args[[index + 1L]], "--")) {
            stop("Missing value for ", argument, call. = FALSE)
        }
        options[[value_options[[argument]]]] <- args[[index + 1L]]
        index <- index + 2L
    }

    for (argument in intersect(c("rate", "port"), names(options))) {
        options[[argument]] <- suppressWarnings(as.numeric(options[[argument]]))
    }
    do.call(image_sorter, options)
}

if (sys.nframe() == 0L && !interactive()) {
    .image_sorter_cli()
}
