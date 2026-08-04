#' Align Delimited Fields Across Strings
#'
#' Splits each input string into fields and pads corresponding fields with
#' spaces so that they align vertically when printed in a monospaced font.
#' Strings may contain different numbers of fields.
#'
#' @param x A character vector containing the strings to align.
#' @param field_separator A character string containing the regular expression
#'   used to separate fields. Defaults to `"\\s+"`, which matches one or more
#'   whitespace characters.
#' @param output_separator A character string inserted between fields in the
#'   aligned output. Defaults to a single space.
#'
#' @return A character vector with the same length as `x`, containing the
#'   aligned strings.
#'
#' @details
#' Field widths are calculated separately for each field position using their
#' displayed widths. Missing trailing fields are supported and do not produce
#' trailing separators. Unnecessary spaces after the final field of each string
#' are removed.
#'
#' Because `field_separator` is interpreted as a regular expression, special
#' regular-expression characters must be escaped when they should be matched
#' literally.
#'
#' @examples
#' x <- c(
#'   "InfluenzaA M GILGFVFTL TRB",
#'   "EBV BRLF1 YVLDHLIVV TRB",
#'   "CMV pp65 NLVPMVATV TRB",
#'   "Example with three"
#' )
#'
#' cat(align_fields(x), sep = "\n")
#'
#' comma_separated <- c(
#'   "a,b,c",
#'   "long,bb",
#'   "medium,b,ccc,d"
#' )
#'
#' cat(
#'   align_fields(comma_separated, field_separator = ","),
#'   sep = "\n"
#' )
#'
#' @export
align_fields <- function(x,
                         field_separator = "\\s+",
                         output_separator = " ") {
    if (!length(x)) {
        return(character())
    }

    # Split each string into fields. field_separator is a regular expression.
    fields <- strsplit(trimws(x), field_separator, perl = TRUE)

    max_fields <- max(lengths(fields))

    # Find the displayed width required for each field position.
    widths <- vapply(seq_len(max_fields), function(i) {
        values <- vapply(
            fields,
            function(z) if (length(z) >= i) z[[i]] else "",
            character(1)
        )
        max(nchar(values, type = "width"))
    }, integer(1))

    # Pad each field and rebuild the strings.
    vapply(fields, function(z) {
        if (!length(z)) {
            return("")
        }

        padded <- vapply(seq_along(z), function(i) {
            paste0(
                z[[i]],
                strrep(" ", widths[[i]] - nchar(z[[i]], type = "width"))
            )
        }, character(1))

        # Remove unnecessary padding after the row's final field.
        sub(" +$", "", paste(padded, collapse = output_separator))
    }, character(1))
}
