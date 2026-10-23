#' Encode text using Huffman coding
#'
#' Build a Huffman codebook from character frequencies and encode the input text
#' using that codebook.
#'
#' @param text A single, non-missing, non-empty character string to encode.
#'
#' @details
#' The algorithm repeatedly combines the two nodes with the smallest frequencies
#' into a parent node. Traversing the resulting tree assigns a zero to each left
#' branch and a one to each right branch.
#'
#' The resulting codes are prefix-free: no character's code is the beginning of
#' another character's code. If the input contains only one distinct character,
#' that character receives the code \code{"0"}.
#'
#' The encoded result stores binary digits as text, rather than packing them into
#' bytes. Keep the codebook to decode the message later.
#'
#' @return A list with two components:
#' \describe{
#'   \item{codes}{A named character vector whose names are the original symbols
#'     and whose values are their binary codes.}
#'   \item{encoded}{A single character string containing the encoded binary digits.}
#' }
#'
#' @seealso \code{\link{decode_huffman}} for decoding a message.
#'
#' @examples
#' result <- huffman("BANANA")
#' result$codes
#' result$encoded
#' decode_huffman(result$encoded, result$codes)
#'
#' # A single distinct character receives the code "0".
#' huffman("AAA")
#'
#' @importFrom stats setNames
#' @export
huffman <- function(text) {

    # Split the input text into individual characters.
    # For "BANANA", this gives:
    # "B", "A", "N", "A", "N", "A"
    chars <- strsplit(text, "")[[1]]

    # Stop if there are no characters to encode.
    if (!length(chars)) stop("Text must not be empty.")

    # Count how often each character appears.
    # For "BANANA": A appears 3 times, B once, and N twice.
    freq <- table(chars)

    # Create one leaf node for each distinct character.
    #
    # Each node is an R list containing:
    # - weight: how often the character appears
    # - symbol: the character itself
    #
    # lapply() collects these nodes into a list.
    nodes <- lapply(names(freq), function(s) {
        list(
            weight = as.numeric(freq[s]),
            symbol = s
        )
    })

    # Build the Huffman tree.
    # Each iteration combines two nodes into one parent.
    # Continue until only the root of the complete tree remains.
    while (length(nodes) > 1) {

        # Extract the weight of each node.
        # order() returns the positions that sort those weights
        # from smallest to largest.
        # Use those positions to reorder the nodes.
        nodes <- nodes[order(sapply(nodes, "[[", "weight"))]

        # The first two nodes now have the smallest weights.
        #
        # Make them children of a new parent node:
        # - the first becomes the left child
        # - the second becomes the right child
        #
        # The parent's weight counts all character occurrences
        # represented by both children.
        #
        # Parent nodes have no symbol because they represent
        # groups of characters, rather than individual characters.
        parent <- list(
            weight = nodes[[1]]$weight + nodes[[2]]$weight,
            left = nodes[[1]],
            right = nodes[[2]]
        )

        # Remove the two nodes that were just combined.
        # Add their new parent to the remaining nodes.
        #
        # list(parent) wraps the parent as one list element.
        # This keeps it as a single node when c() combines the lists.
        nodes <- c(nodes[-c(1, 2)], list(parent))
    }

    # Define a recursive function to read codes from the tree.
    #
    # node: the current tree node
    # prefix: the sequence of bits used to reach that node
    #
    # Start with an empty prefix at the root.
    walk <- function(node, prefix = "") {

        # A node with a symbol is a leaf.
        # Reaching a leaf means its character's code is complete.
        #
        # Internal nodes have no symbol field, so node$symbol
        # returns NULL for those nodes.
        if (!is.null(node$symbol)) {

            # Normally, use the bits collected along the path.
            #
            # Special case: if the input contains only one distinct
            # character, the root is already a leaf.
            # Give that character the code "0".
            #
            # setNames() labels the code with its character,
            # allowing lookup such as codes["A"].
            return(setNames(
                if (prefix == "") "0" else prefix,
                node$symbol
            ))
        }

        # Visit both children:
        # - append "0" when moving left
        # - append "1" when moving right
        #
        # Each call continues until it reaches a leaf.
        # c() combines the named codes returned by both branches.
        c(
            walk(node$left, paste0(prefix, "0")),
            walk(node$right, paste0(prefix, "1"))
        )
    }

    # Only one node remains: the root of the complete tree.
    # Walk through it to create the character-to-code lookup.
    codes <- walk(nodes[[1]])

    # Return both the codebook and the encoded message.
    list(
        codes = codes,

        # Look up the code for every original character.
        # For "BANANA", this gives:
        # "10", "0", "11", "0", "11", "0"
        #
        # collapse = "" joins them into one continuous string.
        encoded = paste0(codes[chars], collapse = "")
    )
}

#' Decode a Huffman-encoded message
#'
#' Recover the original text from a string of binary digits using its Huffman
#' codebook.
#'
#' @param encoded A single, non-missing character string containing only zeros
#'   and ones. An empty string is allowed and decodes to an empty string.
#' @param codes A named character vector mapping symbols to binary codes, as
#'   returned in the \code{codes} component of \code{huffman()}. Use the same
#'   codebook that was used to encode the message.
#'
#' @details
#' Bits are read from left to right. Whenever the collected bits match a complete
#' code, the corresponding symbol is appended to the decoded text and collection
#' starts again.
#'
#' The codebook must contain unique, non-empty binary codes and must be
#' prefix-free: no code may be the beginning of another code. These codebook
#' requirements are assumed and are not validated by this function.
#'
#' The function raises an error if the encoded string contains characters other
#' than zero and one, or if bits remain unmatched at the end of the message.
#'
#' @return A single character string containing the decoded text.
#'
#' @seealso \code{\link{huffman}} for creating a codebook and encoding text.
#'
#' @examples
#' result <- huffman("BANANA")
#' decode_huffman(result$encoded, result$codes)
#'
#' # Empty input decodes to empty text.
#' decode_huffman("", result$codes)
#'
#' @export
decode_huffman <- function(encoded, codes) {

    # Reverse the codebook.
    # Before: A -> "0", B -> "10", N -> "11"
    # After: "0" -> A, "10" -> B, "11" -> N
    lookup <- setNames(names(codes), unname(codes))

    # Split the encoded string into individual bits.
    bits <- strsplit(encoded, "")[[1]]

    # Store decoded symbols here.
    decoded <- character()

    # Collect bits until they match a complete code.
    current <- ""

    for (bit in bits) {

        # Reject characters other than binary digits.
        if (!bit %in% c("0", "1")) {
            stop("The encoded string must contain only 0 and 1.")
        }

        current <- paste0(current, bit)

        # Have we collected a complete character code?
        if (current %in% names(lookup)) {

            # Find its symbol and append it to the output.
            decoded <- c(decoded, lookup[[current]])

            # Start collecting the next character's code.
            current <- ""
        }
    }

    # Leftover bits mean the final code was incomplete or invalid.
    if (current != "") {
        stop("The encoded string ends with an incomplete or invalid code.")
    }

    # Join the decoded symbols into one string.
    paste0(decoded, collapse = "")
}
