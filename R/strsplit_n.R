strsplit_n <- function(x, n) {
  # Split the input string into individual characters
  chars <- strsplit(x, "")[[1]]

  # Calculate the number of substrings
  numSubstrings <- ceiling(length(chars) / n)

  # Create a matrix to store substrings
  substrings <- matrix("", nrow = numSubstrings, ncol = n)

  # Populate the matrix with substrings
  for (i in seq_along(chars)) {
    substrings[(i - 1) %/% n + 1, i %% n + 1] <- chars[i]
  }

  # Convert matrix to a list of strings
  result <- apply(substrings, 1, paste, collapse = "")

  # Remove empty strings
  result <- result[result != ""]

  return(result)
}
