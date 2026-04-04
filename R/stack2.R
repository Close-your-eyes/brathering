#' Stack a Data Frame Without Factors
#'
#' A wrapper around [utils::stack()] that returns a long-format data frame
#' with customizable column names and ensures the variable column is
#' returned as character (not factor).
#'
#' @param x A data frame or list of vectors to be stacked.
#' @param names_to A character string specifying the name of the output
#'   column that will contain the original column names. Default is `"name"`.
#' @param values_to A character string specifying the name of the output
#'   column that will contain the stacked values. Default is `"value"`.
#'
#' @returns A data frame with two columns:
#' \describe{
#'   \item{<values_to>}{The stacked values from `x`.}
#'   \item{<names_to>}{The original column names as character (not factor).}
#' }
#'
#' @details
#' This function improves on [utils::stack()] by:
#' \itemize{
#'   \item Allowing custom output column names
#'   \item Returning the indicator column as character instead of factor
#' }
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:3, b = 4:6)
#'
#' stack2(df)
#'
#' stack2(df, names_to = "variable", values_to = "val")
stack2 <- function(x,
                   names_to = "name",
                   values_to = "value") {
    y <- utils::stack(x)
    names(y) <- c(values_to, names_to)
    y[[names_to]] <- as.character(y[[names_to]])
    return(y)
}


#' Extract Unique Values Per Column as a List
#'
#' Takes a data frame and returns a named list where each element contains
#' the unique values of a column. Internally uses [stack2()] and
#' [dplyr::distinct()] to ensure uniqueness while preserving structure.
#'
#' @param x A data frame or list of vectors.
#'
#' @returns A named list where:
#' \describe{
#'   \item{names}{Correspond to the original column names of `x`.}
#'   \item{values}{Each element is a data frame containing the unique values
#'   (and associated identifiers) for that column.}
#' }
#'
#' @details
#' The function works by:
#' \itemize{
#'   \item Stacking the input using [stack2()]
#'   \item Removing duplicate values across the stacked data using
#'     [dplyr::distinct()]
#'   \item Splitting the result back into a list by column name
#' }
#'
#' Note that uniqueness is determined only on the `value` column.
#'
#' @export
#'
#' @examples
list_unique <- function(x) {

    y <- dplyr::distinct(stack2(x), value, .keep_all = T)
    y <- utils::unstack(y, form = value~name)
    return(y)
}
