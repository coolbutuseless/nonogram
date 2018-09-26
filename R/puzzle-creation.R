


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Given a vector of 0/1 values, create the corresponding nonogram clue
#'
#' @param pattern vector of 0/1 values
#'
#' @return corresponding nonogram clue
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_clue_from_pattern <- function(pattern) {
  r <- rle(pattern)
  res <- r$lengths[r$values != 0]
  if (length(res) == 0) {
    0L
  } else {
    res
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Given a matrix, create a nonogram puzzle
#'
#' @param mat integer matrix with just 0/1 values
#'
#' @return puzzle in R object format i.e. a list
#'         of \code{row_clues} and \code{col_clues}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_puzzle_from_matrix <- function(mat) {
  list(
    row_clues = apply(mat, 1, create_clue_from_pattern),
    col_clues = apply(mat, 2, create_clue_from_pattern)
  )
}