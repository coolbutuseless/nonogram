


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert between nonogram puzzle strings and puzzles
#'
#' A 'puzzle string' is a compact text representation of a nonogram puzzle, whereas a
#' 'puzzle' is an R list object representing the puzzle.
#'
#' A \code{puzzle} is a list containing:
#' \itemize{
#'   \item{a list of the row clues}{}
#'   \item{a list of the column clues}{}
#' }
#'
#' A \code{clue} is a list of integers which is the run-length encoding of just
#' the filled spaces in the final puzzle.
#'
#' e.g. \code{
#' list(row_clues = list(2L, 2:1, c(1L, 1L), 3L, c(1L, 1L), c(1L, 1L), 2L, c(1L, 1L), 1:2, 2L),
#'      col_clues = list(2:1, c(2L, 1L, 3L), 7L, c(1L, 3L), 2:1))
#' }
#'
#' A \code{puzzle string} is a just a compact string representation of the puzzle.
#' It consists of:
#' \itemize{
#'   \item{the numbers for each clue separated by a comma}{}
#'   \item{each clue separated by a colon}{}
#'   \item{the row and column clues (in that order) separated by a dash}{}
#' }
#'
#' e.g. \code{"2:2,1:1,1:3:1,1:1,1:2:1,1:1,2:2-2,1:2,1,3:7:1,3:2,1"}
#'
#' @param puzzle_string compact string representation of a puzzle
#' @param puzzle R list representation of a puzzle
#'
#' @importFrom methods el
#' @importFrom dplyr "%>%"
#' @import purrr
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_puzzle_string_to_puzzle <- function(puzzle_string) {
  el(strsplit(puzzle_string, '-')) %>%
    map(~el(strsplit(.x, ':'))) %>%
    modify_depth(1, ~strsplit(.x, ',')) %>%
    modify_depth(2, as.integer) %>%
    set_names(c("row_clues", "col_clues"))
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname convert_puzzle_string_to_puzzle
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_puzzle_to_puzzle_string <- function(puzzle) {
  puzzle %>%
    modify_depth(2, ~paste(.x, collapse=',')) %>%
    modify_depth(1, flatten_chr) %>%
    map_chr(paste, collapse=':') %>%
    paste(collapse='-')
}





if (FALSE) {

  game_strings <- readRDS("working/leech/nonogram_puzzle_strings.rds")
  pryr::object_size(game_strings)

  g <- puzzle_string_library[1]

  res <- convert_puzzle_string_to_puzzle(g)
  dput(res)


}