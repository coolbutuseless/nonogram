

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Given pattern sets for one axis, only keep those that match an
#' orthogonal pattern set at the given index
#'
#' @param axis_pattern_sets pattern sets for an axis
#' @param orthog_pattern_set pattern sets for an axis so far
#' @param idx which element to look at
#'
#' @return filtered version of axis_pattern_sets
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
filter_pattern_sets <- function(axis_pattern_sets, orthog_pattern_set, idx) {
  for (orthog_idx in seq_along(orthog_pattern_set)) {
    keep_idx <- axis_pattern_sets[[orthog_idx]][,idx] == orthog_pattern_set[orthog_idx]
    axis_pattern_sets[[orthog_idx]] <- axis_pattern_sets[[orthog_idx]][keep_idx,,drop=FALSE]
  }

  axis_pattern_sets
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Filter pattern sets for one axis with the pattern sets for the other axis
#'
#' @param axis_pattern_sets first set
#' @param orthog_pattern_sets second set
#'
#' @return axis_pattern_sets filtered by the constraints of the orthogonal pattern sets
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
filter_with_orthogonal_pattern_sets <- function(axis_pattern_sets, orthog_pattern_sets) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Given the axis pattern sets, if there are values which are the
  # same no matter which sequnce, use these to filter the orthogonal pattern sets
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (orthog_idx in seq_along(orthog_pattern_sets)) {
    must_be_ones_idxs <- which(colSums(orthog_pattern_sets[[orthog_idx]]) == nrow(orthog_pattern_sets[[orthog_idx]]))
    for (one_idx in must_be_ones_idxs) {
      idx_to_keep <- axis_pattern_sets[[one_idx]][,orthog_idx] == 1L
      axis_pattern_sets[[one_idx]] <- axis_pattern_sets[[one_idx]][idx_to_keep, , drop=FALSE]
    }


    must_be_zeros_idxs <- which(colSums(orthog_pattern_sets[[orthog_idx]]) == 0)
    for (zero_idx in must_be_zeros_idxs) {
      idx_to_keep <- axis_pattern_sets[[zero_idx]][,orthog_idx] == 0L
      axis_pattern_sets[[zero_idx]] <- axis_pattern_sets[[zero_idx]][idx_to_keep, , drop=FALSE]
    }
  }

  axis_pattern_sets
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Solve a puzzle by searching recursive with back-tracking and pruning.
#'
#' @param row_solution_patterns solution so far of row patterns
#' @param col_pattern_sets column pattern sets
#' @param row_pattern_sets row pattern sets
#'
#' @return solution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
solve_puzzle_core <- function(row_solution_patterns, col_pattern_sets, row_pattern_sets) {

  col_pattern_sets_lengths  <- map_int(col_pattern_sets, nrow)

  height <- length(row_pattern_sets)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(row_solution_patterns)==height && all(col_pattern_sets_lengths == 1L)) {
    solution <- do.call(rbind, row_solution_patterns)
    return(solution)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # no solution possible on this route
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (any(col_pattern_sets_lengths == 0L)) {
    return()
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Given the column pattern sets, if there are values which are the
  # same no matter which pattern, use these to filter the row pattern sets
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  row_pattern_sets <- filter_with_orthogonal_pattern_sets(row_pattern_sets, col_pattern_sets)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # no solution possible on this route
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  row_pattern_sets_lengths  <- map_int(row_pattern_sets, nrow)
  if (any(row_pattern_sets_lengths == 0L)) {
    return()
  }


  col_pattern_sets <- filter_with_orthogonal_pattern_sets(col_pattern_sets, row_pattern_sets)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # no solution possible on this route
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  col_pattern_sets_lengths  <- map_int(col_pattern_sets, nrow)
  if (any(col_pattern_sets_lengths == 0L)) {
    return()
  }




  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  row_idx                <- length(row_solution_patterns) + 1L
  possible_row_solution_patterns <- row_pattern_sets[[row_idx]]
  next_row_solution_patterns     <- row_solution_patterns

  # For debugging, print the current recursive depth
  # cat(row_idx); flush.console()


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (row_pattern_index in seq(nrow(possible_row_solution_patterns))) {
    row_pattern                  <- possible_row_solution_patterns[row_pattern_index,]
    next_col_pattern_sets      <- filter_pattern_sets(col_pattern_sets, row_pattern, row_idx)
    next_row_solution_patterns[[row_idx]] <- row_pattern
    solution                      <- solve_puzzle_core(next_row_solution_patterns, next_col_pattern_sets, row_pattern_sets)
    if (!is.null(solution)) { break }
  }


  solution
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Solve a puzzle by searching recursive with back-tracking and pruning.
#'
#' @param puzzle nonogram puzzle or puzzle string
#' @param verbose print out timing information. default: FALSE
#'
#' @return solution as a matrix
#'
#' @importFrom purrr map_int
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
solve_puzzle <- function(puzzle, verbose=FALSE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Argument should really be a puzzle object, but just in case the
  # user passed a string, treat it as a puzzle string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(puzzle)) {
    puzzle <- convert_puzzle_string_to_puzzle(puzzle)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The width and height of the puzzle is found by
  # counting the clues in each direction
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  width  <- length(puzzle$col_clues)
  height <- length(puzzle$row_clues)



  if (verbose) {
    message("------------------------------------------------------------\nCreating all possible pattern sets. This can take up to a minute (and lots of ram) for some puzzles ...")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each clue in the puzzle create a pattern set which lists all the
  # possible patterns which satisfy the clue
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  timing <- system.time({
    row_pattern_sets <- puzzle$row_clues %>% map(create_pattern_set, total_length=width )
    col_pattern_sets <- puzzle$col_clues %>% map(create_pattern_set, total_length=height)
  })

  if (verbose) {
    message("Creation of all possible pattern sets from the given clues: ", round(timing[['elapsed']], 2), " seconds")
  }


  timing <- system.time({
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # If some rows have a clue set which means a particular location is always
    # BLACK or always WHITE, then use that information to filter pattern sets
    # in the orthogonal direction.
    # Keep doing this kind of filtering until there are no more occurrences.
    # Many times this sort of filtering is all that is required to solve the puzzle
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    prev_ncombos <- c(prod(map_int(row_pattern_sets, nrow)), prod(map_int(col_pattern_sets, nrow)))
    if (verbose) { message("Starting (Row) x (Column) combinations: ", formatC(prev_ncombos[1], 3), " x ", formatC(prev_ncombos[2], 3)) }
    while (TRUE) {
      row_pattern_sets <- filter_with_orthogonal_pattern_sets(row_pattern_sets, col_pattern_sets)
      col_pattern_sets <- filter_with_orthogonal_pattern_sets(col_pattern_sets, row_pattern_sets)
      ncombos <- c(prod(map_int(row_pattern_sets, nrow)), prod(map_int(col_pattern_sets, nrow)))
      if (identical(ncombos, prev_ncombos)) {
        break
      }
      prev_ncombos <- ncombos
    }

    if (verbose) { message("Filtered (Row) x (Column) combinations: ", formatC(ncombos[1], 3), " x ", formatC(ncombos[2], 3)) }

    if (ncombos[1] == 1) {
      # we've filtered down all the possible row pattern sets so that only
      # one possibility remains at each row. i.e. solved it already!
      solution_matrix <- do.call(rbind, row_pattern_sets)
    } else {
      # Still mutliple patterns per row, going to need to actually recurse
      # and back-track to find an actual solution
      if (verbose) { message("Starting recursive solution with back-tracking...") }
      solution_matrix <- solve_puzzle_core(list(), col_pattern_sets, row_pattern_sets)
    }
  })

  if (verbose) {
    message("Total solution time: ", round(timing[['elapsed']], 2), " seconds\n------------------------------------------------------------\n")
  }

  solution_matrix
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
    library(nonogram)
    library(ggplot2)
  })

  # devtools::use_data(puzzle_string_examples)

  # 320  561  602 1071 1813 1814 2751 4148
  puzzle          <- puzzle_string_library[1071]  # 70=tough   # 1071 = 84s to find all possible patterns
  puzzle          <- convert_puzzle_string_to_puzzle(puzzle)
  puzzle          <- puzzle_string_examples[['gchq']]
  solution_matrix <- solve_puzzle(puzzle, verbose=TRUE)

  create_puzzle_plot(puzzle, solution_matrix, show_clues = FALSE)


  s <- function(i) {
    puzzle <- puzzle_string_library[i]  # 70=tough   # 1071 = 84s to find all possible patterns
    puzzle <- convert_puzzle_string_to_puzzle(puzzle)
    solution_matrix <- solve_puzzle(puzzle, verbose=TRUE)

    create_puzzle_plot(puzzle, solution_matrix, show_clues = FALSE)
  }


}





if (FALSE) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
    library(nonogram)
  })

  # puzzle <- puzzle_string_library[3711]  # 1983
  puzzle <- puzzle_string_library[1071]  # 1983

  if (is.character(puzzle)) {
    puzzle <- convert_puzzle_string_to_puzzle(puzzle)
  }

  width  <- length(puzzle$col_clues)
  height <- length(puzzle$row_clues)

  system.time({
    row_pattern_sets <- puzzle$row_clues %>% map(create_pattern_set, total_length=width )
  })
  system.time({
    col_pattern_sets <- puzzle$col_clues %>% map(create_pattern_set, total_length=height)
  })

  create_pattern_set(puzzle$row_clues[[4]], width)
}












