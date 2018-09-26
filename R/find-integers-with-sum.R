

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pre-generate some tables for "find_integers_with_sum"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  max_cache_target_sum    <- 20
  max_cache_target_length <- 7

  sum_cache <- vector('list', max_cache_target_sum)

  for (target_length in seq(max_cache_target_length)) {
    for (target_sum in target_length:max_cache_target_sum) {

      res <- find_integers_with_sum(target_sum, target_length)
      res <- do.call(rbind, res)
      sum_cache[[target_sum]][[target_length]] <- list(res)
    }
  }

  pryr::object_size(sum_cache)
  devtools::use_data(sum_cache, max_cache_target_sum, max_cache_target_length, internal=TRUE, overwrite = TRUE, compress='xz')
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Find all permutations of all vectors of positive integers of a given length which sum to a given value
#'
#' e.g. \code{find_integers_with_sum(4, 2)} gives `c(1, 3)`, `c(2, 2)`, `c(3, 1)`
#'
#' @param target_length target length
#' @param target_sum target sum
#' @param current_vec current working set for the back-tracking solution process
#'
#' @return list of integer vectors
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
find_integers_with_sum <- function(target_sum, target_length, current_vec=c()) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If we have a target length of 1 and the sum is above zero, then
  # this is a valid solution! If the sum is <1, then this is not a solution.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (target_length == 1L) {
    if (target_sum > 0L) {
      return(list(c(current_vec, target_sum)))
    } else {
      return()
    }
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Special condition. if target_length == target_sum, all remaining
  # integers must be 1L
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (target_length == target_sum) {
    return(list(c(current_vec, rep(1L, target_length))))
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If we're within the cache zone, look stuff up in the cache instead
  # or recursing
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (target_sum <= max_cache_target_sum && target_length <= max_cache_target_length) {
    cbind_args <- c(as.list(current_vec), sum_cache[[target_sum]][[target_length]])
    return(list(do.call(cbind, cbind_args)))
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Limit the choices in the next iteration to just ones that are possible
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  choices <- seq_len(target_sum - target_length + 1L)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each choice, add it to the current_vec and see if that is a pathway
  # to a solution
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  solutions <- list()
  for (new_val in choices) {
    this_solution <- find_integers_with_sum(target_sum - new_val, target_length - 1L, c(current_vec, new_val))
    solutions <- c(solutions, this_solution)
  }

  solutions
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Find all permutations of all vectors of positive numbers of a given length
#' which sum to a given value, with zeros allowed as first and last elements
#'
#' This is used to find all possible run-length encodings of the patterns
#' of zeros for a nonogram clue.
#'
#' @param target_sum target sum
#' @param target_length target length
#'
#' @return matrix
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
find_integers_with_sum_for_nonograms <- function(target_sum, target_length) {

  target_sum    <- as.integer(target_sum)
  target_length <- as.integer(target_length)

  rbind_arg_list <- list()

  if (target_length <= target_sum) {
    x1              <- find_integers_with_sum(target_sum=target_sum, target_length=target_length)
    x1              <- do.call(rbind, x1)
    rbind_arg_list  <- c(rbind_arg_list, list(x1))
  }

  if (target_length - 1L <= target_sum) {
    x2              <- find_integers_with_sum(target_sum=target_sum, target_length=target_length - 1L)
    x2              <- do.call(rbind, x2)
    rbind_arg_list  <- c(rbind_arg_list, list(cbind(0L, x2), cbind(x2, 0L)))
  }

  if (target_length - 2L > 0L) {
    x3              <- find_integers_with_sum(target_sum=target_sum, target_length=target_length - 2L)
    x3              <- do.call(rbind, x3)
    rbind_arg_list  <- c(rbind_arg_list, list(cbind(0L, x3, 0L)))
  }

  do.call(rbind, rbind_arg_list)
}



find_integers_with_sum_for_nonograms_cached <- memoise::memoise(find_integers_with_sum_for_nonograms)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  do.call(rbind, find_integers_with_sum(5, 5))

  find_integers_with_sum_for_nonograms(5, 3)
}












