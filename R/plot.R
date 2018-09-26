
globalVariables(c('x', 'y', 'value'))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plot a nonogram puzzle
#'
#' @param puzzle puzzle object
#' @param solution_matrix solution matrix. optional
#' @param show_clues whether or not to show the clues along the axes. default: TRUE
#' @param title plot title. default: NULL
#' @param base_size base size for ggplot theme. default 11
#' @param col_spacing fractional extra spacing between columns
#' @param row_spacing fractional extra spacing between rows
#'
#' @return ggplot2 object
#'
#' @import ggplot2
#' @importFrom dplyr "%>%"
#' @importFrom purrr map_chr
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_puzzle_plot <- function(puzzle, solution_matrix=NULL, show_clues=TRUE, title = NULL, base_size = 11, col_spacing=0, row_spacing=0) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Auto-convert to puzzle if it looks like a puzzle string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(puzzle)) {
    puzzle <- convert_puzzle_string_to_puzzle(puzzle)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If no solution matrix given, then create a blank one.
  # If solution matrix given, then check its dimensions match the puzzle
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  puzzle_size <- unname(map_int(puzzle, length))
  if (is.null(solution_matrix)) {
    solution_matrix <- matrix(0, nrow=puzzle_size[1], ncol=puzzle_size[2])
  } else {
    stopifnot(identical(dim(solution_matrix), puzzle_size))
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set labels to the the RLE of 1s for each axis if available
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ylabels <- puzzle$row_clues %>% map_chr(~paste(.x, collapse="," )) %>% rev()
  xlabels <- puzzle$col_clues %>% map_chr(~paste(.x, collapse="\n"))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get the axis breaks from the data.frame x + y spacing
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ybreaks <- seq.int(puzzle_size[1]) * (1 + row_spacing)
  xbreaks <- seq.int(puzzle_size[2]) * (1 + col_spacing)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create plot of the solution matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- create_matrix_plot(solution_matrix, title=title, base_size=base_size)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Annotate the matrix plot with clues on the axis labels
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (show_clues) {
    p <- p +
      scale_x_continuous(breaks = xbreaks, labels = xlabels) +
      scale_y_continuous(breaks = ybreaks, labels = ylabels) +
      theme(
        plot.margin = unit(c(t=0, r=0, b=10, l=0), units='pt'),
        axis.text.x = element_text(size=base_size, vjust=1, margin=margin( -9,  0, 0, 0)),
        axis.text.y = element_text(size=base_size, hjust=1, margin=margin(  0, -6, 0, 0))
      )
  }

  p
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a ggplot of a matrix
#'
#' @param mat numeric matrix with just 0/1 values
#' @param tile_size size of tiles. Range 0-1. Default: 0.96
#' @param title plot title. default: NULL
#' @param base_size base size for ggplot theme. default 11
#' @param col_spacing fractional extra spacing between columns
#' @param row_spacing fractional extra spacing between rows
#'
#' @return ggplot2 object
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_matrix_plot <- function(mat, title = NULL, tile_size = 1, base_size = 11, col_spacing = 0, row_spacing = 0) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pre-calc matrix size for reuse
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rows <- nrow(mat)
  cols <- ncol(mat)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert matrix to data.frame for ggplot()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  df <- data.frame(
    value = as.factor(as.vector(mat)),
    y     = rep(rows + 1 - seq.int(rows)) * (1 + row_spacing),
    x     = rep(seq.int(cols), each=rows) * (1 + col_spacing)
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Basic matrix plot
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ggplot(df) +
    geom_tile(aes(x, y, fill=value, colour=value, size=value), width=tile_size, height=tile_size) +
    coord_equal() +
    theme_void(base_size = base_size) +
    scale_fill_manual (values = c('0' = 'grey97' , '1' = 'darkblue')) +
    scale_color_manual(values = c('0' = 'grey50' , '1' = 'grey80'  )) +
    scale_size_manual (values = c('0' = 0.25     , '1' = 0.25      )) +
    theme(
      legend.position = 'none',
      plot.title = element_text(hjust = 0.5, size = 15, face = 'bold')
    ) +
    ggtitle(title)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create plot of all possibilities for a given clue
#'
#' @param clue nongram clue
#' @param total_length the total length the clue spans
#' @param row_spacing fractional extra spacing between rows
#' @param ... arguments passed to \code{create_matrix_plot}
#'
#' @return ggplot2 object
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_clue_plot <- function(clue, total_length, row_spacing=0.3, ...) {
  pattern_set <- create_pattern_set(clue, total_length)

  create_matrix_plot(pattern_set, row_spacing=row_spacing, ...)
}



