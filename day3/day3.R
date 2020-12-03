# function to calculate x-coordinate based on equation of line
x_coord <- function(right, down) {
  constant = right - down
  function(y) {
    (right * y - constant) / down
  }
}

is_integerish <- function(x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}

# function to calculate collisions
num_trees <- function(input = "day3/input", right, down) {

  fun <- x_coord(right, down)

  # load_input
  input <- readLines(input)

  # Work out area dimensions
  input_width <- nchar(input[[1]])
  area_height <- input_height <- length(input)
  area_width <- fun(area_height)

  # convert input in to array
  repetitions <- ceiling(area_width / input_width)
  area <- do.call(rbind,strsplit(input, ""))
  area <- do.call(cbind, replicate(repetitions, area, simplify = FALSE))

  # path coordinates
  y_path <- 1:area_height
  x_path <- fun(y_path)

  # convert to an integer path
  y_path <- y_path[is_integerish(x_path)]
  x_path <- x_path[is_integerish(x_path)]

  # answer
  path <- area[cbind(y_path, x_path)]
  sum(path == "#")
}

# part one
num_trees(right = 3, down = 1)

# part 2
prod(mapply(num_trees, right = c(1, 3, 5, 7, 1), down = c(1, 1, 1, 1, 2)))







