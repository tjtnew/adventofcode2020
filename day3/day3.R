# calculate x-coordinate based on equation of line
x_coord <- function(right, down) {
  constant = right - down
  function(y) {
    (right * y - constant) / down
  }
}

# is the value approximately integer
is_integerish <- function(x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}

# function to calculate collisions
num_trees <- function(input = "day3/input", right, down) {

  fun <- x_coord(right, down)

  # load_input
  input <- readLines(input)

  # Work out area dimensions
  width <- nchar(input[[1]])
  height <- length(input)

  # convert input in to array
  area <- do.call(rbind, strsplit(input, ""))

  # path coordinates
  y_path <- 1:height
  x_path <- ((fun(y_path) - 1) %% width) + 1

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







