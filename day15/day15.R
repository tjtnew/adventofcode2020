input <- scan("day15/input", what = integer(), sep = ",")

day15 <- function(input, iteration) {
  n <- length(input)
  last_said <- integer(iteration)
  last_said[input[1:(n - 1)] + 1] <- 1:(n - 1)
  current <- input[n]
  for (i in n:(iteration - 1)) {
    if(last_said[current + 1] != 0) {
      tmp <- i - last_said[current + 1]
      last_said[current + 1] <- i
      current <- tmp
    } else {
      last_said[current + 1] <- i
      current <- 0
    }
  }
  current
}


# part one ---------------------------------------------------------------------
day15(input, 2020)

# part one ---------------------------------------------------------------------
day15(input, 30000000)
