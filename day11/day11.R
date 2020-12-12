input <- strsplit(readLines("day11/input"), split = "")
dat <- do.call(rbind, input)
extra_row <- rep("L", ncol(dat))
dat <- do.call(rbind, list(extra_row, dat, extra_row))
extra_col <- rep("L", nrow(dat))
dat <- do.call(cbind, list(extra_col, dat, extra_col))

# part one ---------------------------------------------------------------------
state_change <- function(state) {
  new_state <- state
  for (i in 2:(nrow(dat)-1)) {
    for (j in  2:(ncol(dat) - 1)) {
      if (state[i, j] == "L" && all(state[(i - 1):(i + 1), (j-1):(j + 1)] %in% c("L", "."))) {
        new_state[i, j] <- "#"
      } else if (state[i, j] == "#" && sum(state[(i - 1):(i + 1), (j-1):(j + 1)] == "#") >= 5) {
        new_state[i, j] <- "L"
      }
    }
  }
  new_state
}

new <- dat
old <- 9
while(!identical(old, new[2:(nrow(dat)-1), 2:(ncol(dat) - 1)])) {
  old <- new[2:(nrow(dat)-1), 2:(ncol(dat) - 1)]
  new <- state_change(new)
}
sum(new == "#")

# part two ---------------------------------------------------------------------
ul <- function(i, j) list(i - 1, j - 1)
u <- function(i, j) list(i - 1, j)
ur <- function(i, j) list(i - 1, j + 1)
l <- function(i, j) list(i, j - 1)
r <- function(i, j) list(i, j + 1)
dl <- function(i, j) list(i + 1, j - 1)
d <- function(i, j) list(i + 1, j)
dr <- function(i, j) list(i + 1, j + 1)
all_directions <- list(ul, u, ur, l, r, dl, d, dr)

next_seat <- function(i, j, direction, state) {
  idx <- direction(i,j)
  i = idx[[1]]
  j = idx[[2]]
  if (state[i, j] == ".") {
    next_seat(i, j, direction, state)
  } else if (state[i, j] == "L") {
    return(1)
  } else {
    return(-1)
  }
}

state_change <- function(state) {
  new_state <- state
  for (i in 2:(nrow(dat)-1)) {
    for (j in  2:(ncol(dat) - 1)) {
      if (state[i, j] == "L") {
        total <- 0
        for (d in all_directions) {
          total <- total + next_seat(i, j, d, state)
        }
        if (total == 8) {
          new_state[i, j] <- "#"  
        }
      } else if (state[i, j] == "#") {
        total <- 0
        for (d in all_directions) {
          total <- total + next_seat(i, j, d, state)
        }
        if (total < 0) {
          new_state[i, j] <- "L"  
        }
      }
    }
  }
  new_state
}

new <- dat
old <- 9
while(!identical(old, new[2:(nrow(dat)-1), 2:(ncol(dat) - 1)])) {
  old <- new[2:(nrow(dat)-1), 2:(ncol(dat) - 1)]
  new <- state_change(new)
}
sum(new == "#")

