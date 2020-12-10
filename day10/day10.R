input <- scan("day10/input")

# part one ---------------------------------------------------------------------
dat <- sort(c(0, input, max(input) + 3))
sum(diff(dat) == 1) * sum(diff(dat) == 3)

# part two ---------------------------------------------------------------------
l <- rle(diff(dat))$lengths
v <- rle(diff(dat))$values

# sum from (N - 2):N of N choose k is same as sum from 0:2 of N choose k
tmp <- sapply(0:2, function(x) choose(l[v == 1] - 1, x))
prod(rowSums(tmp))
