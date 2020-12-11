input <- scan("day10/input")

# part one ---------------------------------------------------------------------
dat <- sort(c(0, input, max(input) + 3))
sum(diff(dat) == 1) * sum(diff(dat) == 3)

# part two ---------------------------------------------------------------------
l <- rle(diff(dat))$lengths
v <- rle(diff(dat))$values

unique(l) # 3 1 4 2 --- 4 -> 7 combinations, 3 -> 4, 2 -> 2, 1 -> 1
tmp <- l[v == 1]
res <- rep(1, length(tmp))
res[tmp == 2] <- 2
res[tmp == 3] <- 4
res[tmp == 4] <- 7
prod(res)


# method below was a fluke.  I saw a pattern for the length 4 clusters and it
# happened to work.  If we went to length 5 clusters it wouldn't.
# sum from (N - 2):N of N choose k is same as sum from 0:2 of N choose k
# tmp <- sapply(0:2, function(x) choose(l[v == 1] - 1, x))
# prod(rowSums(tmp))