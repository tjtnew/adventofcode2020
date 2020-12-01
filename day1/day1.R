# Load data --------------------------------------------------------------------
input <- as.integer(readLines("day1/input"))

# Part 1 -----------------------------------------------------------------------
dat <- expand.grid(input, input)
tmp <- dat[rowSums(dat) == 2020L,]
apply(tmp, 1, prod)[1]

# Part 2 -----------------------------------------------------------------------
dat <- expand.grid(input, input, input)
tmp <- dat[rowSums(dat) == 2020L,]
apply(tmp, 1, prod)[1]
