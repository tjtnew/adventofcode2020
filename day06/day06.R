input <- readLines("day06/input")

# part one ---------------------------------------------------------------------
dat <- tapply(input, cumsum(input == ""), paste, collapse = "")
counts <- sapply(strsplit(dat, ""), function(x) length(unique(x)))
sum(counts)

# part two ---------------------------------------------------------------------
dat <- tapply(input, cumsum(input == ""), paste, collapse = " ")
tmp <- lapply(sapply(dat, strsplit, " "), function(x) x[x != ""])
tmp <- lapply(tmp, function(x) Reduce(intersect, strsplit(x, "")))
sum(sapply(tmp, length))
