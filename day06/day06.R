input <- readLines("day06/input")
dat <- tapply(input, cumsum(input == ""), paste, collapse = " ")
counts <- sapply(strsplit(dat, ""), function(x) length(unique(x)) - 1)
sum(counts)

tmp <- lapply(sapply(dat, strsplit, " "), function(x) x[x != ""])
tmp <- lapply(tmp, function(x) Reduce(intersect, strsplit(x, "")))
tmp <- sum(sapply(tmp, length))
