# load input
input <- readLines("day2/input")

# split input and transpose
dat <- strsplit(input, " ")
dat <- lapply(seq_along(dat[[1]]), function(x) unlist(lapply(dat, "[[", x)))

# pull out frequency
freq <- strsplit(dat[[1]], "-")
freq <- lapply(seq_along(freq[[1]]), function(x) lapply(freq, "[[", x))
freq_min <- as.integer(freq[[1]])
freq_max <- as.integer(freq[[2]])

# pull out letter
letter <- gsub(":", "", dat[[2]])

# pull out passwords and calculate occurences
passwords <- dat[[3]]
matches <- mapply(gregexpr, letter, passwords, SIMPLIFY = FALSE)
matches_lengths <- mapply(regmatches, passwords, matches)
matches_lengths <- sapply(matches_lengths, length)

# Answer to part one
sum(matches_lengths >= freq_min & matches_lengths <= freq_max)

# Answer to part two
idx_1 <- mapply(function(x, y) x %in% y[[1]], x = freq_min, y = matches)
idx_2 <- mapply(function(x, y) x %in% y[[1]], x = freq_max, y = matches)
sum((idx_1 & !idx_2) | (!idx_1 & idx_2))

