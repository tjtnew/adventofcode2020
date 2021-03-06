dat <- read.table("day05/input", col.names = "encoded")
dat$encoded <- gsub("F|L", "0", dat$encoded)
dat$encoded <- gsub("B|R", "1", dat$encoded)
dat$seat_ids <- strtoi(dat$encoded, base = 2)

# part one ---------------------------------------------------------------------
max(dat$seat_ids)

# part two ---------------------------------------------------------------------
dat <- dat[order(dat$seat_ids), ]
dat$diff <- c(NA, diff(dat$seat_ids, 1))
dat <- na.omit(dat)
dat$seat_ids[dat$diff == 2 ] - 1
