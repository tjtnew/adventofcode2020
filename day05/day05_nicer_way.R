# h/t riinu pius

dat <- read.table("day05/input", col.names = "encoded")
dat$encoded <- gsub("F|L", "0", dat$encoded)
dat$encoded <- gsub("B|R", "1", dat$encoded)
dat$seat_ids <- strtoi(dat$encoded, base = 2)

# part one ---------------------------------------------------------------------
max(dat$seat_ids)

# part two ---------------------------------------------------------------------
possible_seats <- min(dat$seat_ids):max(dat$seat_ids) 
possible_seats[!possible_seats %in% dat$seat_ids]
