# part one ---------------------------------------------------------------------
input <- readLines("day05/input")
dat <- gsub("F|L", "0", input)
dat <- gsub("B|R", "1", dat)
dat <- strcapture(
    "([01]{7})([01]{3})", dat,data.frame(row = character(0), col = character(0))
)
seats <- lapply(dat, strtoi, base = 2)
ids <- seats[["row"]] * 8 + seats[["col"]] 
max(ids)

# part two ---------------------------------------------------------------------
tmp <- data.frame(do.call(cbind,seats), ids = ids)
tmp <- tmp[order(tmp$ids),]
tmp$diff <- c(NA, diff(tmp$ids, 1))
tmp <- na.omit(tmp)
tmp[tmp$diff == 2 & tmp$row != (max(tmp$row) | min(tmp$row)), ]$ids - 1
