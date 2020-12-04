input <- readLines("day04/input")
groups <- cumsum(input == "")
dat <- tapply(input, groups, paste, collapse = " ")

# part one ---------------------------------------------------------------------
required <- "(?=.*byr)(?=.*iyr)(?=.*eyr)(?=.*hgt)(?=.*hcl)(?=.*ecl)(?=.*pid)"
idx <- grepl(required, dat, perl=TRUE)
sum(idx)

# part two ---------------------------------------------------------------------
valid <- dat[idx]
fields <- c("byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:")
captures <- sprintf(".*%s(\\S*)", fields)
tmp <- sapply(captures, strcapture, x = valid, proto = data.frame(character(0)))

byr <- function(x) {
    as.integer(x) >= 1920 & as.integer(x) <= 2002
}

iyr <- function(x){
    as.integer(x) >= 2010 & as.integer(x) <= 2020
}

eyr <- function(x) {
    as.integer(x) >= 2020 & as.integer(x) <= 2030
}

hgt <- function(x) {
    vapply(
        x,
        function(y) {
            if(grepl("cm", y)) {
                y <- sub("cm", "", y)
                as.integer(y) >= 150 & as.integer(y) <= 193
            } else if (grepl("in", y)) {
                y <- sub("in", "", y)
                as.integer(y) >= 59 & as.integer(y) <= 76
            } else {
                FALSE
            }
        },
        logical(1)
    )
}

hcl <- function(x) {
    nchar(x) == 7 & grepl("#[a-f0-9]{6}$", x)
}

ecl <- function(x) {
    x %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
}

pid <- function(x) {
    (nchar(x) == 9) & (grepl("[0-9]{9}$", x))
}

conditions <- list(byr, iyr, eyr, hgt, hcl, ecl, pid)
res <- lapply(seq_along(conditions), function(i) conditions[[i]](tmp[[i]]))
sum(Reduce(`&`, res))
