input <- readLines("day16/input")

# split the data ---------------------------------------------------------------
split <- split(input, cumsum(input == ""))
rules <- split[[1]]
your_ticket <- as.integer(unlist(strsplit(split[[2]][-(1:2)], ",")))
nearby_tickets <- split[[3]][-(1:2)]

pattern <- "(\\w+\\s*\\w*): (\\d+)-(\\d+) or (\\d+)-(\\d+)"
dat <- strcapture(
  pattern, 
  rules,
  proto = data.frame(
    rule = character(),
    lower1 = integer(),
    upper1 = integer(),
    lower2 = integer(),
    upper2 = integer()
  )
)


# part one ---------------------------------------------------------------------
tickets <- lapply(strsplit(nearby_tickets, ","), as.integer)
is_valid_number <- function(x, lower1, upper1, lower2, upper2) {
  any(x >= lower1 && x <= upper1) || any(x >= lower2 && x <= upper2)
}
valid_number <- vapply(
  unlist(tickets),
  is_valid, 
  logical(1), 
  dat$lower1, dat$upper1, dat$lower2, dat$upper2
)
sum(unlist(tickets)[!valid_number])


# part two ---------------------------------------------------------------------
tickets <- lapply(strsplit(nearby_tickets, ","), as.integer)
valid_tickets <- lapply(
  tickets, 
  function(x) {
    all(vapply(x, is_valid, logical(1), dat$lower1, dat$upper1, dat$lower2, dat$upper2))
  }
)
valid_tickets <- tickets[unlist(valid_tickets)]
transposed_tickets <- 
  lapply(
    seq_along(valid_tickets[[1]]),
    function(x) sapply(valid_tickets, "[[", x)
  )

result <- integer(20)
for (i in 1:20) {
  tick <- transposed_tickets[[i]]
  for (j in 1:nrow(dat)) {
    tmp <- dat[j,]
    if (all((tick >= tmp$lower1 & tick <= tmp$upper1) | (tick >= tmp$lower2 & tick <= tmp$upper2))) {
      result[j] <- result[j] + 1
    }
  }
}

dat <- dat[order(result, decreasing = TRUE),]
result <- character(20)
for (i in 1:20) {
  tick <- transposed_tickets[[i]]
  for (j in 1:nrow(dat)) {
    tmp <- dat[j,]
    if (all((tick >= tmp$lower1 & tick <= tmp$upper1) | (tick >= tmp$lower2 & tick <= tmp$upper2))) {
      result[i] <- tmp$rule
    }
  }
}

prod(your_ticket[grepl("departure",result)]) 