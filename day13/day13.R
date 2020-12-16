input <- readLines("day13/input")
estimate <- as.integer(input[1])
ids <- unlist(strsplit(input[2], ","))
bus_ids <- as.integer(ids[ids != "x"])

# part one ---------------------------------------------------------------------
next_bus_times <- (estimate %/% bus_ids + 1) * bus_ids
earliest_bus <- which.min(next_bus_times)
earliest_bus_time <- next_bus_times[earliest_bus]
earliest_bus_id <- bus_ids[earliest_bus]
(earliest_bus_time - estimate) * earliest_bus_id

# part two ----- https://en.wikipedia.org/wiki/Chinese_remainder_theorem) ------
modulo <- bus_ids
minutes <- (which(ids != "x") - 1) %% modulo
modulo <- modulo[order(bus_ids, decreasing = TRUE)]
minutes <- minutes[order(bus_ids, decreasing = TRUE)]
xi <- (modulo - minutes) %% modulo

tt <- 0
product <- 1
for (i in seq_along(modulo)) {
  res <- tt %% modulo[i]
  while(!isTRUE(all.equal(res, xi[i]))) {
    tt <- tt + product
    res <- tt %% modulo[i]
  }
  product <- product * modulo[i]
}
tt
