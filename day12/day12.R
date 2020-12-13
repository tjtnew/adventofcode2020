input <- readLines("day12/input")

dat <- strcapture(
  "(^[NSEWLRF]*)(\\d*)",
  x = input,
  proto = data.frame(action = character(), value = integer())
)
dat$complex <- NA_complex_
dat$id <- 1:nrow(dat)

idx <- dat$action %in% c("N", "S", "E", "W")
compass_lookup <- c(N = 0L + 1i, S = 0L - 1i, E = 1L + 0i, W = -1L + 0i)
dat$complex[idx] <- unname(compass_lookup[dat$action[idx]] * dat$value[idx])

dat[dat$action == "R", "value"] <- 360L - dat[dat$action == "R", "value"]
rotation_movements <- data.frame(
  value = c(90L, 180L, 270L),
  complex = c(0L +1i, -1L + 0i, 0L -1i)
)

idx <- dat$action %in% c("R", "L")
tmp <- merge(
  dat[idx, c("value", "id")],
  rotation_movements,
  sort = FALSE,
  all.x = TRUE
)
dat$complex[idx] <- tmp[order(tmp$id), "complex"]


# part one ---------------------------------------------------------------------
facing <- 1L + 0i
position <- 0L + 0i
for (i in 1:nrow(dat)) {
  action <- dat[i, "action"]
  if(action == "F") {
    position <- position + (facing * dat[i, "value"])
  } else if (action %in% c("L", "R")) {
    facing <- facing * dat[i, "complex"]
  } else {
    position <- position + dat[i, "complex"]
  }
}
abs(Re(position)) + abs(Im(position))


# part two ---------------------------------------------------------------------
waypoint <- 10L + 1i
position <- 0L + 0i
for (i in 1:nrow(dat)) {
  action <- dat[i, "action"]
  if (action == "F") {
    pos_old <- position
    position <- position + ((waypoint - pos_old) * dat[i, "value"])
    waypoint <- waypoint + ((waypoint - pos_old) * dat[i, "value"])
  } else if (action %in% c("L", "R")) {
    waypoint <- (waypoint - position) * dat[i, "complex"] + position
  } else {
    waypoint <- waypoint + dat[i, "complex"]
  }
}
abs(Re(position)) + abs(Im(position))