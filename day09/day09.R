input <- scan("day09/input")

for (i in 26:length(input)) {
  numbers <- input[(i - 25):(i - 1)]
  combinations <- combn(numbers, 2)
  sums <- colSums(combinations)
  if (!input[i] %in% sums) {
    ans_1 <- input[i]
    break
  }
}
# part one ---------------------------------------------------------------------
ans_1

# part two ---------------------------------------------------------------------
dat <- input[input < ans_1]
amount <- 2
finished <- FALSE
while (!finished) {
  for (i in 1:(length(dat) - (amount - 1))) {
    res <- sum(dat[i:(i + amount - 1)])
    if(isTRUE(all.equal(ans_1, res))) {
      ans_2 <- min(dat[i:(i + amount - 1)]) + max(dat[i:(i + amount - 1)])
      finished <- TRUE
      break()
    }
  }
  amount <- amount + 1
}

