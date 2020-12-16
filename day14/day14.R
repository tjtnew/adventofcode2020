input <- readLines("day14/input")
split <- split(input, cumsum(grepl("mask", input)))

# create dataframe of masks, memory location and value
dat <- lapply(
  split, 
  function(x) {
    strcapture(
      "mem\\[(\\d+)\\] = (\\d+)",
      x[-1], 
      proto = data.frame(mem = integer(), value = integer())
    )
  }
)
masks <- vapply(split, function(x) gsub("mask = ", "", x[1]), character(1))
lengths <- vapply(split, length, integer(1)) - 1
dat <- cbind(data.frame(masks = rep(masks, lengths)), do.call(rbind, dat))

# function to convert integers to bits
int_to_bits <- function(x) {
  n <- rev(intToBits(x))
  n <- vapply(n, function(x) sub(".", "", x), character(1))
  n <- paste(n, collapse = "")
  paste0("0000", n) # values are all 32 bit integers so we pad
}

# function to apply the masking
mask_value <- function(value, mask) {
  value[mask != "X"] <- mask[mask != "X"]
  paste(value, collapse = "")
}

# function to convert binary to integer/double
decimal_value <- function(x) {
  tmp = as.integer(strsplit(x, "")[[1]])
  Reduce(function(x, y) x * 2 + y, tmp)
}

# Add the additional columns
dat$bit_values <- vapply(dat$value, int_to_bits, character(1))
v <- strsplit(dat$bit_values, "")
m <- strsplit(dat$masks, "")
dat$masked_value <- mapply(mask_value, v, m)
dat$decimal_value <- vapply(dat$masked_value, decimal_value, numeric(1))

# answer to part one
sum(dat$decimal_value[!duplicated(dat$mem, fromLast = TRUE)])


# function to apply the second version of masking
mask_value_2 <- function(mask, mem) {
  mem[mask != "0"] <- mask[mask != "0"]
  paste(mem, collapse = "")
}

# Add the additional columns
dat$mem_bits <- vapply(dat$mem, int_to_bits, character(1))
mem <- strsplit(dat$mem_bits, "")
mask <- strsplit(dat$masks, "")
dat$masked_mem <- mapply(mask_value_2, mask, mem)

# pretend list is a hash table
result <- list()
# looping
for (i in 1:nrow(dat)) {
  masked_mem <- dat$masked_mem[i]
  numx <- nchar(masked_mem)-nchar(gsub("X", "", masked_mem, fixed=TRUE))
  masked_mem <- strsplit(masked_mem, "")[[1]]
  combinations <- expand.grid(rep(list(0:1), numx))
  for (j in 1:nrow(combinations)) {
    tmp <- masked_mem
    combi <- unlist(combinations[j, ])
    tmp[tmp == "X"] <- as.character(combi)
    tmp <- paste(tmp, collapse = "")
    result[tmp] <- dat$value[i]
  }
}

# answer to part 2
sum(unlist(result)) 
