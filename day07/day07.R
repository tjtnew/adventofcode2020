input <- readLines("day07/input")
dat <- strsplit(input, " bags contain ")
bag_contents <- lapply(seq_along(dat[[1]]), function(x) sapply(dat, `[[`, x))

split_contents <- function(x) {
  contents <- strsplit(x, ", ")
  out <- lapply(
    contents,
    function(x) {
      strcapture(
        "(\\d+) (.+?) bag",
        x,
        proto = data.frame(number = integer(0), contents = character(0))
      )
    }
  )
  do.call(rbind, out)
}

contents <- lapply(bag_contents[[2]], split_contents)
bags <- rep(bag_contents[[1]], sapply(contents, nrow))
res <- cbind(bags = bags, do.call(rbind, contents))
res <- res[, c(1, 3, 2)]


# part one ----------------------------------------------------------------
total <- character(0)
tmp <- 1
bag_types <- "shiny gold"
while(tmp != 0) {
  containing_bags <- na.omit(res[res$contents %in% bag_types, "bags"])
  bag_types <- unique(containing_bags[!containing_bags %in% total])
  total <- append(total, bag_types)
  tmp <- length(bag_types)
}
length(total)


# part two ----------------------------------------------------------------
total <- character(0)
tmp <- 1
contained_bags <- data.frame(bags = "shiny gold")
while(tmp != 0) {
  bags <- merge(x = contained_bags, y = res, by = "bags", all.x = TRUE)
  contained_type <- na.omit(bags$contents)
  contained_number <- na.omit(bags$number)
  contained_bags <- data.frame(bags = rep(contained_type, contained_number))
  total <- append(total, contained_bags$bags)
  tmp <- length(contained_type)
}
length(total)

