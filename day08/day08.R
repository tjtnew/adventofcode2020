input <- read.table("day08/input", sep = " ", col.names = c("op", "arg"))

dat <- input
dat$visited <- visited <- accumulator <-  0
instruction <- 1

console <- function(operation, argument) {
  dat[instruction, "visited"] <<- 1
  switch(
    operation,
    acc = {accumulator <<- accumulator + argument; instruction <<- instruction + 1 },
    nop = {instruction <<- instruction + 1},
    jmp = {instruction <<- instruction + argument}
  )
}

# part one ---------------------------------------------------------------------
while (!visited) {
  console(dat[instruction, "op"], dat[instruction, "arg"])
  visited <- dat[instruction, "visited"]
}
accumulator

# part two ---------------------------------------------------------------------
operations <- rbind(
  data.frame(op ="jmp", index = which(dat$op == "jmp")), 
  data.frame(op ="nop", index = which(dat$op == "nop"))
)

for (i in 1:nrow(operations)) {
  dat <- input
  dat$visited <- visited <- accumulator <- finished <-  0
  instruction <- 1

  if(operations[i, "op"] == "jmp") {
    dat[operations[i, "index"], "op"] <- "nop"
  } else if (operations[i, "op"] == "nop") {
    dat[operations[i, "index"], "op"] <- "jmp"
  }
  
  while (!visited && !finished) {
    console(dat[instruction, "op"], dat[instruction, "arg"])
    if (instruction > nrow(dat)) {
      finished <- 1
    } else {
      visited <- dat[instruction, "visited"]
    }
    
  }
  if (!visited) {
    break()
  }
}
accumulator

