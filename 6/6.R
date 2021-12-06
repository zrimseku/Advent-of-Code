input <- read.delim('6/input', header = F, sep = ',')

nr_days <- 80

state <- as.matrix(input[1,])
for (day in 1:nr_days) {
  cat(day)
  state <- state - 1
  zeros <- which(state == -1)
  state <- c(state, rep(8, length(zeros)))
  state[zeros] <- 6
}

cat('Part 1: ', length(state))

# part 2
fish_on_day <- rep(0, 8)
nr_days <- 256
for (i in 0:8) {
  fish_on_day[i+1] <- length(which(input == i))
}

for (day in 1:nr_days) {
  fish_on_day <- c(fish_on_day[2:7], fish_on_day[1] + fish_on_day[8], fish_on_day[9], fish_on_day[1])
}

fish_number <- sum(fish_on_day)
cat('Part 2: ', format(fish_number, digits = 15))
