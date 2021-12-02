input <- read.delim('2/input.txt', header = F, sep = ' ')

# part 1
position <- c(0, 0)
directions <- list('up'=c(0, -1), 'down'=c(0, 1), 'forward'=c(1, 0))

for (i in 1:nrow(input)) {
  direction <- input[i, 'V1']
  step <- input[i, 'V2']
  position <- position + unlist(directions[direction]) * step
}

cat('Part 1: ', prod(position))

# part 2
position <- c(0, 0)
aim <- 0

for (i in 1:nrow(input)) {
  direction <- input[i, 'V1']
  step <- input[i, 'V2']
  if (direction == 'up') {aim <- aim - step}
  if (direction == 'down') {aim <- aim + step}
  if (direction == 'forward') {position <- position + c(step, step * aim)}
}

cat('Part 1: ', prod(position))
