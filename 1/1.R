input <- read.delim('1/input.txt', header = F)

# part 1
prev <- input[1,]
depths <- input[2:nrow(input), 'V1']
n_larger <- 0
for (depth in depths) {
  if (depth > prev) {n_larger <- n_larger + 1}
  prev <- depth
}

cat('Part 1: ', n_larger, ' larger than previous measurement.')


# part 2

prev <- input[1, 'V1']  # we don't need to look at the sum, only the element three steps back
sum_larger <- 0

for (i in 4:nrow(input)) {
  depth <- input[i, 'V1']
  if (depth > prev) { sum_larger <- sum_larger + 1 }
  prev <- input[i - 2, 'V1']
}

cat('Part 2: ', sum_larger, ' sums that are larger than the previous sum.')
