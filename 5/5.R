input <- read.delim('5/input', header = F, sep = ' ')

input[,c('x1', 'y1')] <- as.integer(matrix(data = unlist(strsplit(input[,1], split = ',')), ncol = 2, byrow=T))
input[,c('x2', 'y2')] <- as.integer(matrix(data = unlist(strsplit(input[,3], split = ',')), ncol = 2, byrow=T))

input <- input[, 4:7]

# part 1
diagram <- matrix(data = 0., nrow = max(input[,c('x1', 'x2')]) + 1, ncol = max(input[,c('y1', 'y2')]) + 1)

for (i in 1:nrow(input)) {
  segment <- input[i,] + 1
  if (segment$x1 == segment$x2 | segment$y1 == segment$y2) {
    diagram[segment$x1:segment$x2, segment$y1:segment$y2] <- diagram[segment$x1:segment$x2, segment$y1:segment$y2] + 1
  }
}

cat('Part 1: ', sum(as.integer(diagram >= 2)))

# part 2
diagram <- matrix(data = 0., nrow = max(input[,c('x1', 'x2')]) + 1, ncol = max(input[,c('y1', 'y2')]) + 1)


for (i in 1:nrow(input)) {
  segment <- input[i,] + 1
  if (segment$x1 == segment$x2 | segment$y1 == segment$y2) {
    diagram[segment$x1:segment$x2, segment$y1:segment$y2] <- diagram[segment$x1:segment$x2, segment$y1:segment$y2] + 1
  } else {
    I <- diag(abs(segment$x1 - segment$x2) + 1)
    diagram[segment$x1:segment$x2, segment$y1:segment$y2] <- diagram[segment$x1:segment$x2, segment$y1:segment$y2] + I
  }
}

cat('Part 2: ', sum(as.integer(diagram >= 2)))