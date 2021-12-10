input <- read.delim('10/input', header = F, sep = '')

alowed_pairs <- c('()', '<>', '[]', '{}')
price <- list(')'=3, ']'=57, '}'=1197, '>'=25137)

error_score <- 0
for (line in input[,1]) {
  chars <- unlist(strsplit(line, split = ''))
  stack <- c()
  for (char in chars) {
    if (char %in% c('(', '<', '[', '{')) {stack <- c(stack, char)}
    else if (paste0(tail(stack, 1), char) %in% alowed_pairs) {stack <- stack[1:(length(stack) - 1)]}
    else {
      error_score <- error_score + price[[char]]
      break
    }
  }
}

cat('Part 1: ', error_score)

# Part 2
correct <- list('('=1, '['=2, '{'=3, '<'=4)
scores <- c()
for (line in input[,1]) {
  chars <- unlist(strsplit(line, split = ''))
  stack <- c()
  line_ok <- T
  for (char in chars) {
    if (char %in% c('(', '<', '[', '{')) {stack <- c(stack, char)}
    else if (paste0(tail(stack, 1), char) %in% alowed_pairs) {stack <- stack[1:(length(stack) - 1)]}
    else { line_ok <- F }
  }
  if (line_ok) {
    sc <- 0
    for (s in rev(stack)) {
      sc <- 5 * sc + correct[[s]]
    }
    scores <- c(scores, sc)
  }
}

cat('Part 2: ', median(scores))