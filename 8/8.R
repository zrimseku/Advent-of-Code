input <- as.matrix(read.delim('8/input', header = F, sep = ' '))

# Part 1
count_unique <- 0
for (nr in input[,12:15]) {if (nchar(nr) %in% c(2, 3, 4, 7)) {count_unique <- count_unique + 1}}

cat('Part 1: ', count_unique)


deduct_mapping <- function (digits) {
  one <- unlist(strsplit(digits[which(nchar(digits) == 2)], ''))
  seven <- unlist(strsplit(digits[which(nchar(digits) == 3)], ''))
  four <- unlist(strsplit(digits[which(nchar(digits) == 4)], ''))
  l5 <- strsplit(digits[which(nchar(digits) == 5)], '')
  l6 <- strsplit(digits[which(nchar(digits) == 6)], '')

  # deduction
  top <- setdiff(seven, one)
  bottom_r <- Reduce(intersect, l6, one)
  top_r <- setdiff(one, bottom_r)

  middle <- Reduce(intersect, l5, four)
  top_l <- setdiff(four, c(middle, top_r, bottom_r))

  bottom <- setdiff(Reduce(intersect, l5), c(middle, top))
  bottom_l <- setdiff(Reduce(union, l5), c(top, middle, bottom, top_l, top_r, bottom_r))

  return (c(top, top_l, top_r, middle, bottom_l, bottom_r, bottom))
}

sum <- 0
for (i in 1:nrow(input)) {
  digits <- input[i, 1:10]
  positions <- deduct_mapping(digits)
  number <- 0
  for (j in 0:3) {
    digit <- unlist(strsplit(input[i, 12 + j], ''))
    # digit <- digits[]
    if (length(digit) == 2) {n <- 1}
    else if (length(digit) == 3) {n <- 7}
    else if (length(digit) == 4) {n <- 4}
    else if (length(digit) == 7) {n <- 8}
    else if (length(digit) == 5) {
        if (positions[2] %in% digit) {n <- 5} else if (positions[5] %in% digit) {n <- 2} else {n <- 3}
    }
    else if (length(digit) == 6) {
      if (!(positions[5] %in% digit)) {n <- 9} else if (!(positions[3] %in% digit)) {n <- 6} else {n <- 0}
    } else {cat(length(digit), '\n')}
    number <- number + n * 10^(3-j)
  }
  sum <- sum + number
}

cat('Part 2: ', sum)