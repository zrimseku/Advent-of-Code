input <- read.fwf('3/input.txt', header = F, width=rep(1, 12), colClasses = 'numeric')

# part 1
most_common <- unlist(lapply(input, median))
least_common <- rep(1, 12) - most_common

to_decimal <- function (bin) {
  dec <- 0
  for (i in 1:length(bin)) {
    dec <- dec + bin[i] * 2^(length(bin)-i)
  }
  dec
}

cat('Part 1: ', to_decimal(most_common) * to_decimal(least_common))

# part 2
oxygen <- input
co2 <- input
og_rating <- 0
cs_rating <- 0
for (i in 1:ncol(input)) {
  m1 <- as.numeric(median(oxygen[,i]) > 0)
  m2 <- abs(as.numeric(median(co2[,i]) > 0) - 1)
  if (nrow(oxygen) > 1) {oxygen <- oxygen[which(oxygen[i] == m1),]}
  if (nrow(co2) > 1) {co2 <- co2[which(co2[i] == m2),]}
}

og_rating <- unlist(oxygen[1,])
cs_rating <- unlist(co2[1, ])

cat('Part 2: ', to_decimal(og_rating) * to_decimal(cs_rating))
