input <- read.delim('14/input', header = F, sep = ' ')

template <- input[1,1]

insertions <- input[2:nrow(input),c(1,3)]

for (s in 1:10) {
  new <- substr(template, 1, 1)
  for (i in 2:nchar(template)) {
    new <- paste0(new, insertions[which(insertions[,1] == substr(template, i-1, i)),2], substr(template, i, i))
  }
  template <- new
}

tab <- table(strsplit(template, ''))
cat('Part 1: ', max(tab) - min(tab))

# Part 2
template <- input[1,1]

pairs <- c()
for (i in 2:nchar(template)) {pairs <- c(pairs, substr(template, i-1, i))}

combinations <- table(pairs)
tab <- table(strsplit(template, ''))

for (l in insertions[,2]) {if (!(l %in% names(tab))) {tab[l] <- 0}}

for (i in 1:nrow(insertions)) {
  if (!(insertions[i,2] %in% names(tab))) {tab[insertions[i,2]] <- 0}
  if (!(insertions[i,1] %in% names(combinations))) {combinations[insertions[i,1]] <- 0}
}

for (s in 1:40) {
  to_add <- list()
  for (n in names(combinations)) {to_add[[n]] <- 0}
  for (i in 1:nrow(insertions)) {
    pair <- insertions[i, 1]
    new <- insertions[i, 2]
    new_pairs <- c(paste0(substr(pair, 1, 1), new), paste0(new, substr(pair, 2, 2)))
    before <- combinations[[pair]]    # saving in case "pair = new_pair"
    for (p in new_pairs) { to_add[[p]] <- to_add[[p]] + before }
    combinations[[pair]] <- combinations[[pair]] - before
    tab[[new]] <- tab[[new]] + before
  }
  for (add in (names(to_add))) { combinations[[add]] <- combinations[[add]] + to_add[[add]] }
}

cat('Part 2: ', format(max(tab) - min(tab), digits = 15))