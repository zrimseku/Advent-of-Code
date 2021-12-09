input <- read.fwf('9/input', header = F, width=rep(1, 100), colClasses = 'numeric')

# Part 1
sum_risk <- 0
for (i in 1:nrow(input)) {
  for (j in 1:ncol(input)) {
    is_low_pt <- T
    pt <- input[i, j]
    if (i != 1) {if (input[i-1, j] <= pt) {is_low_pt <- F}}
    if (i != nrow(input)) {if (input[i+1, j] <= pt) {is_low_pt <- F}}
    if (j != 1) {if (input[i, j-1] <= pt) {is_low_pt <- F}}
    if (j != ncol(input)) {if (input[i, j+1] <= pt) {is_low_pt <- F}}
    if (is_low_pt) {sum_risk <- sum_risk + 1 + pt}
  }
}

cat('Part 1: ', sum_risk)

# Part 2
basin_sizes <- c()

unvisited <- input != 9

add_visits <- function (i, j, unvisited) {
  if (i == 1) {lr <- 1} else if (i == nrow(input)) {lr <- -1} else {lr <- c(1, -1)}
  if (j == 1) {ud <- 1} else if (j == ncol(input)) {ud <- -1} else {ud <- c(1, -1)}
  to_visit <- matrix(ncol = 2, nrow=0)
  for (l in lr) {
      if (unvisited[i+l, j]) {
        to_visit <- rbind(to_visit, c(i+l, j))
        unvisited[i+l, j] <- F
      }
  }
  for (u in ud) {
      if (unvisited[i, j+u]) {
        to_visit <- rbind(to_visit, c(i, j+u))
        unvisited[i, j+u] <- F
      }
  }
  result <- list(to_visit, unvisited)
  return (result)
}

for (i in 1:nrow(input)) {
  for (j in 1:ncol(input)) {
    if (unvisited[i, j]) {
      basin_size <- 1
      unvisited[i, j] <- F
      result <- add_visits(i, j, unvisited)
      to_visit <- result[[1]]
      unvisited <- result[[2]]
      while (nrow(to_visit) > 0) {
        basin_size <- basin_size + 1
        position <- to_visit[1,]
        result <- add_visits(position[1], position[2], unvisited)
        unvisited <- result[[2]]
        to_visit <- rbind(to_visit[-1,], result[[1]])
      }
      basin_sizes <- c(basin_sizes, basin_size)
    }
  }
}

cat('Part 2: ', prod(sort(basin_sizes, decreasing = T)[1:3]))