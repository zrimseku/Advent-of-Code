input <- read.delim('12/input', header = F, sep = '-')

vertices <- unique(unlist(input))

edges <- list()
nr_visits <- list()

for (v in vertices) {
  edges[[v]] <- NULL
  nr_visits[[v]] <- 0
}

for (i in seq_len(nrow(input))) {
  row <- input[i,]
  edges[[row[,1]]] <- c(edges[[row[,1]]], row[2])
  edges[[row[,2]]] <- c(edges[[row[,2]]], row[1])
}

distinct_paths <- 0
edges[['end']] <- NULL

find_path <- function (v) {
  if (v == 'end') { distinct_paths <<- distinct_paths + 1 }

  if (v != toupper(v)) { nr_visits[[v]] <<- nr_visits[[v]] + 1 }

  for (n in edges[[v]]) { if (nr_visits[[n]] < 1) {find_path(n)} }

  if (v != 'start') { nr_visits[[v]] <<- nr_visits[[v]] - 1 }
}

nr_visits[['start']] <- 1
find_path('start')

cat('Part 1: ', distinct_paths)


# Part 2
distinct_paths <- 0
nr_visits <- list()

for (v in vertices) { nr_visits[[v]] <- 0 }

find_path2 <- function (v) {
  if (v == 'end') { distinct_paths <<- distinct_paths + 1 }

  if (v != toupper(v)) { nr_visits[[v]] <<- nr_visits[[v]] + 1 }

  for (n in edges[[v]]) {
    if (n != 'start') {
      if (nr_visits[[n]] < 1) { find_path2(n) }
      else if (nr_visits[[n]] == 1) { find_path(n) }
    }
  }

  if (v != 'start') { nr_visits[[v]] <<- nr_visits[[v]] - 1}
}

nr_visits[['start']] <- 1
find_path2('start')

cat('Part 2: ', distinct_paths)