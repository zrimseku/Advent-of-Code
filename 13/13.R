input <- read.delim('13/input', header = F, sep = ',')

coordinates <- na.omit(input)
coordinates[,1] <- as.numeric(coordinates[,1])
coordinates[,] <- coordinates[,] + 1

paper <- matrix(ncol=max(coordinates[,1]), nrow=max(coordinates[,2]), data=0)

for (i in seq_len(nrow(coordinates))) { paper[coordinates[i, 2], coordinates[i, 1]] <- 1 }

folds <- input[is.na(input)[,'V2'],1]

f <- folds[1]

spl <- unlist(strsplit(f, split = '='))
coord <- substr(spl[1], nchar(spl[1]), nchar(spl[1]))
val <- as.numeric(spl[2]) + 1
if (coord == 'x') {
  paper[,(2*val - ncol(paper)):(val - 1)] <- paper[,(2*val - ncol(paper)):(val - 1)] + paper[,ncol(paper):(val + 1)]
  paper <- paper[,1:(val - 1)]
} else {
  paper[(2*val - nrow(paper)):(val - 1),] <- paper[(2*val - nrow(paper)):(val - 1),] + paper[nrow(paper):(val + 1),]
  paper <- paper[1:(val - 1),]
}

cat('Part 1: ', sum(paper > 0))

# Part 2

for (f in folds[2:length(folds)]) {
  spl <- unlist(strsplit(f, split = '='))
  coord <- substr(spl[1], nchar(spl[1]), nchar(spl[1]))
  val <- as.numeric(spl[2]) + 1
  if (coord == 'x') {
    if (2*val - ncol(paper) > 0) {
      paper[,(2*val - ncol(paper)):(val - 1)] <- paper[,(2*val - ncol(paper)):(val - 1)] + paper[,ncol(paper):(val + 1)]
      paper <- paper[,1:(val - 1)]
    } else {
      paper[,(val + 1):(2*val)] <- paper[,(val + 1):(2*val)] + paper[,(val - 1):1]
      paper <- paper[,ncol(paper):(val + 1)]
    }
  } else {
    if (2*val - nrow(paper) > 0) {
      paper[(2*val - nrow(paper)):(val - 1),] <- paper[(2*val - nrow(paper)):(val - 1),] + paper[nrow(paper):(val + 1),]
      paper <- paper[1:(val - 1),]
    } else {
      paper[(val + 1):(2*val),] <- paper[(val + 1):(2*val),] + paper[(val - 1):1,]
      paper <- paper[nrow(paper):(val + 1),]
    }
  }
}

paper[which(paper > 0)] <- '#'
paper[which(paper == 0)] <- ''
paper