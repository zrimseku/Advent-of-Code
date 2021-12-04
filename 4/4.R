input <- read.delim('4/input.txt', header = F, sep = ',')

drawn <- input[1,]
boards_input <- input[2:501, 'V1']

boards <- data.frame()

for (b_row in boards_input) {
  b <- trimws(b_row, which = "left")
  b_split <- unlist(strsplit(b, split="\\s{1,}"))
  boards <- rbind(boards, as.integer(b_split))
}

marked <- boards
marked[,] <- 0
stop <- F

for (number in drawn) {
  marked[which(boards == number, arr.ind = T)] <- 1
  for (player in 0:99) {
    player_marked <- marked[(player*5 + 1):((player+1) * 5),]
    r <- rowSums(player_marked)
    c <- colSums(player_marked)
    if (5 %in% r | 5 %in% c) {
      stop <- T
      break
    }
  }
  if (stop) {break}
}

winning_board <- boards[(player*5 + 1):((player+1) * 5),]
winning_board[which(player_marked == 1, arr.ind = T)] <- 0

cat('Part 1: ', sum(winning_board) * number)

# part 2

marked <- boards
marked[,] <- 0
won <- rep(0, 100)
last_won <- 0

for (number in drawn) {
  marked[which(boards == number, arr.ind = T)] <- 1
  for (player in 0:99) {
    if (won[player+1] == 0) {
      player_marked <- marked[(player*5 + 1):((player+1) * 5),]
      r <- rowSums(player_marked)
      c <- colSums(player_marked)
      if (5 %in% r | 5 %in% c) {
        won[player + 1] <- 1
        last_won <- player
      }
    }
  }
  if (sum(won) == 100) {break}
}

last_winning_board <- boards[(last_won*5 + 1):((last_won+1) * 5),]
last_winning_board[which(player_marked == 1, arr.ind = T)] <- 0

cat('Part 2: ', sum(last_winning_board) * number)