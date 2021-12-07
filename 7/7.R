input <- as.matrix(read.delim('7/input', header = F, sep = ','))

fuel_costs <- sum(input)
for (i in 1:max(input)) {fuel_costs <- c(fuel_costs, sum(abs(input - i)))}

cat('Part 1: ', min(fuel_costs))


fuel_costs2 <- c()
costs <- c(0)
for (i in 1:max(input)) {costs <- c(costs, tail(costs, 1) + i)}

for (i in 0:max(input)) {fuel_costs2 <- c(fuel_costs2, sum(costs[abs(input - i) + 1]))}

cat('Part 2: ', min(fuel_costs2))