X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
n <- c(6,5,4,3,2,1)
test <- data.frame(X1,X2,X3,n)

## compute the descriptive statistics of the artificial dataset
destat(test)

write.csv(rankings_19,"rankings2019.csv", row.names = FALSE)
write.csv(records_19,"records2019.csv", row.names = FALSE)

## create an artificial dataset
X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
test <- data.frame(X1,X2,X3)

## aggregate the ranking of all the observations and create a summary matrix
test2 <- rankagg(test)
view(test2)
