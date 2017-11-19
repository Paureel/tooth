mns <- NULL
sds <- NULL
lambda <- 0.2
sims <- 10000
numexp <- 40
for (i in 1 : sims) {
m <- mean(rexp(numexp, lambda)) 
s <- sd(rexp(numexp, lambda))  
mns <- c(mns, m) 
sds <- c(sds, s)
}
hist(mns)
hist(sds)
hist(rexp(sims, 0.2))
print(mean(rexp(sims, 0.2)))