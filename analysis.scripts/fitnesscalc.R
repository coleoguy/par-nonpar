# Mean fitness for females h = 0
dat <- read.csv('fitness_h0.csv')

p1 <- dat[,c(2,4,8,10)]
p2 <- rowSums(p1)
p <- mean(p2/1000)

q1 <- dat[,c(5,7,11,13)]
q2 <- rowSums(q1)
q <- mean(q2/1000)

s <- 0.3
h <- 0

genotype.fit.mat  <- matrix(c(1+s, 1+h*s, 1, 
                              1/(1+s), 1/(1+h*s), 1),
                            3, 2, byrow = F)
colnames(genotype.fit.mat) <- c("male", "female")
rownames(genotype.fit.mat) <- c("00","01","11")

fitness1 <- ((p^2)*genotype.fit.mat[1,2]) + ((2*(p*q))*genotype.fit.mat[2,2]) + ((q^2)*genotype.fit.mat[3,2])


# Mean fitness for females h = 1
dat <- read.csv('fitness_h1.csv')

p1 <- dat[,c(2,4,8,10)]
p2 <- rowSums(p1)
p <- mean(p2/1000)

q1 <- dat[,c(5,7,11,13)]
q2 <- rowSums(q1)
q <- mean(q2/1000)

s <- 0.3
h <- 1

genotype.fit.mat  <- matrix(c(1+s, 1+h*s, 1, 
                              1/(1+s), 1/(1+h*s), 1),
                            3, 2, byrow = F)
colnames(genotype.fit.mat) <- c("male", "female")
rownames(genotype.fit.mat) <- c("00","01","11")

fitness2 <- ((p^2)*genotype.fit.mat[1,2]) + ((2*(p*q))*genotype.fit.mat[2,2]) + ((q^2)*genotype.fit.mat[3,2])


# Mean fitness for males h = 0
dat <- read.csv('fitness_Y_h0.csv')

p1 <- dat[,c(14,16)]
p2 <- rowSums(p1)
p <- mean(p2/1000)

q1 <- dat[,c(17,19)]
q2 <- rowSums(q1)
q <- mean(q2/1000)

s <- 0.3
h <- 0

genotype.fit.mat  <- matrix(c(1+s, 1+h*s, 1, 
                              1/(1+s), 1/(1+h*s), 1),
                            3, 2, byrow = F)
colnames(genotype.fit.mat) <- c("male", "female")
rownames(genotype.fit.mat) <- c("00","01","11")

fitness3 <- ((p^2)*genotype.fit.mat[1,1]) + ((2*(p*q))*genotype.fit.mat[2,1]) + ((q^2)*genotype.fit.mat[3,1])



# Mean fitness for males h = 1
dat <- read.csv('fitness_Y_h1.csv')

p1 <- dat[,c(14,16)]
p2 <- rowSums(p1)
p <- mean(p2/1000)

q1 <- dat[,c(17,19)]
q2 <- rowSums(q1)
q <- mean(q2/1000)

s <- 0.3
h <- 1

genotype.fit.mat  <- matrix(c(1+s, 1+h*s, 1, 
                              1/(1+s), 1/(1+h*s), 1),
                            3, 2, byrow = F)
colnames(genotype.fit.mat) <- c("male", "female")
rownames(genotype.fit.mat) <- c("00","01","11")

fitness4 <- ((p^2)*genotype.fit.mat[1,1]) + ((2*(p*q))*genotype.fit.mat[2,1]) + ((q^2)*genotype.fit.mat[3,1])