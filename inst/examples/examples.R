n <- 1e6
W <- matrix(rnorm(n*3), ncol = 3)
Y.1 <- rbinom(n, 1, plogis(1 + 0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3]^2 ))
Y.0 <- rbinom(n, 1, plogis(0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3]^2 ))

n <- 1e3
W <- matrix(rnorm(n*3), ncol = 3)
A <- rbinom(n, 1, 1 / (1 + exp(-(.2*W[,1] - .1*W[,2] + .4*W[,3]))))
Y <- rbinom(n, 1, plogis(A + 0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3]^2 ))
obs <- rbinom(n, 1, 1 / (1 + exp(.2*W[,1] - .1*W[,2])))

tmp <- data.frame(W, A, Y)
tmp$A <- as.factor(tmp$A)
tmp$Y <- ifelse(obs == 0, NA_real_, tmp$Y)

psi <- tml3(tmp, "A", "Y", c("X1", "X2", "X3"),
            learners_outcome = c("glm", "ranger"),
            folds = 10,
            super_efficient = FALSE)
psi

mean(Y.1)
mean(Y.0)
