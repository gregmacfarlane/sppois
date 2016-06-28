# from ?glm
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
# random spatial distance, symmetric and zero diagonal
relationship <- matrix(runif(81, 0, 1), 9)
relationship[lower.tri(relationship)] <- t(relationship)[lower.tri(relationship)]
diag(relationship) <- 0
W <- spdep::mat2listw(relationship)

