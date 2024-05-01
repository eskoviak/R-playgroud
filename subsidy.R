# Subsidy/TaxedUpToSubsidy; s < p + t
A1 <- function(p,t,s) {
  A <- matrix(c(1+t/p, 1, 1, 0,
                0,1,0,0,
                0,0,1,1,
                0,0,0,1), 4, 4)
  b <- matrix(c(s,p,s,t), 4, 1)
  round(solve(A,b),2)
}

A2 <- function(p,t,s) {
  A <- diag(1,4,4)
  b <- matrix(c(p,t,0,0))
  round(solve(A,b),2)
}

A3 <- function(p,t,s) {
  A <- matrix(c(1,1,0,0,
                0,1,0,-t,
                0,0,1,0,
                0,0,0,p), 4,4)
  b <- matrix(c(s,p,0,0), 4,1)
  round(solve(A,b),2)
}

A4 <- function(p,t,s) {
  A <- diag(1,4,4)
  b <- matrix(c(p,0,0,0), 4,1)
  round(solve(A,b),2)
}

###################
# Using tax matrices
# tacc :== tax matrix for account
# tcon :== tax matrix for consumer
###################

#
# Subsidy/Subsidy Taxed
#
A0t <- function(p,tacc, tcon, s) {
  ta <- sum(tacc)
  tc <- sum(tcon)
  A <- matrix(c(1,1,-ta,0,
                0,1,0,-tc,
                0,0,1,0,
                0,0,0,1), 4, 4)
  b <- matrix(c(min(s,p),p,0,0), 4, 1)
  round(solve(A,b),2)
}

#
# Subsidy/TaxedUpToSubsidy
# * handles both s < p + t) and s >= p + t  
#
A1t <- function(p, tacc, tcon, s) {
  ta <- sum(tacc)
  tc <- sum(tcon)
  if (s < p * (1 + ta)) {
    A <- matrix(c(1+ta,1,1,0,
                  0,1,0,0,
                  0,0,1,1,
                  0,0,0,1), 4, 4)
    ;
    b <- matrix(c(s,p,s,p*tc), nrow = 4, ncol = 1)
  }
  else {A <- diag(1, 4, 4) ; b <- matrix(c(p, p*ta,0, 0),4,1) }
  round(solve(A,b),2)
}