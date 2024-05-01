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
# shape 1,x (x = rates for each jurisdiction)
# no need at this time for them to be same shape
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
                  0,1,0,-tc,
                  0,0,1,0,
                  0,0,0,1), 4, 4)
    ;
    b <- matrix(c(s,p,s,0), nrow = 4, ncol = 1)
  }
  else {A <- diag(1, 4, 4) ; b <- matrix(c(p, 0, p*ta, 0),4,1) }
  round(solve(A,b),2)
}

#
# Subsidy/TaxedUpToSubsidy
# * handles both s < p + t and s >= p +t
# * handles both $ and %
# new structure instead of s:
# sub :== [ <dollar amount>, <percent amount>, <percent max>]
#   where either:  sub[1] <dollar amount> is > 0 AND sub[2:3] is 0
#   OR             
#      sub[1] = 0 
#      AND 0 <= sub[2] <= 1.0 
#      AND 0 < = sub[3] <= 3000.0 
A1t1 <- function(p, tacc, tcon, sub) {
  ta <- sum(tacc)
  tc <- sum(tcon)
  if (sub[1] > 0) { # $
    s = sub[1];
    dollar = 1;
  }
  else { # %
    s = min(p*sub[2],sub[3]);
    dollar = 0;
    surplus <- max(0,sub[3]-s) # surplus :== remain of max - (% of price)
  }
    if (s < p * (1 + ta)) {
    A <- matrix(c(1+ta,1,1,0,
                  0,1,0,-tc,
                  0,0,1,0,
                  0,0,0,1), 4, 4);
    b <- matrix(c(s,p,s,0), nrow = 4, ncol = 1)
  }
  else {A <- diag(1, 4, 4) ; b <- matrix(c(p, 0, p*ta, 0),4,1) }
  round(solve(A,b),2)
}