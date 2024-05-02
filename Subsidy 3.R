#
# Subsidy 3 
#
# Parameters
# p :== price of item
# tacc :== a vector of the tax rates for the account (adjusted for any exclusions)
# tcon :== a vector of the tax rates for the consumer (adjusted)
# sub :== a three element vector:
#   sub[1] :== the $ subsidy, or 0
#   sub[2] :== the % subsidy, or 0
#   sub[3] :== max %, value 0 to 3000
#   sub[4] :== 0 Subsidy, 1 TaxedUpToSubsidy
# pd :== 0 (not payroll deduct) or 1 (payroll deduct)
# 
# returns x, a matrix shape(6,1)
#   x[1] :== Account Cost
#   x[2] :== Account Tax
#   x[3] :== Guest Cost
#   x[4] :== Guest Tax
#   x[5] :== Payroll Deduct Cost
#   x[6] :== Payroll Deduct Tax
#
# intermediate values
# A :== Coefficient Matrix, shape(6,6)
# b :== intercept Matrix, shape(6,1)
# ta :== effective tax rate for account
# tc :== effective tax rate for consumer

library(dst)

# Base matrices
A_base <- diag(n = 1, nrow = 6, ncol = 6)    # 6x6 Identity Coefficient Matrix
b_base <- matrix(data = 0, nrow = 6, ncol = 1)  # 6x1 intercept matrix

# Printer function
print_ans <- function(ans_matrix) {
  #print(ans_matrix)
  print(paste("Account Cost:   ", ans_matrix[1,1]))
  print(paste("Account Tax:    ", ans_matrix[2,1]))
  print(paste("Guest Cost:     ", ans_matrix[3,1]))
  print(paste("Guest Tax:      ", ans_matrix[4,1]))
  print(paste("PayDeduct Cost: ", ans_matrix[5,1]))
  print(paste("PayDeduct Tax:  ", ans_matrix[6,1]))
  

}
print_info <- function(sub = 0, pd = 0) {
  info <- paste(if (sub[1] > 0) {"$"} else {paste(paste(sub[2],"% "), paste(sub[3]," Max")) })
  print(info)
}

# Cash and Carry, all cases
cash_and_carry <- function(p, tacc = 0, tcon, sub = 0, pd = 0) {
  # ignoring sub and pd.
  A <- A_base
  b <- b_base
  b[3,1] <- p               # price
  b[4,1] <- p * sum(tcon)   # tax
  #b <- matrix(c(0, 0, p, p * sum(tcon), 0, 0), 6, 1)
  print_ans(round(solve(A, b), 2))
}

# PayrollDeduction, All Cases
PayrollDeduction <- function(p, tacc, tcon, sub = 0, pd) {
  # ignoring sub
  ta <- sum(tacc)
  tc <- sum(tcon)
  A <- diag(1,6,6)
  b <- matrix(c(0,0,if(!pd) p else 0,if(!pd) {p*tc} else 0,pd*p,pd*p*ta),6,1)
  print_ans(round(solve(A,b),2))
}

Subsidy <- function(p, tacc, tcon, sub, pd) {
  ta <- sum(tacc)
  tc <- sum(tcon)
#  if(!pd) {
#  A <- matrix(c(1+(if(sub[4]){ta} else {0}),-ta,1,0,0,0,
#                0,1,0,0,0,0,
#                0,0,1,-tc,0,0,
#                0,0,0,1,0,0,
#                0,0,0,0,1,0,
#                0,0,0,0,0,1), 6,6);
#    if(sub[1] > 0) {
#    b <- matrix(c(min(sub[1],p),0,p,0,0,0),6,1);
#    
#  } else {
#    A <- matrix(c(if(p * (1*ta) < sub[3]) {1} else {1+(if(sub[4]){ta} else {0})},-ta,0,0,1,0,
#                  0,1,0,0,0,0,
#                  0,0,1,0,0,0,
#                  0,0,0,1,0,0,
#                  0,0,0,0,1,-tc,
#                  0,0,0,0,0,1), 6, 6)
#    
#    #A
#    b <- matrix(c(min(p*sub[2],sub[3]),0,0,0,p,0))
#    #b
#    #if( p * (1+ta) < sub[3]) {A[1,1] <- 1}
#    #b <- matrix(c(min(p*sub[2],sub[3]),0,p,0,0,0),6,1)
#  }
#  
#  round(solve(A,b),2)
  
  A = diag(1,6,6)
  A[2,1] = -ta
  b = matrix(c(0,0,0,0,0,0),6,1)
  
  
  # Case a
  if(sub[1] & !sub[4] & !pd)
  {
    print("In case a:")
    print_info(sub,pd)
    A[3,1] = 1;
    A[4,3] = -tc;
    b[3] = p;
    b[1] = min(sub[1],p);
  }
  
  # Case b
  if(sub[1] & sub[4] & !pd)
  {
    print("In case b: $, TaxedUpToSubsidy, not PayrollDeduct");
    A[1,1] = 1+ta;
    A[3,1] = 1;
    A[4,3] = -tc;
    b[3] = p;
    b[1] = min(sub[1],p)
  }
  
  # Case c
  if(!sub[1] & !sub[4] & !pd)
  {
    print("In case c ");
    A[3,1] = 1;
    A[4,3] = -tc;
    b[1] = min(p*sub[2], sub[3]);
    b[3] = p;
  }
  
  # case d
  if(!sub[1] & sub[4] & !pd)
  {
    print("In case d");
    if (p*(1+ta) < sub[3]) { } else { A[1,1] = 1+ta };
    A[3,1] = 1;
    A[4,3] = -tc;
    b[1] = min(p*sub[2], sub[3]);
    b[3] = p;
  }
  
  # Case e
  if(sub[1] & !sub[4] & pd)
  {
    print("In case e");
    A[5,1] = 1;
    A[6,5] = -ta
    b[1] = min(p, sub[1]);
    b[5] = p
  }
  
  # Case f
  if(sub[1] & sub[4] & pd)
  {
    print("In case f");
    A[1,1] = 1+ta;
    A[5,1] = 1;
    A[6,5] = -ta;
    b[1] = min(p, sub[1]);
    b[5] = p;
  }
  
  # Case g
  if(!sub[1] & !sub[4] & pd)
  {
    print("In case g");
    A[5,1] = 1;
    A[6,5] = -ta;
    b[1] = min(p*sub[2], sub[3]);
    b[5] = p
  }
  
  # case h
  if(!sub[1] & sub[4] & pd)
  {
    print("In case h");
    A[1,1] = 1+ta;
    A[5,1] = 1
    A[6,5] = -ta
    b[1] = min(p*sub[2], sub[3]);
    b[5] = p
  }
  
  print_ans(round(solve(A,b),2))

  

}