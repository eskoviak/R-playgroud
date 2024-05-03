#
# Subsidy  
#
# Parameters
# p :== price of item
# tacc :== a scalar or vector of the tax rates for the account (adjusted for any exclusions)
# tcon :== a scalar or vector of the tax rates for the consumer (adjusted)
# sub :== a four element vector:
#   sub[1] :== the $ subsidy, or 0
#   sub[2] :== the % subsidy, or 0
#   sub[3] :== max %, value 0 to 3000
#   sub[4] :== 0 Subsidy, 1 TaxedUpToSubsidy
# pd :== 0 (not payroll deduct) or 1 (payroll deduct)
# 
# returns x, a matrix--shape(6,1)
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

#library(dst)

# Base matrices
A_base <- diag(n = 1, nrow = 6, ncol = 6)    # 6x6 Identity Coefficient Matrix
b_base <- matrix(data = 0, nrow = 6, ncol = 1)  # 6x1 intercept matrix

# Printer function -- Pretty Prints the result
print_ans <- function(ans_matrix) {
  print(paste("Account Cost:   ", ans_matrix[1,1]))
  print(paste("Account Tax:    ", ans_matrix[2,1]))
  print(paste("Guest Cost:     ", ans_matrix[3,1]))
  print(paste("Guest Tax:      ", ans_matrix[4,1]))
  print(paste("PayDeduct Cost: ", ans_matrix[5,1]))
  print(paste("PayDeduct Tax:  ", ans_matrix[6,1]))
}

# Decodes the sub matrix and pd flags
print_info <- function(sub = 0, pd = 0) {
  info <- paste(if (sub[1] > 0) {"$, "} else {paste(paste(sub[2],"% :"), paste(sub[3]," Max, ")) })
  info <- paste(info, (if (sub[4] == 0) {"SubsidyTaxed, "} else {"TaxedUpToSubsidy, "}))
  info <- paste(info, (if (pd == 0) {"Not Payroll Deduct"} else {"Payroll Deduct"}))
  print(info)
}

# Cash and Carry, all cases
CashandCarry <- function(p, tacc = 0, tcon, sub = 0, pd = 0) {
  A <- A_base
  b <- b_base
  b[3,1] <- p               
  b[4,1] <- p * sum(tcon)
  
  return(round(solve(A,b),2))
}

# PayrollDeduction, All Cases
PayrollDeduction <- function(p, tacc, tcon, sub = 0, pd) {
  A <- A_base
  b <- b_base
  if (pd) {
    b[5,1] <- p;
    b[6,1] <- p* sum(tacc)
  } else {
    b[3,1] <- p;
    b[4,1] <- p*sum(tcon)
  }

  return(round(solve(A,b),2))
}

Subsidy <- function(p, tacc, tcon, sub, pd) {
  # initialize common entities
  ta <- sum(tacc)
  tc <- sum(tcon)
  A = diag(1,6,6)
  A[2,1] = -ta
  b = matrix(c(0,0,0,0,0,0),6,1)
  
  
  # Case a $,SubsidyTaxed,not PD
  if(sub[1] & !sub[4] & !pd)
  {
    print("In case a:")
    A[3,1] = 1;
    A[4,3] = -tc;
    b[3] = p;
    b[1] = min(sub[1],p);
  }
  
  # Case b $,TaxedUpToSubsidy, not PD
  if(sub[1] & sub[4] & !pd)
  {
    print("In case b:");
    A[1,1] = 1+ta;
    A[3,1] = 1;
    A[4,3] = -tc;
    b[3] = p;
    b[1] = min(sub[1],p)
  }
  
  # Case c %, SubsidyTaxed, not PD
  if(!sub[1] & !sub[4] & !pd)
  {
    print("In case c:");
    A[3,1] = 1;
    A[4,3] = -tc;
    b[1] = min(p*sub[2], sub[3]);
    b[3] = p;
  }
  
  # case d %, TaxedUpToSubsidy, not PD
  if(!sub[1] & sub[4] & !pd)
  {
    print("In case d");
    if (p*(1+ta) < sub[3]) { } else { A[1,1] = 1+ta };
    A[3,1] = 1;
    A[4,3] = -tc;
    b[1] = min(p*sub[2], sub[3]);
    b[3] = p;
  }
  
  # Case e $ SubsidyTaxed, PD
  if(sub[1] & !sub[4] & pd)
  {
    print("In case e");
    A[5,1] = 1;
    A[6,5] = -ta
    b[1] = min(p, sub[1]);
    b[5] = p
  }
  
  # Case f $,TaxedUpToSubsidy, PD
  if(sub[1] & sub[4] & pd)
  {
    print("In case f");
    A[1,1] = 1+ta;
    A[5,1] = 1;
    A[6,5] = -ta;
    b[1] = min(p, sub[1]);
    b[5] = p;
  }
  
  # Case g %, SubsidyTaxed, PD
  if(!sub[1] & !sub[4] & pd)
  {
    print("In case g");
    A[5,1] = 1;
    A[6,5] = -ta;
    b[1] = min(p*sub[2], sub[3]);
    b[5] = p
  }
  
  # case h %, TaxedUpToSubsidy, PD
  if(!sub[1] & sub[4] & pd)
  {
    print("In case h");
    A[1,1] = 1+ta;
    A[5,1] = 1
    A[6,5] = -ta
    b[1] = min(p*sub[2], sub[3]);
    b[5] = p
  }

  return(round(solve(A,b),2))
}