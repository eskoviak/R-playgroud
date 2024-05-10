type_list <- c("CashandCarry", "PayrollDeduction", "Subsidy", "SubsidyandPayrollDeduction", "SubsidywithPcard")
test_data <- structure(list(p=as.numeric(),
                            type=character(),
                            s=as.numeric(),
                            s_type=character,
                            s_max=as.numeric()),
                       class="data.frame")

for (i in 1:nrow(tax_rules)) {
  p <- as.numeric(tax_rules[i,'paid sub amt']+tax_rules[i,'paid cons amt']+tax_rules[i,'paid PD amt'])
  if(is.na(p)) {next}
  type <- as.character(tax_rules[i,'program type'])
  if(is.na(type)) {next}
  tacc <- c(0,0,0,0)
  tcon <- c(0,0,0,0)
  tacc[1] <- as.numeric(gsub('%','',tax_rules[i,15]))/100
  tacc[2] <- as.numeric(gsub('%','',tax_rules[i,16]))/100
  tacc[3] <- as.numeric(gsub('%','',tax_rules[i,17]))/100
  tacc[4] <- as.numeric(gsub('%','',tax_rules[i,18]))/100
  tacc[is.na(tacc)] <- 0
  tcon <- tacc
  

  if(grep("#", tax_rules[1,4])) { s <- 0; s_type <- "NA"; s_max <- 0}
  else {
    if(grep("[$]",tax_rules[i,4])) { s <- as.numeric(gsub('$','',tax_rules[i,4])); s_type="dollar"; s_max=0 }
    if(grep("[%]",tax_rules[i,4])) {}
  }
  newrow = data.frame(p=p,type=type, s=s, s_type=s_type, s_max=s_max)
  test_data <- rbind(test_data, newrow)
}
