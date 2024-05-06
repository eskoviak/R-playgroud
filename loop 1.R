test_data <- structure(list(p=as.numeric(),
                            type=character()),
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
  newrow = data.frame(p=p,type=type)
  test_data <- rbind(test_data, newrow)
}
