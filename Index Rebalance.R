### This code was used for priliminary analysis and eventually solver in excel was used to solve the problem

library(lpSolve)
profit <- read.csv("E:/Hackathons/HackerRank/ISB/profit.csv", stringsAsFactors=FALSE)
View(profit)
profit$upp=as.integer(200000/profit$Day_2)
profit$Profit=(profit$Day_2-profit$Day_0)/profit$Day_0


lprec <- make.lp(0, 51)
set.objfn(lprec, profit$Day_2)
add.constraint(lprec, profit$Day_2, ">=", 5790000)
add.constraint(lprec, profit$Day_2, "<=", 5800000)


set.bounds(lprec, lower = c(rep(1,51)), columns = c(seq(1,51,1)))
set.bounds(lprec, upper = profit$upp, columns = c(seq(1,51,1)))
dimnames(lprec) <- list(RowNames, ColNames)


solve(lprec)
get.objective(lprec)
get.variables(lprec)
get.constraints(lprec)


for( i in 1:51)
{
  set.type(lprec, i, "integer")
}

out=get.variables(lprec)
write.csv(out,"out.csv",row.names=F)
