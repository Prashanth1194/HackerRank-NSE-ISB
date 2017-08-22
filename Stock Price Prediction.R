d0 = read.csv("stocks_closing_prices.csv",stringsAsFactors = F)

fun1 = function(x)
{
  x$flag = NA
  x = x[order(x$Day.Sequence),]
  for(i in 2:nrow(x))
  {
    x$flag[i] = ifelse(x$Closing.Price[i]>=x$Closing.Price[i-1],1,0)
  }
  
  
  return(x)
}

d0 = by(d0,d0$Stock.ID,FUN = fun1)
d0 = data.frame(rbindlist(d0))

moving_avg = function(x)
{
  x$ltma = NA
  x$stma = NA
  x$lstma = NA
  x$ltma_flag = NA
  x$stma_flag = NA
  x$lstma_flag = NA
  x = x[order(x$Day.Sequence),]
  for(j in 247:nrow(x))
  {
    
    x$ltma[j] = sum(x$Closing.Price[(j-246):(j-1)])
    x$ltma[j] = x$ltma[j]/246
    
    x$stma[j] = sum(x$Closing.Price[(j-60):(j-1)])
    x$stma[j] = x$stma[j]/60
    
    x$lstma[j] = sum(x$Closing.Price[(j-5):(j-1)])
    x$lstma[j] = x$lstma[j]/5
    
    x$ltma_flag[j] = sum(x$flag[(j-246):(j-1)])
    x$stma_flag[j] = sum(x$flag[(j-60):(j-1)])
    x$lstma_flag[j] = sum(x$flag[(j-5):(j-1)])
  }
  
  return(x)
  
  
}

d1_a = by(d0,d0$Stock.ID,FUN = moving_avg)
library(data.table)
d1_a = data.frame(rbindlist(d1_a))


d1 = d1_a[d1_a$Day.Sequence>247,]

d1$year = ifelse(d1$Day.Sequence<=246,1,ifelse(d1$Day.Sequence>246 & d1$Day.Sequence<=(247+246-1),2,
                                               ifelse(d1$Day.Sequence>492 & d1$Day.Sequence<=(492+246-1),3,
                                                      ifelse(d1$Day.Sequence>739 & d1$Day.Sequence<=(739+246-1),4,5))))



library(DMwR)
d1$Weekdays = as.factor(d1$Weekdays)
regr.eval(d1$Closing.Price,d1$ltma)
regr.eval(d1$Closing.Price,d1$stma)
regr.eval(d1$Closing.Price,d1$lstma)

ID = d1$Stock.ID[d1$Day.Sequence==1230 & d1$Closing.Price>=500]
d3_1 = d1[d1$Stock.ID %in% ID,]

ID_1 = d1$Stock.ID[d1$Day.Sequence==1230 & d1$Closing.Price<500]
d3_2 = d1[d1$Stock.ID %in% ID_1,]

fit = lm(Closing.Price~.,data=d3_1[,-c(3,5)])
d3_1$pred = predict(fit)
regr.eval(d3_1$Closing.Price,d3_1$pred)

fit_1 = lm(Closing.Price~.,data=d3_2[,-c(3,5)])
d3_2$pred = predict(fit_1)
regr.eval(d3_2$Closing.Price,d3_2$pred)

### 1231st day
d4_1 = rep(2,length(unique(d3_1$Stock.ID)))

d4_1 = as.data.frame(d4_1)
names(d4_1)[1] = 'Weekdays'
d4_1$Stock.ID = unique(d3_1$Stock.ID)
d4_1$Day.Sequence = 1231
d4_1$year = 6
d4_1$Closing.Price = NA

d4_1 = rbind(d4_1,d3_1[d3_1$Day.Sequence>=984,c(1:4,12)])

d4_1 = by(d4_1,d4_1$Stock.ID,FUN = fun1)
d4_1 = data.frame(rbindlist(d4_1))

d4_1 = by(d4_1,d4_1$Stock.ID,FUN = moving_avg)
library(data.table)
d4_1_a = data.frame(rbindlist(d4_1))

d4_1_a = d4_1_a[d4_1_a$Day.Sequence==1231,]

d4_1_a$Weekdays = as.factor(d4_1_a$Weekdays)
d4_1_a$Closing.Price = predict(fit,d4_1_a)

#####

d5_1 = rep(3,length(unique(d3_1$Stock.ID)))

d5_1 = as.data.frame(d5_1)
names(d5_1)[1] = 'Weekdays'
d5_1$Stock.ID = unique(d3_1$Stock.ID)
d5_1$Day.Sequence = 1232
d5_1$year = 6
d5_1$Closing.Price = NA

d5_1 = rbind(d5_1,d4_1_a[,c(1:5)])
d5_1 = rbind(d5_1,d3_1[d3_1$Day.Sequence>=985,c(1:4,12)])

d5_1 = by(d5_1,d5_1$Stock.ID,FUN = fun1)
d5_1 = data.frame(rbindlist(d5_1))

d5_1 = by(d5_1,d5_1$Stock.ID,FUN = moving_avg)
library(data.table)
d5_1_a = data.frame(rbindlist(d5_1))

d5_1_a = d5_1_a[d5_1_a$Day.Sequence==1232,]

d5_1_a$Weekdays = as.factor(d5_1_a$Weekdays)
d5_1_a$Closing.Price = predict(fit,d5_1_a)

#########s
d6_1 = rep(4,length(unique(d3_1$Stock.ID)))

d6_1 = as.data.frame(d6_1)
names(d6_1)[1] = 'Weekdays'
d6_1$Stock.ID = unique(d3_1$Stock.ID)
d6_1$Day.Sequence = 1233
d6_1$year = 6
d6_1$Closing.Price = NA

d6_1 = rbind(d6_1,d4_1_a[,c(1:5)])
d6_1 = rbind(d6_1,d5_1_a[,c(1:5)])
d6_1 = rbind(d6_1,d3_1[d3_1$Day.Sequence>=986,c(1:4,12)])

d6_1 = by(d6_1,d6_1$Stock.ID,FUN = fun1)
d6_1 = data.frame(rbindlist(d6_1))

d6_1 = by(d6_1,d6_1$Stock.ID,FUN = moving_avg)
library(data.table)
d6_1_a = data.frame(rbindlist(d6_1))

d6_1_a = d6_1_a[d6_1_a$Day.Sequence==1233,]

d6_1_a$Weekdays = as.factor(d6_1_a$Weekdays)
d6_1_a$Closing.Price = predict(fit,d6_1_a)

d7_1_a = do.call(rbind,list(d4_1_a,d5_1_a,d6_1_a))

###########
d4_2 = rep(2,length(unique(d3_2$Stock.ID)))

d4_2 = as.data.frame(d4_2)
names(d4_2)[1] = 'Weekdays'
d4_2$Stock.ID = unique(d3_2$Stock.ID)
d4_2$Day.Sequence = 1231
d4_2$year = 6
d4_2$Closing.Price = NA

d4_2 = rbind(d4_2,d3_2[d3_2$Day.Sequence>=984,c(1:4,12)])

d4_2 = by(d4_2,d4_2$Stock.ID,FUN = fun1)
d4_2 = data.frame(rbindlist(d4_2))

d4_2 = by(d4_2,d4_2$Stock.ID,FUN = moving_avg)
library(data.table)
d4_2_a = data.frame(rbindlist(d4_2))

d4_2_a = d4_2_a[d4_2_a$Day.Sequence==1231,]

d4_2_a$Weekdays = as.factor(d4_2_a$Weekdays)
d4_2_a$Closing.Price = predict(fit_1,d4_2_a)

#####

d5_2 = rep(3,length(unique(d3_2$Stock.ID)))

d5_2 = as.data.frame(d5_2)
names(d5_2)[1] = 'Weekdays'
d5_2$Stock.ID = unique(d3_2$Stock.ID)
d5_2$Day.Sequence = 1232
d5_2$year = 6
d5_2$Closing.Price = NA

d5_2 = rbind(d5_2,d4_2_a[,c(1:5)])
d5_2 = rbind(d5_2,d3_2[d3_2$Day.Sequence>=985,c(1:4,12)])

d5_2 = by(d5_2,d5_2$Stock.ID,FUN = fun1)
d5_2 = data.frame(rbindlist(d5_2))

d5_2 = by(d5_2,d5_2$Stock.ID,FUN = moving_avg)
library(data.table)
d5_2_a = data.frame(rbindlist(d5_2))

d5_2_a = d5_2_a[d5_2_a$Day.Sequence==1232,]

d5_2_a$Weekdays = as.factor(d5_2_a$Weekdays)
d5_2_a$Closing.Price = predict(fit_1,d5_2_a)

#########s
d6_2 = rep(4,length(unique(d3_2$Stock.ID)))

d6_2 = as.data.frame(d6_2)
names(d6_2)[1] = 'Weekdays'
d6_2$Stock.ID = unique(d3_2$Stock.ID)
d6_2$Day.Sequence = 1233
d6_2$year = 6
d6_2$Closing.Price = NA

d6_2 = rbind(d6_2,d4_2_a[,c(1:5)])
d6_2 = rbind(d6_2,d5_2_a[,c(1:5)])
d6_2 = rbind(d6_2,d3_2[d3_2$Day.Sequence>=986,c(1:4,12)])

d6_2 = by(d6_2,d6_2$Stock.ID,FUN = fun1)
d6_2 = data.frame(rbindlist(d6_2))

d6_2 = by(d6_2,d6_2$Stock.ID,FUN = moving_avg)
library(data.table)
d6_2_a = data.frame(rbindlist(d6_2))

d6_2_a = d6_2_a[d6_2_a$Day.Sequence==1233,]

d6_2_a$Weekdays = as.factor(d6_2_a$Weekdays)
d6_2_a$Closing.Price = predict(fit_1,d6_2_a)

d7_2_a = do.call(rbind,list(d4_2_a,d5_2_a,d6_2_a))
#########

d7 = rbind(d7_1_a,d7_2_a)

d7 = d7[,c(2,3,5)]
d8 = reshape(d7,v.names = 'Closing.Price',idvar = 'Stock.ID',timevar = 'Day.Sequence',direction = 'wide')
d8 = d8[order(d8$Stock.ID),]

d9 = d0[d0$Day.Sequence==1230,c(3,4)]
d10 = merge(d8,d9,by='Stock.ID')
d10$diff = ((d10$Closing.Price.1233-d10$Closing.Price)/(d10$Closing.Price))*100

d11 = d10[d10$Closing.Price>=500,]
d11 = d11[order(-d11$diff),]
d11$Quantity = 0
d11$Quantity[1:10] = floor(1000000/d11$Closing.Price[1:10])

d11$Quantity1 = 0
d11$Quantity1[1:20] = floor(500000/d11$Closing.Price[1:20])
d11$Quantity2 = 0
d11$Quantity3 = 0



d12 = d10[d10$Closing.Price<500 & d10$Closing.Price>200,]
d12$Quantity = 0
d12$Quantity1 = 0
d12$Quantity2 = 0
d12$Quantity2[1:10] = floor(1000000/d12$Closing.Price[1:10])

d12$Quantity3 = 0
d12$Quantity3[1:20] = floor(500000/d12$Closing.Price[1:20])

d13 = do.call(rbind,list(d11,d12))

d13$Quantity4 = 0
d13$Quantity4[1:10] = floor(1000000/d13$Closing.Price[1:10])

d14 = d10[d10$Closing.Price<=200,]
d14$Quantity = 0
d14$Quantity1 = 0
d14$Quantity2 = 0
d14$Quantity3 = 0
d14$Quantity4 = 0

d15 = rbind(d13,d14)

names(d15)[1:4] = c("Stock_ID","Closing_Price_1231","Closing_Price_1232","Closing_Price_1233")                                                                                        

a = c(12,203,293,60,103,140,99,191,243,79,249,228)

d15$flag = ifelse(d15$Stock_ID %in% a,1,0)

d15$Quantity = ifelse(d15$flag==1,floor(1000000/d15$Closing.Price),0)
d15$Quantity = ifelse(d15$Stock_ID==249,38,ifelse(d15$Stock_ID==228,4,d15$Quantity))

d15 = d15[,c(1:4,7)]
write.csv(d15,'separate_models.csv',row.names = F)

