pglsfit<-pgls(log(distinctdf$HM)~log(distinctdf$Skull.width..mm.), data = birdCDO, #check comparative data object here<---
              lambda = 'ML', #find lambda using maximum likelihood
              bounds = list(lambda=c(0.00001,1)))
print(summary(pglsfit))



#ones without headmass
length(df$Binomial[which(is.na(df$HM))])
#nrow(df[which(is.na(df$Head.mass..g.)),])
#df$Binomial[which(is.na(df$Skull.width..mm.))]

#add imputed head mass that are NA from the specimen's skull width
df$HM[which(is.na(df$HM))]<-
  exp(log(df$Skull.width..mm.[which(is.na(df$HM))])*pglsfit$model$coef[2]+
        pglsfit$model$coef[1])#WCP
df$HM



















