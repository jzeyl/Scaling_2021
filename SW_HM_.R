pglsfit<-pgls(log(distinctdf$Head.mass..g.)~log(distinctdf$Skull.width..mm.), data = birdCDO, #check comparative data object here<---
              lambda = 'ML', #find lambda using maximum likelihood
              bounds = list(lambda=c(0.00001,1)))
print(summary(pglsfit))



#ones without headmass
length(df$Binomial[which(is.na(df$Head.mass..g.))])
#nrow(df[which(is.na(df$Head.mass..g.)),])
#df$Binomial[which(is.na(df$Skull.width..mm.))]

#add imputed head mass that are NA from the specimen's skull width
df$Head.mass..g.[which(is.na(df$Head.mass..g.))]<-
  exp(log(df$Skull.width..mm.[which(is.na(df$Head.mass..g.))])*pglsfit$model$coef[2]+
        pglsfit$model$coef[1])#WCP
df$Head.mass..g.



















