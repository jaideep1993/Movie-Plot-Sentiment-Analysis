## Web Data Analytics ##
##       Team - 4 - Section 2      ##
## Team Members: Saurabh Suman, Abhishek Talwar, Sagar Pradhan, Jaideep Dutta ##

#Reading in the datasets
final <- read.csv(file="final.csv",header=T, sep=",")
names(final)
final$delta21=abs(final$delta21)
final$delta32=abs(final$delta32)
final$delta43=abs(final$delta43)

#calculate the average sentiment
final$avg_sentiment_plot = (final$azure_sentiments_Setup + final$azure_sentiments_Dev + 
                              final$azure_sentiments_Cmp + final$azure_sentiments_Clmx)/4
#rename the average sentiment to average sentiment delta to prevent conflict in names
names(final)[names(final) == 'avg_sentiment'] <- 'avg_sentiment_delta'
names(final)

#doing data exporation 
hist(final$roi, xlab="roi", ylab="movies", main="Distribution of roi")
hist(log(1+final$roi), xlab="Log(roi)", ylab="movies", main="Distribution of Logged roi")

df=final[,c('roi','avg_sentiment_delta','avg_sentiment_plot','runtime','azure_sentiments_Clmx',
            'azure_sentiments_Cmp','azure_sentiments_Dev','azure_sentiments_Setup','delta21','delta32',
            'delta43','budget','revenue','year')]

#check for skewness
library(e1071)   
for(i in 1:ncol(df)){
  print(paste(colnames(df)[i],skewness(df[,i])))
}

skewness(log(1+df$roi))
table(df$year)

# Testing out various models
#running log log regression model 
fit8 = lm(log(1+roi) ~ log(1+avg_sentiment_delta)+runtime+azure_sentiments_Clmx+azure_sentiments_Cmp+
            azure_sentiments_Setup+delta21+delta32+log(1+avg_sentiment_plot)+budget, data = final)
summary(fit8)

#separating out runtime feature into categorical across median
test=final

median(test$runtime)
test$longMovie <- ifelse(test$runtime >=median(test$runtime), 1, 0)
test$longMovie = as.factor(test$longMovie)

#log log regression with interaction terms
fit10 = lm(log(1+roi) ~ log(1+avg_sentiment_delta)*longMovie+azure_sentiments_Clmx+azure_sentiments_Cmp+
             azure_sentiments_Setup+delta21+delta32+log(1+avg_sentiment_plot)*longMovie+budget, data = test)
summary(fit10)

cc = fit10$coefficients

(eqn <- paste("log(roi) =", paste(round(cc[1],2), paste(round(cc[-1],2), names(cc[-1]), sep=" * ", collapse=" + "), sep=" + "), "+ e"))

cc = fit8$coefficients

(eqn <- paste("log(roi+1) =", paste(round(cc[1],2), paste(round(cc[-1],2), names(cc[-1]), sep=" * ", collapse=" + "), sep=" + "), "+ e"))

# Exporting out intermediate file
write.csv(test,file='test1.csv')

# Reading in the file
test2 <- read.csv(file="test1.csv",header=T, sep=",")
test2$delta21=test2$azure_sentiments_Cmp-test2$azure_sentiments_Setup
test2$delta32=test2$azure_sentiments_Dev-test2$azure_sentiments_Cmp
test2$delta43=test2$azure_sentiments_Clmx-test2$azure_sentiments_Dev

test2$avg_sentiment_delta = (test2$delta21 + test2$delta32 + test2$delta43)/3

#some additional data explorations
boxplot(test$revenue, main="Boxplot of Revenue")
boxplot(test$budget, main="Boxplot of Budget")
boxplot(test$runtime, main="Boxplot of Runtime")

#some additional data explorations using scatter plot
#plot(roi ~ UnitPrice, data=rtl)

plot(roi ~ avg_sentiment_plot, data=test
     , ylim=c(0,500)
     , type="p" #points
     , col="dark green"
     , main="Scatterplot of avg_sentiment_plot vs. roi"
)

plot(roi ~ avg_sentiment_delta, data=test
     , ylim=c(0,500)
     , type="p" #points
     , col="dark green"
     , main="Scatterplot of avg_sentiment_delta vs. roi"
)

plot(roi ~ runtime, data=test
     , ylim=c(0,500)
     , type="p" #points
     , col="dark green"
     , main="Scatterplot of runtime vs. roi"
)

plot(roi ~ budget, data=test
     , ylim=c(0,500)
     , type="p" #points
     , col="dark green"
     , main="Scatterplot of budget vs. roi"
)

#some additional data explorations using correlation heatmap
cormat<-signif(cor(test[,c('avg_sentiment_plot','avg_sentiment_delta','budget','runtime','roi','azure_sentiments_Clmx','azure_sentiments_Cmp' ,'azure_sentiments_Setup','azure_sentiments_Dev','delta21','delta32','delta43')]),2)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cormat, col=col, symm=TRUE)

# Histogram 
hist(test$revenue)
hist(test$budget)
hist(test$runtime)

# Final Models
# Fitting a Model with interaction between avg_sentiment_delta & length of the movie
fit15 = lm(log(1+roi) ~ log(1+avg_sentiment_delta)*longMovie+azure_sentiments_Clmx+azure_sentiments_Cmp+azure_sentiments_Setup+delta21+delta32+log(1+avg_sentiment_plot)+budget, data = test)
summary(fit15)

cc = fit15$coefficients

(eqn <- paste("log(1+roi) =", paste(round(cc[1],2), paste(round(cc[-1],2), names(cc[-1]), sep=" x ", collapse=" + "), sep=" + "), "+ e"))

# Fitting a model without an interaction term
fit16 = lm(log(1+roi) ~ log(1+avg_sentiment_delta) +longMovie+azure_sentiments_Clmx+azure_sentiments_Cmp+azure_sentiments_Setup+delta21+delta32+log(1+avg_sentiment_plot)+budget, data = test)
summary(fit16)

cc = fit16$coefficients

(eqn <- paste("log(1+roi) =", paste(round(cc[1],2), paste(round(cc[-1],2), names(cc[-1]), sep=" x ", collapse=" + "), sep=" + "), "+ e"))

