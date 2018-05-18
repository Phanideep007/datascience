usedcar=read.csv(file.choose())
scale(usedcar$Price)

usedcar=usedcar[-1]
head(usedcar)

boxprice=BoxCoxTrans(usedcar$Price)
priceboxcox=predict(boxprice,usedcar$Price)
head(priceboxcox)
hist(priceboxcox)
plot(density(priceboxcox))
plot(density(usedcar$Price))
skewness(priceboxcox)
skewness(usedcar$Price)
par(mfrow=c(1,2))
dev.off()
princomp()

