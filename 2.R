n1 <- seq(2,10,1)
l <-length(n1)
n2 <- seq(3,11,1)
lenght(n2)
n <- 0
for (i in 1:l)
{
  n[i] <- n1[i]+n2[i]
}
print(n)



subset(airquality, Temp>80, select=c(Ozone, Temp))

subset(airquality, Temp>80, select=Ozone:Wind)
subset(airquality, Temp>80, select=-Temp)
subset(airquality, Day==1, select=-Temp)

head(state.x77)
mean(airquality$Temp)
attach(mtcars)
head(mtcars)

mean(mpg)
detach(mtcars)

getwd()




complete.cases(mtcars$vs)



complete.cases(airquality$Solar.R)

na.omit(airquality$Solar.R)

x <- c(1:10,20,30)
print(x)
boxplot(x)
boxplot.stats(x)

#-----------------------------------------------------------------------


people <- read.csv("people.txt")
head(people)
library(editrules)
E <- editset(c("age>=0","age<=150"))
print(E)
violatedEdits(E,people)

E2 <- editfile("rules.txt")
print(E2)
ve <- violatedEdits(E2,people)
print(ve)
describe(ve)
plot(ve)


write.csv(mtcars,"mtcars.csv")
read.csv(file.choose())

help(mtcars)

#----------------------------------------------------------------

library(stringr)
a <- "  hello world"
print(a)
str_trim(a)
str_trim(a,side="left")


str_pad(112, width=6, side="left", pad=0)

#------------------------------------------------------------------

gender <- c("M", "male", "female", ".Fem")
print(gender)

grepl("m", gender, ignore.case = TRUE)

grep("m", gender, ignore.case = TRUE)

grepl("^m", gender, ignore.case = TRUE)

#--------------------------------------------------------------------

paste(c("hello", "hey", "howdy"), c("jared","bob","David"))

paste ("hello",c("jared","bob","David"),c("goodbye","see you"))

#--------------------------------------------------------------------


person <- "jared"
partysize <- "eight"
waitime <- 25

sprintf("hello %s, your party of %s will be seated in %s minutes", person,partysize,waitime)
sprintf("hello %s, your party of %s will be seated in %s minutes", c("jared","bob"),c("eight",16,"four",10),waitime)

#---------------------------------------------------------------------

adist("abc", "bac")

library(stringdist)
stringdist("abc", "bac")

#----------------------------------------------------------------------


library(XML)
theurl <- "http://www.loc.gov//rr/print/list/057_chron.html"
readHTMLTable(theurl)
presidents <- readHTMLTable(theurl, which=3, as.data.frame = TRUE, skip.rows = 1, header = TRUE, stringAsFactors=FALSE)
head(presidents)
tail(presidents)
presidents <- presidents[1:65,]
head(presidents)
tail(presidents)
library(stringr)
yearlist <- str_split(presidents$YEAR,pattern = "-")
head(yearlist)
yearmatrix <- data.frame(Reduce(rbind,yearlist))
head(yearmatrix)
names(yearmatrix) <- c("Start","Stop")
head(yearmatrix)
presidents <- cbind(presidents,yearmatrix)
head(presidents)
class(presidents$Reduce.rbind..yearlist.)
as.numeric(presidents$Start)
presidents$Start <- as.numeric(as.character(presidents$Start))
head(presidents$Start)
presidents$Stop <- as.numeric(as.character(presidents$Stop))
head(presidents$Stop)
head(presidents)

#----------------------------------------------------------------

str_sub(presidents$PRESIDENT, start=1, end=3)
str_sub(presidents$PRESIDENT, start=4, end=8)
presidents[str_sub(presidents$Start, start=4, end=4)==1, c("YEAR","PRESIDENT","Start","Stop")]

#------------------------------------------------------------------

johnpos <- str_detect(presidents$PRESIDENT, pattern = "John")
print(johnpos)
presidents[johnpos,c("YEAR","PRESIDENT","Start","Stop")]

#-----------------------------------------------------------------
johnpos1 <- str_detect(presidents$PRESIDENT, coll("john", ignore_case = TRUE))
print(johnpos1)
presidents[johnpos1,c("YEAR","PRESIDENT","Start","Stop")]

#---------------------------------------------------------------------


library(ggplot2)
data("diamonds")
head(diamonds)
nrow(diamonds)
ggplot(data=diamonds)+geom_histogram(aes(x=carat))
ggplot(data=diamonds)+geom_density(aes(x=carat), fill="grey50")
ggplot(diamonds, aes(x=carat, y=price))+geom_point()
plot(diamonds$carat,diamonds$price, type='b')

#--------------------------------------------------------------------

plot(diamonds$carat, diamonds$price)
cor(diamonds$carat, diamonds$price)
#------------------------------------------------------------------

head(mtcars)
nrow(mtcars)
mtcars[order(mtcars$mpg),]
mtcars[order(mtcars$mpg, mtcars$wt),]

mtcars[order(mtcars$mpg, -mtcars$wt),]

