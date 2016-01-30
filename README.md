# notes

# HF

flights
str(flights)
flights[,.N,]  # nof rows
flights[dest == 'LAX'& origin == 'JFK', .N, ]
library(data.table)
flights[dest == 'LAX'& origin == 'JFK', mean(arr_delay, na.rm=TRUE)]
?mean
?table
#mean <- function(x, ...) base::marn(x, na.rm) 
flights[origin == 'JFK', mean(arr_delay, na.rm=TRUE), by = dest] # mean with group by 

?which.min # lowest element

flightsa <- flights[origin == 'JFK', mean(arr_delay, na.rm=TRUE), by = dest][order(V1)] # mean with group by and ordering
flightsa <- flights[origin == 'JFK', list(avg_del = mean(arr_delay, na.rm=TRUE)), by = dest] # naming field 
flightsa <- flights[origin == 'JFK', .(avg_del = mean(arr_delay, na.rm=TRUE)), by = dest] # naming field  list='.'
flightsa

fligtsa = NULL
setorder(fligtsa, -V1)
flightsa[1]
head(flightsa, 1) #first element
str(flightsa)
library(ggplot2)
ggplot(flightsa, aes(x= dest, y = avg_del)) + geom_bar() #error - countolni akarja, de itt nem tudja
?geom_bar
ggplot(flightsa, aes(x= dest, y = avg_del)) + geom_bar(stat="identity") #char shows ABC order


flightsa <- flights[origin == 'JFK', .(avg_del = mean(arr_delay, na.rm=TRUE)), by = dest][order(avg_del)]
                                                                                          # naming field  list='.'

setorder(fligtsa, avg_del)
flightsa[,dest := factor(dest, levels = flightsa$dest)]
str(as.numeric(flightsa))

str(as.numeric(flightsa$dest))

ggplot(flightsa, aes(x= factor(dest), y = avg_del)) + geom_bar(stat="identity") #char shows ABC order

# lehet transzponalni coord flippel es sorba rendezni a szoveges mezoket numerikus jelleggel
ggplot(flightsa, aes(x = dest, y = avg_del)) + geom_boxplot()
ggplot(flights, aes(x = dest, y = arr_delay)) + geom_boxplot()
# data.table- peldakat megcsinalni!!!!!

ggplot(flightsa, aes(x = dest, y = avg_del)) + geom_bar(stat="identity")

setorder(flightsa, avg_del)
flightsa
flightsa$dest
flightsa[, dest := factor(dest, levels = flightsa$dest)]
str(flightsa$dest)

ggplot(flightsa, aes(x = dest, y = avg_del)) + geom_bar(stat="identity")

ggplot(flightsa, aes(x = dest, y = avg_del)) + geom_bar(stat="identity") + coord_flip() + ggtitle("Average akármi") + xlab("foobar")


################################

http://psycho.unideb.hu/statisztika/pages/interaktiv.html


str(heightweight)
df <- heightweight

fit <- lm(weightLb ~ heightIn, data = heightweight )
fit
predict(fit)
plot(df$heightIn, df$weightLb)

abline(fit, col= 'red') # new layer to the same plot
points(df$heightIn, predict(fit)) # add new points  to the prev plot() - will use only the only vector from fit
points(df$heightIn, predict(fit),col = 'blue', pch=19)

segments(df$heightIn, df$weightLb, df$heightIn, predict(fit), col = 'green') # connectin points, with vectors addig segments in a loop


predict(fit, newdata = data.frame(heightIn = 104/2.5)) * 0.45

plot(df$heightIn, df$weightLb, xlim =c(0,100)) # limits for x
abline(fit, col= 'red') # new layer to the same plot
plot(df$heightIn, df$weightLb, xlim =c(0,100), ylim = c(0,300)) # limits for x
# working ok only narrow intervall

fit <- lm(weightLb ~ heightIn + heightIn^2, data = heightweight )
fit <- lm(weightLb ~ poly(heightIn,2), data = heightweight )
fit <- lm(weightLb ~ poly(heightIn,2, raw = TRUE), data = heightweight )
fit
?poly

abline(fit, col= 'red')
abline(fit, col= 'blue')
points(df$heightIn, predict(fit), col = 'blue', pch = 19)
plot(df$heightIn, df$weightLb) #, xlim =c(0,100), ylim = c(0,300)) # limits for x

points(-1000:1000, prefict(fit, newdata = data.fram(heightIn = -1000:1000), col = 'blue', pch = 19)
str(df)
       
ggplot(df, aes(x=heightIn, y=weightLb))+ geom_point()+geom_smooth(method='lm', se = TRUE)
ggplot(df, aes(x=heightIn, y=weightLb))+ geom_point()+geom_smooth(method='lm', se = TRUE, formula=y ~ poly(x,5)) # formula to method

str(shoes)
plot(shoes)

df <- shoes

ggplot(shoes, aes(x=size, y=math))+ geom_point()+geom_smooth(method='lm', se = TRUE)

cor(df) # correlation coeff

# strange correlations
# https://www.google.com/trends/correlate

lm(formula= math ~ x, data = df)
lm(size ~ x, df)
residuals(lm(size ~ x, df))
residuals(lm(math ~ x, df))
cor(residuals(lm(size ~ x, df)), residuals(lm(math ~ x, df))) #partial corr [-1,1]
install.packages("psych")
library(psych)

partial.r(df, 2:3, 4)

str(iris)
plot(iris)
summary(fit)

fit <- lm(Sepal.Width ~ Sepal.Length, iris)
plot(fit)

plot(iris$Sepal.Length, iris$Sepal.Width)
abline(fit, col = 'red')
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species)


fit <- lm(Sepal.Width ~ Sepal.Length + Species, iris) # create species flags as it is a factor!
fit

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color = Species)) + geom_smooth(method = 'lm', se = TRUE) + geom_point()
# clustering

?hclust
str(iris)
str(dist(iris)) #dist each obs.
str(dist(iris[,1:4]))

dm<-dist(iris[,1:4])
str(dm)
hclust(dm)  # slow for large datasets
plot(hclust(dm))
hc <- hclust(dm)

str(hc)

#cutting clusters
rect.hclust(hc, k=3, border = 'red')
cn <- cutree(hc, k = 3) # cluster number at given k cluster cutting

table(cn, iris$Species) # counfusion matrix with fact


?dist
#before distancing, standardize to 0
?scale
scale(iris$Petal.Width) 

km <- kmeans(iris[,1:4], 3)
str(km$cluster)
table(km$cluster, iris$Species) # counfusion matrix with fact

plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species)
sample()

ir <- data.table(datasets::iris)
set.seed(42)

i <- sample(1:150, 100)
ir[i, .N, by = Species]
ir[-i, .N, by = Species]

ir[, rnd := runif(150)]
setorder(ir, rnd)
ir[1:100]
train <- ir[i]
test <- ir[-i]
str(train)
library(class)
?knn
train[,1:4] # DT syntax missleading...
train[, 1:4, with = FALSE]
train[,.N]
kn <- knn(train[,1:4, with = FALSE], test[,1:4, with = FALSE], cl = train$Species) # we should not include Speacies! 1:4 not enough...

plot(kn)

str(kn)
table(kn, test$Species)

kn <- knn(train[,1:4, with = FALSE], test[,1:4, with = FALSE], cl = train$Species, k = 5) # nof neighbors def=1

library(NbClust)
str(ir)

ir[,-5, with = FALSE]

?NbClust
NbClust(ir[,-5, with = FALSE], method = 'complete') # optimal nof clusters mathematically

library(rpart)
?rpart
ct <- rpart(Species ~ ., data = train) # '.' use all columns except written
plot(ct)
text(ct)
library(rpart.plot)
rpart.plot(ct)
plot(as.party(ct))

predict(ct) # probabilities
predict(ct, type = 'class') # only labels with highes prob.
table(predict(ct, type = 'class', nnewdata = test))


############ try to predict sex ##############################
df <- heightweight

i <- sample(1:237, 150)
str(df)
setDT(df)


set.seed(42)

train <- df[i]
test <- df[-i]


#or reorder
# df <- df[order(runif(nrow(df))),1]


NbClust(df[,-1, with = FALSE], method = 'complete') # optimal nof clusters mathematically

kn1 <- knn(train[,2:5, with = FALSE], test[,2:5, with = FALSE], cl = train$sex) # nof neighbors def=1

str(kn)
table(kn1, test$sex)

fit <- lm(sex ~ ., train)

table(fit)
predict(fit, test)

# km1 <- kmeans(train[,2:5, with = FALSE], 2)
# table(km1$cluster, test$sex)

str(km1)
str(train)

cor(df$heightIn, df$weightLb)

ct <- rpart(sex ~ heightIn + weightLb, data = df, minsplit= 1)

table(predict(ct, type = 'class'), df$sex) # overfitting
plot(ct); text(ct)

nrow(df)

ct <- rpart(sex ~ heightIn + weightLb, data = train, minsplit = 5) # how many obs.at levels

table(train$sex, predict(ct, type = 'class')) # overfitting
table(test$sex, predict(ct, type = 'class', newdata = test)) # overfitting


?rpart.control

plot(ct); text(ct)

setDF(ir)
str(ir)
prcomp(ir[,1:4]) # PCA comp for each column, SD>1 better, higher component more interactions
# first some can identify outliers if U set a treshold, higher comp. can show above a treshold the never possibel interactions
# we have 1 colomn that is most relative (PC1) dev is the highest
# rotation matrx
prcomp(ir[,1:4], scale = TRUE)  #scaling included
str(ir)

pc <- prcomp(ir[,1:4], scale = TRUE)
plot(pc)
plot(pc$x[,1:2], col = ir$Species) # bettet to use for GMB,or RF to make an artificial feature space

str(eurodist)
eurodist

prcomp(eurodist)

cmdscale(eurodist)
m<-cmdscale(eurodist)
plot(cmdscale(eurodist), type = 'n') # 
text(m[,1], -m[,2], labels(eurodist)) # concentrate the n*n matrix to n*2 much easier -> make relative coordinates


