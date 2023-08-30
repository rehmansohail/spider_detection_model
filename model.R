train <- read.csv("spider_train.csv")


plot(train$size, train$spider, xlab = "size", ylab = "Probability of being Malignant",
     col = "red", pch = 1, cex = 2)

logit.size <- glm(spider ~ size, data = train, family = "binomial")

summary(logit.size)

av = seq(0,1,0.01)

size.data <- data.frame(av)
names(size.data) <- "size"

pv <- predict(logit.size, size.data, type = 'response')

lines(av, pv, col = "blue", lwd = 2)

#test 1

test <- read.csv("spider_test.csv")

p.test <- predict(logit.size, test, type = 'response')

c.test <- ifelse(p.test > 0.3, 1, 0)

tab.test <- table(Predicted = c.test, Actual = test$spider)

print(tab.test)

sn.test <- tab.test[2,2]/sum(tab.test[ ,2])

print(sn.test)

#test 2
c.test1 <- ifelse(p.test > 0.5, 1, 0)

tab.test1 <- table(Predicted = c.test1, Actual = test$spider)

print(tab.test1)

sn.test1 <- tab.test1[2,2]/sum(tab.test1[ ,2])

print(sn.test1)
#test 3
c.test2 <- ifelse(p.test > 0.7, 1, 0)

tab.test2 <- table(Predicted = c.test2, Actual = test$spider)

print(tab.test2)

sn.test2 <- tab.test2[2,2]/sum(tab.test2[ ,2])

print(sn.test2)

#bar plot

A <- c(sn.test,sn.test1,sn.test2)
B <- c(0.3,0.5,0.7)


barplot(A, names.arg = B, xlab ="Cutoff", 
        ylab ="Senstivity", col ="blue", 
        main ="Variation of senstivities with cutoff")


