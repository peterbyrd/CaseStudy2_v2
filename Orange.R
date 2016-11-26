## Author: Peter Byrd
## Data: November 25, 2016
## Case Study 2 - Question 3

### Calculate the mean and the median of the trunk circumferences for different sizes of trees
mean(subset(Orange, Tree == 1)$circumference)
median(subset(Orange, Tree ==1)$circumference)

mean(subset(Orange, Tree == 2)$circumference)
median(subset(Orange, Tree ==2)$circumference)

mean(subset(Orange, Tree == 3)$circumference)
median(subset(Orange, Tree ==3)$circumference)

mean(subset(Orange, Tree == 4)$circumference)
median(subset(Orange, Tree ==4)$circumference)

mean(subset(Orange, Tree == 5)$circumference)
median(subset(Orange, Tree ==5)$circumference)

# Create a scatter plot of the data
require(ggplot2)
require(reshape2)
ggplot(Orange, aes(x=age, y=circumference, color=Tree)) + geom_point(shape=1) + 
  ggtitle("Tree Circumference by Age") + theme(plot.title = element_text(hjust = 0.5))

# Create a boxplot of circumference by Tree
ggplot(Orange, aes(x=Tree, y=circumference)) + geom_boxplot(aes(fill=Tree)) +
  ggtitle("Circumference by Tree Type") + theme(plot.title = element_text(hjust = 0.5))

