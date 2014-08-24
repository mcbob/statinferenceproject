# Now in the second portion of the class, we're going to analyze the ToothGrowth
# data in the R datasets package. 
# 1. Load the ToothGrowth data and perform some basic exploratory data analyses 
# 2. Provide a basic summary of the data.
# 3. Use confidence intervals and hypothesis tests to compare tooth growth by 
# supp and dose. (Use the techniques from class even if there's other approaches
# worth considering) 
# 4. State your conclusions and the assumptions needed for your
# conclusions.

data(ToothGrowth)

coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")
library(ggplot2)
qplot(len, data = ToothGrowth, facet = supp ~ dose)
g <- ggplot(ToothGrowth, aes(dose, len))
g + geom_point() + facet_grid(. ~ supp) + geom_smooth(method = "lm")

# Some descriptive statistics
summary(means)
mean(ToothGrowth$len)
sd(ToothGrowth$len)
hist(ToothGrowth$len, breaks = 15)

# quick linear model fit to see significancies
lmlen <- lm(len ~ supp + dose, ToothGrowth)
summary(lmlen)


# Hypothesis test
data <- subset(ToothGrowth, dose != 1)
t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data)

data <- subset(ToothGrowth, dose != 2)
t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data)

data <- subset(ToothGrowth, dose != 0.5)
t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data)

t.test(len ~ supp, paired = FALSE, var.equal = FALSE, ToothGrowth)
t.test(len ~ supp, paired = FALSE, var.equal = FALSE, ToothGrowth, 
       alternative = "greater")

data <- subset(ToothGrowth, dose == 0.5)
t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data)

data <- subset(ToothGrowth, dose == 1)
t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data)

data <- subset(ToothGrowth, dose == 2)
t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data)