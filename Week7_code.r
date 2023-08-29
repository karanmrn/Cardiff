library(lawstat)
library(EnvStats)
library(TeachingDemos)
library(Deducer)

abalone<-read.table("abalone.data", sep=",")

# Test if there is evidence that the diameter of the shells of abalones is different for the three different sexes
shapiro.test(abalone[abalone[,1]=="M", 3])
shapiro.test(abalone[abalone[,1]=="F", 3])
shapiro.test(abalone[abalone[,1]=="I", 3])
# Reject for all of them 

# We can test symmetry test
symmetry.test(abalone[abalone[,1]=="M", 3])
symmetry.test(abalone[abalone[,1]=="F", 3])
symmetry.test(abalone[abalone[,1]=="I", 3])

# test equality of variance
leveneTest(abalone[,3], abalone[,1])

# theoretically from the moment we rejected normality we can use the Kruskal Wallis test, 
# But since ANOVA is considered robust to violation of one of the two assumptions (normality and equality of variances) 
# I tested both assumptions to see if there is a chance to be allowed to use ANOVA test
kruskal.test(abalone[,3]~abalone[,1])  # reject the null hypothesis 



# if someone told you we can assume normality then you can use the 
oneway.test(abalone[,3]~abalone[,1])
# which assumes normality but not equality of variances

# if someone told you we can assume normality and equality of variances 
aov(abalone[,3]~abalone[,1])
# or even better
anova(lm(abalone[,3]~abalone[,1]))


# test weather the diameter is different for the three different sexes and for abalones that have small or large length
# Here I want to create a second categorical variable and therefore I split the abalones to those with length >0.5 (large abalones) and
# those with length < 0.5 (small abalones) 
# create the new categorical variable 
lengthbin<-abalone[,2]>0.5


anova(lm(abalone[,3]~abalone[,1]+lengthbin))




# Multiple comparisons
pairwise.t.test(abalone[,3],abalone[,1])
# but to get bonferroni
pairwise.t.test(abalone[,3],abalone[,1], p.adjust.method="bonferroni")
# Unpooled case
pairwise.t.test(abalone[,3],abalone[,1], p.adjust.method="bonferroni", pool.sd=FALSE)
# Let's see the Tukey HSD
TukeyHSD(aov(abalone[,3]~abalone[,1]))






# test if the proportion of male, female and infant abalones with diameter greater than 0.25 are equal. 
table(abalone[,1])
table(abalone[abalone[,3]>0.25,1])
tot <- c(table(abalone[,1])[[1]], table(abalone[,1])[[2]], table(abalone[,1])[[3]])
su <- c(table(abalone[abalone[,3]>0.25,1])[[1]], table(abalone[abalone[,3]>0.25,1])[[2]], table(abalone[abalone[,3]>0.25,1])[[3]])
prop.test(su, tot)

# the three proportions are equal to 0.8
prop.test(su, tot, p=c(0.8, 0.8, 0.8))

# the three proportions are equal to a different proportion each. 
prop.test(su, tot, p=c(0.99, 0.8, 0.96))


# is there evidence that abalone and binary length are independent 
chisq.test(abalone[,1],lengthbin)


# Goodness of fit test 
# Let's test if the whole weight follows Normal distribution
# Let's use 4 bins 
wholeweight<-abalone[,5]
probs4 <- rep(0.25, 4)
quan <- qnorm(c(0.25, 0.5, 0.75), mean(wholeweight), sd(wholeweight))
obs <- c(length(wholeweight[wholeweight<quan[1]]), length(wholeweight[wholeweight<quan[2]]) -
            length(wholeweight[wholeweight<quan[1]]), length(wholeweight[wholeweight<quan[3]]) -
            length(wholeweight[wholeweight<quan[2]]), length(wholeweight) -
            length(wholeweight[wholeweight<quan[3]]))
chisq.test(obs, p=probs4)
pchisq(47.648, 1, lower.tail=FALSE)



wholeweight<-abalone[,5]
probs5 <- rep(0.2, 5)
quan <- qnorm(c(0.2, 0.4, 0.6, 0.8), mean(wholeweight), sd(wholeweight))
obs <- c(length(wholeweight[wholeweight<quan[1]]), length(wholeweight[wholeweight<quan[2]]) -
            length(wholeweight[wholeweight<quan[1]]), length(wholeweight[wholeweight<quan[3]]) -
            length(wholeweight[wholeweight<quan[2]]), length(wholeweight[wholeweight<quan[4]]) -
            length(wholeweight[wholeweight<quan[3]]), length(wholeweight) -
            length(wholeweight[wholeweight<quan[4]]))
chisq.test(obs, p=probs5)
pchisq(40.088, 2, lower.tail=FALSE)




wholeweight<-abalone[,5]
probs4 <- rep(0.25, 4)
quan <- qchisq(c(0.25, 0.5, 0.75), mean(wholeweight)/2)
obs <- c(length(wholeweight[wholeweight<quan[1]]), length(wholeweight[wholeweight<quan[2]]) -
            length(wholeweight[wholeweight<quan[1]]), length(wholeweight[wholeweight<quan[3]]) -
            length(wholeweight[wholeweight<quan[2]]), length(wholeweight) -
            length(wholeweight[wholeweight<quan[3]]))
chisq.test(obs, p=probs4)
pchisq(6870.5, 2, lower.tail=FALSE)


