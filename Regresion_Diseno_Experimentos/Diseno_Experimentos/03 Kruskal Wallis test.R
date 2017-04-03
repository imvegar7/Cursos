# Problem

# If you have to perform the comparison between multiple groups, 
# but you can not run a ANOVA because the groups do not follow a 
# normal distribution, you can use the Kruskal-Wallis test, 
# which can be applied when you can not make 
# the assumption that the groups follow a gaussian distribution.

# Example
y <- c(1, 5, 8, 17, 16,
       2, 16, 5, 7, 4,
       1, 1, 3, 7, 9,
       2, 15, 2, 9, 7)
treatment <- as.factor(rep(letters[1:4], each=5))

# Using anova
mod <- aov(y ~ treatment)


# Residuals ---------------------------------------------------------------
eij <- residuals(mod)
# Bartlett test
bartlett.test(x=eij, g=treatment)
# Shapiro Wilk test
shapiro.test(eij)


# Now we can apply the kruskal.test() function:
kruskal.test(x=y, g=treatment)
