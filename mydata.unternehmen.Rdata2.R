dataset2 <- iris

# (A) Create a scatterplot matrix for the complete dataset.
plot(iris)


# (B) Does the plant influence the width of the sepals? Create a boxplot
# and add a chart title
boxplot(dataset2$Sepal.Width ~ dataset2$Species, main="Sepal with by species", 
        ylab='Sepal Width', xlab='Species')

# (C) Create a histogram for the width of the sepals of all plant species,
hist(dataset2$Sepal.Width, main="Sepal Width of all Species", freq=FALSE,
     xlab='Sepal Width', ylab='Density of the Widths', las=1, breaks= 10)


# as well as a histogram for the width of the sepals of the respective plant
# species

# Setosa
setosa_sepal_width <- dataset2[dataset2$Species=='setosa', 'Sepal.Width']
hist(setosa_sepal_width, main="Sepal Width of all Setosas", freq=FALSE,
     xlab='Sepal Width', ylab='Density of the Widths', las=1, breaks= 10)
# versicolor
versicolor_sepal_width <- dataset2[dataset2$Species=='versicolor', 'Sepal.Width']
hist(versicolor_sepal_width, main="Sepal Width of all Versicolors", freq=FALSE,
     xlab='Sepal Width', ylab='Density of the Widths', las=1, breaks= 10)
# virginica
virginica_sepal_width <- dataset2[dataset2$Species=='virginica', 'Sepal.Width']
hist(versicolor_sepal_width, main="Sepal Width of all Virginicas", freq=FALSE,
     xlab='Sepal Width', ylab='Density of the Widths', las=1, breaks= 10)


# (D) Calculate the empirical key figures minimum, maximum, arithmetic
# mean and empirical variance for the width and length of the sepals
# and petals for all plant species.

# minimum, maximum, mean, variance of Sepal Lengths of all plant species
len_sepals <- dataset2[,'Sepal.Length']
min(len_sepals)
max(len_sepals)
mean(len_sepals)
var(len_sepals)

# minimum, maximum, mean, variance of Petal Lengths of all plant species
len_petals <- dataset2[,'Petal.Length']
min(len_petals)
max(len_petals)
mean(len_petals)
var(len_petals)

