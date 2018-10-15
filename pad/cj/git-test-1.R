library(ggplot2)

data("mtcars")
qplot(mtcars$mpg, geom='histogram')
qplot(mtcars$hp, geom='histogram')


