#this_is_for_seminar_1
x <- 3 * 4
x
thisIsAReallyLongName <- 2.5
jennyRocks <- 2^3
jennyRocks
seq(1,10)
yo <- "hello world"
y <- seq(1, 10)
y
(y <- seq(1, 10))
date()
objects()
ls()
rm(y)
ls()
rm(list = ls())
#supposed to type q() here!
getwd()
#supposed to start a new Rstudio project, but doing this means exiting this directory. So I won't yet.
a <- 2
b <- -3
sigSq <- 0.5
x <- runif(40)
y <- a + b * x + rnorm(40, sd = sqrt(sigSq))
(avgX <- mean(x))
write(avgX, "avgX.txt")
plot(x, y)
abline(a, b, col = "purple")
dev.print(pdf, "toylinePlot.pdf")
#problem: I didn't get a '## pdf' reponse as expected...
#after quitting and restarting R...
n <- 40
a <- 2
b <- -3
sigSq <- 0.5
x <- runif(n)
y <- a + b * x + rnorm(n, sd = sqrt(sigSq))
(avgX <- mean(x))
write(avgX, "avgX.txt")
plot(x, y)
abline(a, b, col = "purple")
dev.print(pdf, "toylinePlot.pdf")
source(~/.active-rstudio-document)
#looks like sourcing does do something - makes a graph.
#however, it doesn't return any numbers.