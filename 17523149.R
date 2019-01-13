library(xlsx)

xlsx.file <- "tabel1.xlsx"
tabel1 <- read.xlsx(xlsx.file, sheetName = "Sheet1")
head(tabel1)
colnames(tabel1) <- c('xi', 'yi')
head(tabel1)
#Nomer 5
plot(yi,xi)
curver(f1, add=FALSE)

#Nomer 11 
library(pracma)
f <- function(x) x^2 - 6
trapzfun(f, 0, 1)

#Nomer 12
f <- function(x) x^3 + 4*x^2 - 10
trapzfun(f, 1, 2)

#Nomer 13
h <- 0.1
x <- seq(0.1, by =h)
f <- function(x) {
  return (x^2)
}
f0 <- f(x[1])
fi <- sapply(x[2:10],f)
fn <- f(x[length(x)])

trap <- function (f0, fi, fn, h){
 L <- h * (f0 + 2 * sum(fi)+ fn)/2
  return (L)
}
trap (f0 , fi , fn , h)

#Nomer 14
"Jawabannya A"

#Nomer 15
h <- 0.2
x <- seq(0.1, by =h)
f <- function(x) {
  return (x^2)
}
f0 <- f(x[1])
fi <- sapply(x[2:5],f)
fn <- f(x[length(x)])

trap <- function (f0, fi, fn, h){
  L <- h * (f0 + 2 * sum(fi)+ fn)/2
  return (L)
}
trap (f0 , fi , fn , h)