rm(list = ls())#Limpa a memoria
x <- 2
x+3

X <- 3
x <- seq(-5,5, length = 50)
plot(x^2, 
     type = "l", 
     col = "red", 
     main = "Meu Primeiro Gráfico", 
     xlab = "Idade", 
     ylab = "y")

y <- function(x){
  x^3
}

curve(y, -5,5)
