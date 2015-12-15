# package setup
#install.packages("ellipse")
library(ellipse)


# helper functions for plotting
bvn_countour_plot <- function(ux = 0, 
                              uy = 0, 
                              sx = 1, 
                              sy = 1, 
                              rho = 0.5, 
                              xlab = "xlab", 
                              ylab = "ylab"){
  
  mu <- c(ux, uy)
  s12 <- rho * sx * sy
  sigma  <- cbind(c(sx^2,s12),c(s12,sy^2)) 
  
  xmax <- round((max(ellipse(sigma, level = 0.999)[,1] + mu[1]))) + 1
  xmin <- round((min(ellipse(sigma, level = 0.999)[,1] + mu[1]))) - 1
  
  ymax <- round((max(ellipse(sigma, level = 0.999)[,2] + mu[2]))) + 1
  ymin <- round((min(ellipse(sigma, level = 0.999)[,2] + mu[2]))) - 1
  
  #if(xmin < 250 & xmin > 0){
  #  xmin <- 0
  #}
  
  #if(ymin < 250 & ymin > 0){
  #  ymin <- 0
  #}
  
  plot(
    ellipse(sigma, level = 0.90)[ ,1] + mu[1],
    ellipse(sigma, level = 0.90)[ ,2] + mu[2],
    xlab = xlab, ylab = ylab,
    xlim = c(xmin, xmax), ylim = c(ymin, ymax), type="l")
  
  points(mu[1], mu[2], pch=3)
  
  lines(
    ellipse(sigma, level = 0.999)[ ,1] + mu[1],
    ellipse(sigma, level = 0.999)[ ,2] + mu[2],
    xlim = c(xmin, xmax), ylim = c(ymin, ymax), type="l")
  
  lines(
    ellipse(sigma, level = 0.99)[ ,1] + mu[1],
    ellipse(sigma, level = 0.99)[ ,2] + mu[2],
    xlim = c(xmin, xmax), ylim = c(ymin, ymax), type="l")
  
  lines(
    ellipse(sigma, level = 0.50)[ ,1] + mu[1],
    ellipse(sigma, level = 0.50)[ ,2] + mu[2],
    xlim = c(xmin, xmax), ylim = c(ymin, ymax), type="l")
  
  points(mu[1], mu[2], pch=3)
  
}

add_conditional_mean <- function(ux = 0, uy = 0, sx = 1, sy = 1, rho = 0.5, given = "x"){
  
  if(given == "x"){
    abline(
      uy - ux * rho * sy / sx,
      rho * sy / sx,
      col = "green"
    )
  }
  
  if(given == "y"){
    abline(
      uy - ux * sy / (rho * sx),
      sy / (rho * sx),
      col = "green"
    )
  }
  
}

#shade area
ux <- 370
sx <- 50
uy <- 270
sy <- 40
rho <- -0.80
xlab = "Guns"
ylab = "Butter"

a <- 400
bc <- 450
bp <- 250
d <- 700

abline(v = a, col = "blue")
for(i in seq(a, a * 5, by = 5)){
   abline(v = i, col = "blue", lty = 3)
}
