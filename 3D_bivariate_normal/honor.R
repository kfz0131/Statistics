# Bivariate normal distribution
library(mvtnorm)
library(plot3D)
# type in parameter
mu_x = 0
mu_y = 10
sx = 0.15
sy = 1
r = 0.9
n = 100
cutoff <- 3.5

# Create grid of interesting values.
x <- seq(mu_x - cutoff * sx, mu_x + cutoff * sx, length = n)
y <- seq(mu_y - cutoff * sy, mu_y + cutoff * sy, length = n)


# create the mean matrix (u) and covariance matrix (s)
u <- c(mu_x, mu_y)
covariance <- r * sx * sy
s <- matrix(c(sx ^ 2, covariance, covariance, sy ^ 2), 2)

# setup matrix to store densities
bivariate_normal <- matrix(0, n, n)

# calculates the densities
for (i in seq_along(x)) {
  for (j in seq_along(y)) {
    bivariate_normal[i, j] <- dmvnorm(c(x[i], y[j]), mean = u, sigma = s)
  }
}

# slide bar parameter
vertical_rotation = 0
horizontal_rotation = 0

# plots
persp3D(
  x,
  y,
  bivariate_normal,
  colvar = bivariate_normal,
  phi = vertical_rotation,
  theta = horizontal_rotation,
  ticktype = "detailed",
  expand = 0.5,
  shade = 0.2,
  xlab = "x",
  ylab = "y",
  zlab = "z = f(x, y)",
  contour = TRUE
)
#-------------------------------------------------------------------------------
# wrap-up function
bivariate_normal_3d <- function(mu_x,mu_y,sx,sy,r,n,cutoff,
                               vertical_rotation,horizontal_rotation){
  # Use mvtnorm library and plot3D library
  library(mvtnorm)
  library(plot3D)
  # Create grid of interesting values.
  x <- seq(mu_x - cutoff * sx, mu_x + cutoff * sx, length = n)
  y <- seq(mu_y - cutoff * sy, mu_y + cutoff * sy, length = n)
  
  
  # create the mean matrix (u) and covariance matrix (s)
  u <- c(mu_x, mu_y)
  covariance <- r * sx * sy
  s <- matrix(c(sx ^ 2, covariance, covariance, sy ^ 2), 2)
  
  # setup matrix to store densities
  bivariate_normal <- matrix(0, n, n)
  
  # calculates the densities
  for (i in seq_along(x)) {
    for (j in seq_along(y)) {
      bivariate_normal[i, j] <- dmvnorm(c(x[i], y[j]), mean = u, sigma = s)
    }
  }
  
  # plots
  persp3D(
    x,
    y,
    bivariate_normal,
    colvar = bivariate_normal,
    phi = vertical_rotation,
    theta = horizontal_rotation,
    ticktype = "detailed",
    expand = 0.5,
    shade = 0.2,
    xlab = "x",
    ylab = "y",
    zlab = "z = f(x, y)",
    contour = TRUE
  )
}

# local test
bivariate_normal_3d(0,10,0.15,1,0.6,100,3.5,0,0)

# Run the app with code on the webpage
runApp("honor", display.mode = "showcase")

#------------------------------------------------------------------------------
# Conditional 3D plot
# wrap-up function
condition_3d <- function(mu_x,mu_y,sx,sy,r,n,cutoff,
                                vertical_rotation,horizontal_rotation,cx,cy){
  # Use mvtnorm library and plot3D library
  library(mvtnorm)
  library(plot3D)
  # Create grid of interesting values.
  x <- seq(mu_x - cutoff * sx, mu_x + cutoff * sx, length = n)
  y <- seq(mu_y - cutoff * sy, mu_y + cutoff * sy, length = n)
  
  
  # create the mean matrix (u) and covariance matrix (s)
  u <- c(mu_x, mu_y)
  covariance <- r * sx * sy
  s <- matrix(c(sx ^ 2, covariance, covariance, sy ^ 2), 2)
  
  # setup matrix to store densities
  bivariate_normal <- matrix(0, n, n)
  
  # calculates the densities
  if(missing(cx) & missing(cy)){
    for (i in seq_along(x)) {
      for (j in seq_along(y)) {
        bivariate_normal[i, j] <- dmvnorm(c(x[i], y[j]), mean = u, sigma = s)
      }
    }
    # plots
    persp3D(
      x,
      y,
      bivariate_normal,
      colvar = bivariate_normal,
      phi = vertical_rotation,
      theta = horizontal_rotation,
      ticktype = "detailed",
      expand = 0.5,
      shade = 0.2,
      xlab = "x",
      ylab = "y",
      zlab = "z = f(x, y)"
    )
  }
  # condition on given X
  else if(missing(cy)){
    for (i in which.min(abs(x - cx)):length(x) ) {
      for (j in seq_along(y)) {
        bivariate_normal[i, j] <- dmvnorm(c(x[i], y[j]), mean = u, sigma = s)
      }
    }
    # plots
    persp3D(
      x,
      y,
      bivariate_normal,
      colvar = bivariate_normal,
      phi = vertical_rotation,
      theta = horizontal_rotation,
      ticktype = "detailed",
      expand = 0.5,
      shade = 0.2,
      xlab = "x",
      ylab = "y",
      zlab = "z = f(x, y)"
    )
  }
  # condition on given Y
  else if(missing(cx)){
    for (i in seq_along(x)) {
      for (j in which.min(abs(y - cy)):length(y)) {
        bivariate_normal[i, j] <- dmvnorm(c(x[i], y[j]), mean = u, sigma = s)
      }
    }
    # plots
    persp3D(
      x,
      y,
      bivariate_normal,
      colvar = bivariate_normal,
      phi = vertical_rotation,
      theta = horizontal_rotation,
      ticktype = "detailed",
      expand = 0.5,
      shade = 0.2,
      xlab = "x",
      ylab = "y",
      zlab = "z = f(x, y)"
    )
  }
  # For debug use. This should not happen
  else{
    for (i in which.min(abs(x - cx)):length(x)) {
      for (j in  which.min(abs(y - cy)):length(y)) {
        bivariate_normal[i, j] <- dmvnorm(c(x[i], y[j]), mean = u, sigma = s)
      }
    }
    # plots
    persp3D(
      x,
      y,
      bivariate_normal,
      colvar = bivariate_normal,
      phi = vertical_rotation,
      theta = horizontal_rotation,
      ticktype = "detailed",
      expand = 0.5,
      shade = 0.2,
      xlab = "x",
      ylab = "y",
      zlab = "z = f(x, y)"
    )
  }
}

# local test
condition_3d(0,10,0.15,1,0.6,100,3.5,0,0,cx=0.02)
#------------------------------------------------------------------------------------------------
# Work on list
# 1. Set the flat contour plot(does not need to change with the rotation) 
#    along with the 3D bivariate normal plot
# 2. Create a click button, when it is clicked, show the argument of condition on X and Y
# 3. Show one slice of joint probability given X or Y on 3D dimension
# 4. Show the 3D condition plot
# 5. Create a github account to share the 3D bivariate code
# 6. Set up own Shiny url: https://www.rstudio.com/pricing/
