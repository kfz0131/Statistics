shinyServer(function(input, output) {
  # 3D Plot 
  output$distPlot <- renderPlot({
    library(mvtnorm)
    library(plot3D)
    # wrap-up function
    bivariate_normal_3d <- function(mu_x,mu_y,sx,sy,r,n,cutoff,
                                    vertical_rotation,horizontal_rotation){
      if(n=='Low'){
        n = 25
      }
      else{
        n = 100
      }
      # Use mvtnorm library
      library(mvtnorm)
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
        box = TRUE,
        contour = TRUE
      )
    }
    
    # Output
    bivariate_normal_3d(input$mu_x,input$mu_y,input$sx,input$sy,
                        input$r,input$resolution,3.5,input$vr,input$hr)
    
  },
    height = 1000, width = 800
  )
  #---------------------------------------------------------------------------
  # Condition Plot
  output$conditionPlot <- renderPlot({
    library(mvtnorm)
    library(plot3D)
    # wrap-up function
    
    condition_3d <- function(mu_x,mu_y,sx,sy,r,n,cutoff,
                             vertical_rotation,horizontal_rotation,cx,cy){
      if(n=='Low'){
        n = 25
      }
      else{
        n = 100
      }
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
    # Output
    condition_3d(input$mu_x,input$mu_y,input$sx,input$sy,input$r,input$resolution,3.5,input$vr,input$hr,cx=0.02)
  },
    height = 1000, width = 800
  )
  #---------------------------------------------------------------------------
  # Contour Plot
  output$contour <- renderPlot({
    library(plot3D)
    # wrap-up function
    contourplot <- function(mu_x,mu_y,sx,sy,r,n,cutoff,
                            vertical_rotation,horizontal_rotation){
      if(n=='Low'){
        n = 25
      }
      else{
        n = 100
      }
      # Use mvtnorm library
      library(mvtnorm)
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
      contour3D(x,
                y,
                bivariate_normal,
                colvar = bivariate_normal,
                phi = vertical_rotation,
                theta = horizontal_rotation)
    }
    
    # Output
    contourplot(input$mu_x,input$mu_y,input$sx,input$sy,
                        input$r,input$resolution,3.5,input$vr,input$hr)
    
  },
  height = 1000, width = 800
  )
})
