shinyServer(function(input, output) {
  # basic 3D Plot + Contour
  output$distPlot <- renderPlot({
    # Use mvtnorm and plot 3D libraries
    library(mvtnorm)
    library(plot3D)
    
    # helper function for 3D bivaraite normal distribution
    bivariate_normal_3d <- function(mu_x,mu_y,sx,sy,r,n,cutoff,
                                    vertical_rotation,horizontal_rotation) {
      # Convert resolution to sample size
      if (n == 'Low') {
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
          bivariate_normal[i, j] <-
            dmvnorm(c(x[i], y[j]), mean = u, sigma = s)
        }
      }
      
      # Put 3D plot and contour plot together
      par(mfrow = c(2,1))
      
      # Draw the 3D plot
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
    
    # helper function for contour plot
    bvn_countour_plot <- function(ux,
                                  uy,
                                  sx,
                                  sy,
                                  rho,
                                  cx,
                                  cy,
                                  condition,
                                  X,
                                  Y) {
      # Use ellipse library
      library(ellipse)
      
      # Parameters
      xlabel <- "x"
      ylabel <- "y"
      mu <- c(ux, uy)
      s12 <- rho * sx * sy
      sigma  <- cbind(c(sx ^ 2,s12),c(s12,sy ^ 2))
      
      xmax <- ux + 3.5 * sx
      xmin <- ux - 3.5 * sx
      
      ymax <- uy + 3.5 * sy
      ymin <- uy - 3.5 * sy
      
      # Basic contour plot
      plot(
        ellipse(sigma, level = 0.90)[,1] + mu[1],
        ellipse(sigma, level = 0.90)[,2] + mu[2],
        xlab = xlabel, ylab = ylabel,
        xlim = c(xmin, xmax), ylim = c(ymin, ymax), type = "l"
      )
      
      points(mu[1], mu[2], pch = 3)
      
      lines(
        ellipse(sigma, level = 0.999)[,1] + mu[1],
        ellipse(sigma, level = 0.999)[,2] + mu[2],
        xlim = c(xmin, xmax), ylim = c(ymin, ymax), type = "l"
      )
      
      lines(
        ellipse(sigma, level = 0.99)[,1] + mu[1],
        ellipse(sigma, level = 0.99)[,2] + mu[2],
        xlim = c(xmin, xmax), ylim = c(ymin, ymax), type = "l"
      )
      
      lines(
        ellipse(sigma, level = 0.50)[,1] + mu[1],
        ellipse(sigma, level = 0.50)[,2] + mu[2],
        xlim = c(xmin, xmax), ylim = c(ymin, ymax), type = "l"
      )
      
      
      # Add shaded area if given X or Y
      if (condition == TRUE) {
        if (X == FALSE & Y == TRUE) {
          abline(h = cy, col = "blue")
          for (i in seq(cy, abs(cy) * 5, by = 0.1)) {
            abline(h = i, col = "blue", lty = 3)
          }
        }
        
        if (Y == FALSE & X == TRUE) {
          abline(v = cx, col = "blue")
          for (i in seq(cx, abs(cx) * 5, by = 0.01)) {
            abline(v = i, col = "blue", lty = 3)
          }
        }
      }
      
      
    }
    
    # Output
    bivariate_normal_3d(
      input$mu_x,input$mu_y,input$sx,input$sy,
      input$r,input$resolution,3.5,input$vr,input$hr
    )
    
    bvn_countour_plot(
      input$mu_x,input$mu_y,input$sx,input$sy,
      input$r,input$cx,input$cy,input$condition,input$X,input$Y
    )
  },
  # Adjust output image size
  height = 1000, width = 800)
  #---------------------------------------------------------------------------
  # 3D cutoff bivariate normal distribution given X or Y
  output$cutoffPlot <- renderPlot({
    # Use mvtnorm and plot 3D libraries
    library(mvtnorm)
    library(plot3D)
    
    # helper function for 3D cutoff bivariate normal distribution
    
    cutoff_3d <- function(mu_x,mu_y,sx,sy,r,n,cutoff,
                          vertical_rotation,horizontal_rotation,cx,cy,condition,X,Y) {
      # Convert resolution to sample size
      if (n == 'Low') {
        n = 25
      }
      else{
        n = 100
      }
      
      # Create grid of interesting values.
      x <- seq(mu_x - cutoff * sx, mu_x + cutoff * sx, length = n)
      y <- seq(mu_y - cutoff * sy, mu_y + cutoff * sy, length = n)
      
      
      # create the mean matrix (u) and covariance matrix (s)
      u <- c(mu_x, mu_y)
      covariance <- r * sx * sy
      s <- matrix(c(sx ^ 2, covariance, covariance, sy ^ 2), 2)
      
      # setup matrix to store densities
      bivariate_normal <- matrix(0, n, n)
      
      # Given X
      if (condition == TRUE) {
        if (X == TRUE & Y == FALSE) {
          for (i in which.min(abs(x - cx)):length(x)) {
            for (j in seq_along(y)) {
              bivariate_normal[i, j] <-
                dmvnorm(c(x[i], y[j]), mean = u, sigma = s)
            }
          }
          # plot
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
        # Given Y
        if (Y == TRUE & X == FALSE) {
          for (i in seq_along(x)) {
            for (j in which.min(abs(y - cy)):length(y)) {
              bivariate_normal[i, j] <-
                dmvnorm(c(x[i], y[j]), mean = u, sigma = s)
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
    }
    # Output
    cutoff_3d(
      input$mu_x,input$mu_y,input$sx,input$sy,input$r,input$resolution,3.5,input$vr,input$hr,input$cx,input$cy,input$condition,input$X,input$Y
    )
  },
  
  # Adjust output image size
  height = 1000, width = 800)
  #---------------------------------------------------------------------------
  output$slicePlot <- renderPlot({
    slice_3d <- function(mu_x,mu_y,sx,sy,r,n,cutoff,
                         vertical_rotation,horizontal_rotation,
                         cx,cy,condition,X,Y) {
      # Convert resolution to sample size
      if (n == 'Low') {
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
      
      
      # Calculates the densities
      
      # Given X
      if (condition == TRUE) {
        if (Y == FALSE & X == TRUE) {
          i = which.min(abs(x - cx))
          for (j in seq_along(y)) {
            bivariate_normal[i, j] <-
              dmvnorm(c(x[i], y[j]), mean = u, sigma = s)
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
        # Given Y
        if (X == FALSE & Y == TRUE) {
          j = which.min(abs(y - cy))
          for (i in seq_along(x)) {
            bivariate_normal[i, j] <-
              dmvnorm(c(x[i], y[j]), mean = u, sigma = s)
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
    }
    # Output
    slice_3d(
      input$mu_x,input$mu_y,input$sx,input$sy,input$r,
      input$resolution,3.5,input$vr,input$hr,input$cx,
      input$cy,input$condition,input$X,input$Y
    )
  },
  # Adjust output image size
  height = 1000, width = 800)
})
