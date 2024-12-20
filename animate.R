library(animation)
library(colorspace)
library(farver)

# Modified simulation function to track states
simulate_color_repulsion_with_states <- function(n_colors,
                                                 max_iterations = 100,
                                                 learning_rate = 50,
                                                 base_colors = NULL,
                                                 save_every = 5,
                                                 boundary_force = 0.3) { # New parameter for boundary force
  
  L_range <- if(!is.null(base_colors)) {
    rgb_base <- t(sapply(base_colors, function(c) col2rgb(c)))
    lab_base <- convert_colour(rgb_base, from="rgb", to="lab")
    c(min(lab_base[,1]), max(lab_base[,1]))
  } else {
    c(40, 80)
  }
  
  points <- matrix(nrow = n_colors, ncol = 3)
  n_base <- if(!is.null(base_colors)) length(base_colors) else 0
  n_random <- n_colors - n_base
  
  # Set base colors
  if(n_base > 0) {
    points[1:n_base,] <- convert_colour(rgb_base, from="rgb", to="lab")
  }
  
  # Initialize remaining points for uniform perceptual distribution
  if(n_random > 0) {
    random_indices <- (n_base + 1):n_colors
    # Use wider L* range since we're not prioritizing lightness hierarchy
    L_min <- 30  # Still avoid extremely dark colors
    L_max <- 90  # Still avoid extremely light colors
    
    # Initialize with more saturated colors
    points[random_indices,1] <- runif(n_random, L_min, L_max)
    
    # Initialize a* and b* with minimum saturation
    min_saturation <- 30
    for(i in 1:n_random) {
      # Generate random angle in radians
      theta <- runif(1, 0, 2*pi)
      # Random radius between min_saturation and 60
      r <- runif(1, min_saturation, 60)
      # Convert to a* b* coordinates
      points[random_indices[i],2] <- r * cos(theta)
      points[random_indices[i],3] <- r * sin(theta)
    }
  }
  
  # List to store states
  states <- list()
  states[[1]] <- points
  
  # Define boundary coordinates
  boundaries <- list(
    L = L_range,
    a = c(-80, 80),
    b = c(-80, 80)
  )
  
  for(iter in 1:max_iterations) {
    forces <- matrix(0, nrow = n_colors, ncol = 3)
    
    if(n_random > 0) {
      for(i in (n_base + 1):n_colors) {
        # Color-to-color repulsion
        for(j in 1:n_colors) {
          if(i != j) {
            rgb_i <- convert_colour(matrix(points[i,], ncol=3), from="lab", to="rgb")
            rgb_j <- convert_colour(matrix(points[j,], ncol=3), from="lab", to="rgb")
            
            cie_dist <- compare_colour(rgb_i, rgb_j, "rgb", "lab", method="cie2000")
            
            diff <- points[i,] - points[j,]
            # Equal weights for uniform perceptual distances
            weights <- c(1, 1, 1)  # Treat all LAB dimensions equally
            weighted_diff <- diff * weights
            dist <- sqrt(sum(weighted_diff^2))
            # Create vector of same length as diff for the magnitude
            magnitude <- rep(1 / (cie_dist^2), length(diff))
            # Element-wise multiplication using c()
            force_vector <- c(weighted_diff/dist) * magnitude
            forces[i,] <- forces[i,] + force_vector
          }
        }
        
        # Add boundary repulsion forces
        for(dim in 1:3) {
          boundary_min <- if(dim == 1) boundaries$L[1] else -80
          boundary_max <- if(dim == 1) boundaries$L[2] else 80
          
          # Distance to boundaries
          dist_to_min <- points[i,dim] - boundary_min
          dist_to_max <- boundary_max - points[i,dim]
          
          # Exponential repulsion from boundaries
          min_force <- boundary_force * exp(-dist_to_min/20)  # /20 controls force falloff
          max_force <- boundary_force * exp(-dist_to_max/20)
          
          forces[i,dim] <- forces[i,dim] + min_force - max_force
        }
        
        # Add repulsion from gray center (a=0, b=0)
        # Calculate distance from center in a-b plane
        dist_from_center <- sqrt(points[i,2]^2 + points[i,3]^2)
        min_desired_saturation <- 30  # Minimum desired distance from gray
        if(dist_from_center < min_desired_saturation) {
          # Create outward force in a-b plane
          saturation_force <- 0.5 * (1 - dist_from_center/min_desired_saturation)
          # Apply force in a and b dimensions proportionally to current position
          forces[i,2] <- forces[i,2] + saturation_force * points[i,2]/max(dist_from_center, 0.1)
          forces[i,3] <- forces[i,3] + saturation_force * points[i,3]/max(dist_from_center, 0.1)
        }
      }
    }
    
    # Calculate adaptive learning rate based on multiple factors
    
    # 1. Calculate average force magnitude
    avg_force_magnitude <- mean(sqrt(rowSums(forces^2)))
    
    # 2. Calculate average color distance
    avg_color_dist <- 0
    if(n_random > 0) {
      distances <- numeric()
      for(i in (n_base + 1):n_colors) {
        for(j in 1:n_colors) {
          if(i != j) {
            rgb_i <- convert_colour(matrix(points[i,], ncol=3), from="lab", to="rgb")
            rgb_j <- convert_colour(matrix(points[j,], ncol=3), from="lab", to="rgb")
            distances <- c(distances, compare_colour(rgb_i, rgb_j, "rgb", "lab", method="cie2000"))
          }
        }
      }
      avg_color_dist <- mean(distances)
    }
    
    # More gradual learning rate adaptation
    force_factor <- 1 / (1 + 0.1 * avg_force_magnitude)  # Less sensitive to force magnitude
    
    # Modified distance factor that only starts reducing when colors are very well spread
    distance_threshold <- 30  # Only start slowing down when average distance is good
    distance_factor <- if(avg_color_dist > distance_threshold) {
      1 / (1 + 0.05 * (avg_color_dist - distance_threshold))
    } else {
      1  # Keep full learning rate when colors aren't well spread
    }
    
    # Much gentler progress decay
    progress_factor <- 1 - 0.5 * (iter/max_iterations)^0.15  # Very slow decay
    
    # Combine factors with a minimum learning rate to prevent complete stopping
    current_lr <- learning_rate * force_factor * distance_factor * progress_factor
    current_lr <- max(current_lr, learning_rate * 0.1)  # Never go below 10% of initial rate
    points <- points + forces * current_lr
    
    # Enforce boundaries (but less strictly)
    if(n_random > 0) {
      points[(n_base + 1):n_colors,1] <- pmax(L_range[1], pmin(L_range[2], points[(n_base + 1):n_colors,1]))
      points[(n_base + 1):n_colors,2] <- pmax(-80, pmin(80, points[(n_base + 1):n_colors,2]))
      points[(n_base + 1):n_colors,3] <- pmax(-80, pmin(80, points[(n_base + 1):n_colors,3]))
    }
    
    if(iter %% save_every == 0) {
      states[[length(states) + 1]] <- points
    }
  }
  
  if(max_iterations %% save_every != 0) {
    states[[length(states) + 1]] <- points
  }
  
  return(states)
}

# Function to create animation
animate_repulsion <- function(n_colors, 
                              base_colors = NULL, 
                              max_iterations = 100,
                              learning_rate = 50,
                              save_every = 5,
                              boundary_force = 0.3,
                              filename = "repulsion.gif") {
  # Get states
  states <- simulate_color_repulsion_with_states(n_colors, 
                                                 max_iterations = max_iterations,
                                                 learning_rate = learning_rate,
                                                 base_colors = base_colors,
                                                 save_every = save_every,
                                                 boundary_force = boundary_force,
                                                 return_states = TRUE)
  
  # Create GIF using animation package
  saveGIF({
    for(i in seq_along(states)) {
      points <- states[[i]]
      
      # Convert current LAB coordinates to RGB for visualization
      # Ensure values are within valid RGB range
      rgb_colors <- convert_colour(points, from="lab", to="rgb")/255
      hex_colors <- rgb(rgb_colors[,1], rgb_colors[,2], rgb_colors[,3])
      
      # Create plot
      par(mar = c(4, 4, 2, 1))
      plot(points[,2], points[,3], 
           col = hex_colors, pch = 16, cex = 2,
           xlim = c(-80, 80), ylim = c(-80, 80),
           xlab = "a", ylab = "b", 
           main = sprintf("Iteration %d", (i-1) * save_every))
      
      # Add color information
      if(!is.null(base_colors)) {
        points(points[1:length(base_colors),2], 
               points[1:length(base_colors),3],
               col = "black", pch = 1, cex = 2.5, lwd = 2)
      }
      
      # Add current learning rate and average color distance to plot
      if(i > 1) {
        # Calculate average color distance for information
        distances <- numeric()
        for(k in 1:(nrow(points)-1)) {
          for(j in (k+1):nrow(points)) {
            rgb_k <- convert_colour(matrix(points[k,], ncol=3), from="lab", to="rgb")
            rgb_j <- convert_colour(matrix(points[j,], ncol=3), from="lab", to="rgb")
            distances <- c(distances, compare_colour(rgb_k, rgb_j, "rgb", "lab", method="cie2000"))
          }
        }
        avg_dist <- mean(distances)
        mtext(sprintf("Avg Color Distance: %.1f", avg_dist), 
              side=3, line=0, cex=0.8)
      }
      
      # Add grid
      grid()
    }
  }, movie.name = filename, 
  ani.width = 600, 
  ani.height = 600, 
  interval = 0.1)
}

# Usage example
set.seed(123)
animate_repulsion(8, 
                  base_colors = c("#F9A904", "#00559D"),
                  max_iterations = 400,
                  learning_rate = 500,
                  save_every = 5,
                  boundary_force = 0.01)


rgb_colors1 <- convert_colour(states[[1]], from="lab", to="rgb")/255
hex_colors1 <- rgb(rgb_colors1[,1], rgb_colors1[,2], rgb_colors1[,3])
rgb_colors2 <- convert_colour(states[[81]], from="lab", to="rgb")/255
hex_colors2 <- rgb(rgb_colors2[,1], rgb_colors2[,2], rgb_colors2[,3])
compare_palettes(list(hex_colors1, hex_colors2), labels = c("Before", "After"), TRUE)
