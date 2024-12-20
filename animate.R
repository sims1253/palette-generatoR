library(animation)
library(colorspace)
library(farver)

# Function to create animation
animate_repulsion <- function(n_colors, 
                              base_colors = NULL, 
                              max_iterations = 100,
                              learning_rate = 50,
                              save_every = 5,
                              boundary_force = 0.3,
                              filename = "repulsion.gif") {
  # Get states
  states <- simulate_color_repulsion(n_colors, 
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
