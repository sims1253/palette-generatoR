library(colorspace)
library(farver)
library(future)
library(future.apply)
library(progressr)

# Set up parallel processing
plan(multisession, workers = 8)
handlers(global = TRUE)
handlers("progress")

# Calculate color harmony score
calc_harmony_score <- function(palette) {
  # Convert to LCh for better hue handling
  rgb_vals <- t(sapply(palette, function(color) {
    col2rgb(color)/255
  }))
  lch_vals <- convert_colour(rgb_vals, from="rgb", to="lch")
  
  # Extract hue values
  hues <- lch_vals[,3]
  
  # Calculate hue differences between adjacent colors
  hue_diffs <- diff(hues)
  # Adjust for circular nature of hue
  hue_diffs <- pmin(abs(hue_diffs), 360 - abs(hue_diffs))
  
  # Penalty for very large hue jumps
  large_jump_penalty <- sum(hue_diffs > 90) * 0.2
  
  # Encourage smoother hue transitions
  smoothness <- -sd(hue_diffs)
  
  # Encourage similar chroma levels for harmony
  chroma_coherence <- -sd(lch_vals[,2])
  
  return(smoothness + chroma_coherence - large_jump_penalty)
}

# Function to calculate Mach band effects between colors
calc_mach_bands <- function(color1, color2) {
  # Convert to LAB space for perceptual calculations
  rgb1 <- col2rgb(color1)/255
  rgb2 <- col2rgb(color2)/255
  lab1 <- convert_colour(matrix(rgb1, ncol=3), from="rgb", to="lab")
  lab2 <- convert_colour(matrix(rgb2, ncol=3), from="rgb", to="lab")
  
  # Calculate lightness gradient
  delta_L <- abs(lab1[1] - lab2[1])
  
  # Calculate chroma gradient
  C1 <- sqrt(sum(lab1[2:3]^2))
  C2 <- sqrt(sum(lab2[2:3]^2))
  delta_C <- abs(C1 - C2)
  
  # Return combined measure of potential Mach banding
  return(delta_L * 0.7 + delta_C * 0.3)
}

# Function to evaluate luminance monotonicity
calc_luminance_monotonicity <- function(palette) {
  # Convert to Lab space
  rgb_vals <- t(sapply(palette, function(color) {
    col2rgb(color)/255
  }))
  lab_vals <- convert_colour(rgb_vals, from="rgb", to="lab")
  
  # Calculate differences in lightness
  lightness_diffs <- diff(lab_vals[,1])
  
  # Penalize non-monotonic changes
  monotonicity_violations <- sum(lightness_diffs < 0)
  
  return(monotonicity_violations)
}

#' Enhanced perceptual distance calculation
#' @param color1,color2 Hex color codes
#' @return Numeric distance value
calc_perceptual_distance <- function(color1, color2) {
  rgb1 <- matrix(col2rgb(color1)/255, ncol=3)
  rgb2 <- matrix(col2rgb(color2)/255, ncol=3)
  
  # CIE2000 distance
  cie_dist <- compare_colour(rgb1, rgb2, "rgb", "lab", method = "cie2000")
  
  # Hue difference in LCh space
  lch1 <- convert_colour(rgb1, from="rgb", to="lch")
  lch2 <- convert_colour(rgb2, from="rgb", to="lch")
  #  hue_diff <- min((lch1[3] - lch2[3])%%360, (lch2[3] - lch1[3])%%360)/180
  hue_diff <- min(abs(lch1[3] - lch2[3]), 360 - abs(lch1[3] - lch2[3])) / 180
  
  # Lightness contrast
  lab1 <- convert_colour(rgb1, from="rgb", to="lab")
  lab2 <- convert_colour(rgb2, from="rgb", to="lab")
  light_contrast <- abs(lab1[1] - lab2[1])/100
  
  # Combined metric
  #0.8 * cie_dist + 0.12 * hue_diff + 0.08 * light_contrast
  1 * cie_dist + 0 * hue_diff + 0 * light_contrast
}

enhanced_perceptual_distance <- function(color1, color2) {
  # Base perceptual distance
  base_dist <- calc_perceptual_distance(color1, color2)
  
  # Mach band effects
  mach_effect <- calc_mach_bands(color1, color2)
  
  # Convert to JzAzBz color space (better for uniform perception)
  rgb1 <- col2rgb(color1)/255
  rgb2 <- col2rgb(color2)/255
  lab1 <- convert_colour(matrix(rgb1, ncol=3), from="rgb", to="lab")
  lab2 <- convert_colour(matrix(rgb2, ncol=3), from="rgb", to="lab")
  
  # Calculate perceptual uniformity measure
  L1 <- lab1[1]
  L2 <- lab2[1]
  C1 <- sqrt(sum(lab1[2:3]^2))
  C2 <- sqrt(sum(lab2[2:3]^2))
  
  # Penalize large jumps in either luminance or chroma
  uniformity_penalty <- abs(L1 - L2)/100 + abs(C1 - C2)/100
  
  # Combined metric
  return(base_dist - 0.2 * mach_effect - 0.1 * uniformity_penalty)
}

calc_palette_distances <- function(palette){
  distances = numeric()
  for(i in 1:(length(palette)-1)){
    for(j in (i+1):length(palette)){
      distances = c(distances, calc_perceptual_distance(palette[[i]], palette[[j]]))
    }
  }
  return(distances)
}

validate_cvd <- function(palette) {
  deutan_sim <- deutan(palette)
  protan_sim <- protan(palette)
  tritan_sim <- tritan(palette)
  
  min_distances <- c(
    min(calc_palette_distances(palette)),
    min(calc_palette_distances(deutan_sim)),
    min(calc_palette_distances(protan_sim)),
    min(calc_palette_distances(tritan_sim))
  )
  return(min(min_distances))
}

sort_palette <- function(palette, base_colors = NULL) {
  if (length(palette) <= 2) return(palette)
  
  if(!is.null(base_colors)){
    sorted_colors <- base_colors
    remaining_colors <- setdiff(palette, base_colors)
  } else {
    min_distances <- sapply(palette, function(color) {
      min(
        unlist(
          sapply(
            sapply(palette,
                   function(sc) calc_perceptual_distance(color, sc)
                   ),
            function(i) if(i > 0) i)
          )
        )
    })
    
    # Add color with maximum minimum distance
    best_color <- palette[which.max(min_distances)]
    sorted_colors <- c(best_color)
    remaining_colors <- setdiff(palette, best_color)
  }
  # Start with base colors

  
  while (length(remaining_colors) > 0) {
    # For each remaining color, get its minimum distance to current palette
    min_distances <- sapply(remaining_colors, function(color) {
      min(sapply(sorted_colors, function(sc) enhanced_perceptual_distance(color, sc)))
    })
    
    # Add color with maximum minimum distance
    best_color <- remaining_colors[which.max(min_distances)]
    sorted_colors <- c(sorted_colors, best_color)
    remaining_colors <- setdiff(remaining_colors, best_color)
  }
  
  return(sorted_colors)
}

#' Plot multiple color palettes for comparison
#' @param palette_list List of palettes, each palette being a vector of hex colors
#' Plot multiple color palettes for comparison
#' @param palette_list List of palettes, each palette being a vector of hex colors
compare_palettes <- function(palette_list,
                             labels = NULL,
                             add_hex = FALSE,
                             base_colors = NULL,
                             sorted = FALSE) {
  # Input validation
  if (!is.list(palette_list)) {
    stop("palette_list must be a list of color vectors")
  }
  
  if(is.null(labels)) labels <- paste("Palette", seq_along(palette_list))
  
  # Calculate dimensions
  n_palettes <- length(palette_list)
  max_colors <- max(sapply(palette_list, length))
  
  # Set up the plotting area
  # Add extra space at top for title and bottom for labels
  old_par <- par(mar = c(2, 6, 3, 2))
  on.exit(par(old_par))
  
  # Create empty plot
  plot(0, 0, type = "n", 
       xlim = c(0, max_colors), 
       ylim = c(0, n_palettes + 0.4),
       xlab = "", ylab = "", 
       xaxt = "n", yaxt = "n")
  
  # Plot each palette
  for (i in seq_along(palette_list)) {
    palette <- palette_list[[i]]
    if(sorted) {
      palette = sort_palette(palette, base_colors = base_colors)
    }
    n_colors <- length(palette)
    
    # Plot color rectangles
    for (j in seq_along(palette)) {
      # Draw rectangle
      rect(j-1, n_palettes-i+0.4/2, j, n_palettes-i+1+0.4/2, 
           col = palette[[j]], border = "gray80")
      
      if(add_hex){
        # Add hex code as vertical text
        # Calculate text position
        x_pos <- j - 0.5
        y_pos <- n_palettes-i+0.7+0.4/2
        
        # Determine if color is dark (for text contrast)
        rgb_vals <- col2rgb(palette[[j]])/255
        luminance <- 0.2126 * rgb_vals[1] + 0.7152 * rgb_vals[2] + 0.0722 * rgb_vals[3]
        text_col <- if(luminance < 0.5) "white" else "black"
        
        # Add text
        text(x_pos, y_pos, 
             labels = toupper(palette[[j]]),
             srt = 90,  # Rotate text 90 degrees
             adj = c(0.5, 0.5),  # Center text
             col = text_col,
             cex = 0.8)  # Slightly smaller text
      }  

    }
    
    # Add palette labels
    text(-0.25, n_palettes-i+0.75, 
         labels = labels[i], 
         pos = 2, 
         xpd = TRUE)
  }
}

#' Generate variations of a base color
#' @param color Base color in hex format
#' @param max_delta_e Maximum perceptual difference allowed
#' @param n_variations Number of variations to generate
#' @return Vector of hex colors
generate_color_variations <- function(color, max_delta_e = 0.01, n_variations = 3) {
  # Convert base color to RGB and then to LAB
  rgb_base <- col2rgb(color)
  lab_base <- convert_colour(matrix(rgb_base/255, ncol=3), from = "rgb", to = "lab")
  
  variations <- matrix(nrow = n_variations, ncol = 3)
  
  # Generate variations in LAB space
  for(i in 1:n_variations) {
    # Adding small random noise to L, a, b channels, ensuring valid perceptual variations
    variation <- lab_base + matrix(rnorm(3, 0, max_delta_e/3), ncol=3)
    
    # Clamp the LAB values to reasonable perceptual ranges
    variation[1] <- pmin(pmax(variation[1], 0), 100)  # L: [0, 100]
    variation[2] <- pmin(pmax(variation[2], -128), 128)  # a: [-128, 128]
    variation[3] <- pmin(pmax(variation[3], -128), 128)  # b: [-128, 128]
    
    # Convert back to RGB and clip the values to [0, 1] range
    rgb_var <- convert_colour(variation, from = "lab", to = "rgb")
    rgb_var <- pmax(0, pmin(1, rgb_var))
    
    variations[i,] <- rgb_var
  }
  
  # Convert to hex colors
  hex_variations <- rgb(variations[,1], variations[,2], variations[,3])
  
  # Calculate perceptual distances for all generated variations
  distances <- sapply(hex_variations, function(var_color) {
    calc_perceptual_distance(color, var_color)
  })
  
  # Filter variations that meet the perceptual distance constraint
  valid_variations <- hex_variations[distances <= max_delta_e]
  
  # If no valid variations, return original color
  if(length(valid_variations) == 0) {
    return(color)
  }
  
  # Return the original color plus valid variations
  return(c(color, valid_variations))
}

generate_base_variants <- function(base_colors, n_variations, max_delta_e) {
  var1 = generate_color_variations(color = base_colors[[1]],
                                   max_delta_e = max_delta_e,
                                   n_variations = n_variations)
  var2 = generate_color_variations(color = base_colors[[2]],
                                   max_delta_e = max_delta_e,
                                   n_variations = n_variations)
  variations = vector(mode = "list", length = min(length(var1), length(var2)))
  for(i in seq_len(min(length(var1), length(var2)))){
    variations[[i]] = list(var1[[i]], var2[[i]])
  }
  return(variations)
}

generate_simple_palettes <- function(base_colors, n_colors = 8) {
  # Check that we have at least two colors in the input base variations
  if (length(base_colors) < 2) {
    stop("At least two base colors (or color variations) are required.")
  }
  
  # Convert the variations into LCh color space
  palette_lch <- lapply(base_colors, function(color) {
    rgb_vector <- col2rgb(color) / 255
    c(convert_colour(matrix(rgb_vector, ncol = 3), from = "rgb", to = "lch"))
  })
  
  # If we need more colors than the provided variations, interpolate in the LCh space
  n_needed_colors <- n_colors - length(palette_lch)
  if (n_needed_colors > 0) {
    for (i in 1:n_needed_colors) {
      # Create interpolated values
      l <- seq(palette_lch[[1]][1], palette_lch[[length(base_colors)]][1], 
               length.out = n_needed_colors + 2)[i+1]
      c <- seq(palette_lch[[1]][2], palette_lch[[length(base_colors)]][2], 
               length.out = n_needed_colors + 2)[i+1]
      h <- seq(palette_lch[[1]][3], palette_lch[[length(base_colors)]][3], 
               length.out = n_needed_colors + 2)[i+1]
      
      # Create a simple numeric vector for the interpolated color
      interp_color <- c(l, c, h)
      
      # Append to palette_lch as a simple vector
      palette_lch <- append(palette_lch, list(interp_color))
    }
  }
  
  # Convert the LCh colors back to RGB
  palette_rgb <- lapply(palette_lch, function(lch) {
    convert_colour(matrix(lch, ncol = 3), from = "lch", to = "rgb")
  })
  
  # Return the RGB values as hex colors
  palette_hex <- sapply(palette_rgb, function(rgb) {
    rgb_vals <- pmax(0, pmin(1, rgb))  # Ensure RGB values are within valid range
    rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3])
  })
  
  return(palette_hex)
}


#' Calculate metrics for a palette
#' @param palette Vector of hex colors
#' @return List of calculated metrics
calculate_palette_metrics <- function(palette) {
  if (is.null(palette) || length(palette) < 2) return(NULL)
  
  # Get all pairwise perceptual distances
  distances <- calc_palette_distances(palette)
  
  contrasts <- numeric()
  hue_diffs <- numeric()
  chroma_diffs <- numeric()
  lightness_vals <- numeric()
  homogeneity_scores <- numeric()
  
  for(i in 1:(length(palette)-1)) {
    for(j in (i+1):length(palette)) {
      rgb1 <- matrix(col2rgb(palette[[i]]) / 255, ncol = 3)
      rgb2 <- matrix(col2rgb(palette[[j]]) / 255, ncol = 3)
      
      # Lightness contrast calculation
      lab1 <- convert_colour(rgb1, from = "rgb", to = "lab")
      lab2 <- convert_colour(rgb2, from = "rgb", to = "lab")
      contrast <- abs(lab1[1] - lab2[1]) / 100
      contrasts <- c(contrasts, contrast)
      
      # Hue and chroma differences (LCh space)
      lch1 <- convert_colour(rgb1, from = "rgb", to = "lch")
      lch2 <- convert_colour(rgb2, from = "rgb", to = "lch")
      hue_diff <- min((lch1[3] - lch2[3]) %% 360, (lch2[3] - lch1[3]) %% 360) / 180
      chroma_diff <- abs(lch1[2] - lch2[2]) / 100
      hue_diffs <- c(hue_diffs, hue_diff)
      chroma_diffs <- c(chroma_diffs, chroma_diff)
    }
    
    # Collect lightness values for lightness spread metrics
    rgb_base <- matrix(col2rgb(palette[[i]]) / 255, ncol = 3)
    lab_base <- convert_colour(rgb_base, from = "rgb", to = "lab")
    lightness_vals <- c(lightness_vals, lab_base[1])
  }
  
  # Calculate additional metrics
  min_distance <- min(distances)
  mean_distance <- mean(distances)
  max_distance <- max(distances)
  min_contrast <- min(contrasts)
  mean_contrast <- mean(contrasts)
  
  # Calculate the spread of lightness contrasts
  lightness_contrast_spread <- max(contrasts) - min(contrasts)
  
  # Calculate hue diversity (range of hue differences)
  hue_diversity <- diff(range(hue_diffs))
  
  # Calculate chroma diversity (range of chroma differences)
  chroma_diversity <- diff(range(chroma_diffs))
  
  # Standard deviation of lightness for balanced distribution
  lightness_stddev <- sd(lightness_vals)
  
  # Homogeneity (proportion of colors within close perceptual distance)
  homogeneity_score <- sum(distances <= 0.05) / length(distances)  # Example threshold for "closeness"
  
  list(
    min_distance = min_distance,
    mean_distance = mean_distance,
    max_distance = max_distance,
    min_contrast = min_contrast,
    mean_contrast = mean_contrast,
    lightness_contrast_spread = lightness_contrast_spread,
    hue_diversity = hue_diversity,
    chroma_diversity = chroma_diversity,
    lightness_stddev = lightness_stddev,
    homogeneity_score = homogeneity_score
  )
}

# Helper function to check if metrics are valid
is_valid_metrics <- function(metrics) {
  if (is.null(metrics)) return(FALSE)
  metric_names <- c("min_distance", "mean_distance", "max_distance", 
                    "min_contrast", "mean_contrast", "lightness_contrast_spread",
                    "hue_diversity", "chroma_diversity", "lightness_stddev",
                    "homogeneity_score")
  all(sapply(metrics[metric_names], function(x) !is.null(x) && is.finite(x)))
}

simple_objective_function <- function(numeric_vector, metric_weights, initial_colors) {
  MAX_PENALTY <- 1e6
  
  tryCatch({
    if (length(numeric_vector) == 0 || length(numeric_vector) %% 3 != 0) {
      return(MAX_PENALTY)
    }
    
    if (!all(is.finite(numeric_vector))) {
      return(MAX_PENALTY)
    }
    
    numeric_vector <- pmax(0, pmin(1, numeric_vector))
    palette <- unflatten_palette(numeric_vector)
    
    # Initial colors preservation
    initial_rgb <- t(sapply(initial_colors, function(color) {
      rgb <- col2rgb(color)/255
      as.vector(rgb)
    }))
    current_rgb <- matrix(numeric_vector[1:(length(initial_colors) * 3)], 
                          ncol = 3, byrow = TRUE)
    initial_color_penalty <- sum((initial_rgb - current_rgb)^2) * 1000
    
    metrics <- calculate_palette_metrics(palette)
    if (!is_valid_metrics(metrics)) {
      return(MAX_PENALTY)
    }
    
    # More nuanced CVD handling
    cvd_score <- validate_cvd(palette)
    cvd_penalty <- if(cvd_score < 0.15) {
      300  # Severe penalty for very poor CVD performance
    } else if(cvd_score < 0.2) {
      150  # Moderate penalty for borderline cases
    } else {
      0    # No penalty for good CVD performance
    }
    
    # Calculate base score with adjusted components
    base_components <- c(
      metrics$min_distance * metric_weights["min_distance"],
      metrics$mean_distance * metric_weights["mean_distance"],
      metrics$max_distance * metric_weights["max_distance"],
      metrics$min_contrast * metric_weights["min_contrast"],
      metrics$mean_contrast * metric_weights["mean_contrast"],
      metrics$lightness_contrast_spread * metric_weights["lightness_contrast_spread"],
      metrics$hue_diversity * metric_weights["hue_diversity"] * 1.2,  # Boost hue diversity
      metrics$chroma_diversity * metric_weights["chroma_diversity"] * 1.1,  # Boost chroma diversity
      metrics$lightness_stddev * metric_weights["lightness_stddev"],
      -metrics$homogeneity_score * metric_weights["homogeneity_score"]
    )
    
    if (!all(is.finite(base_components))) {
      return(MAX_PENALTY)
    }
    
    base_score <- sum(base_components)
    final_score <- -(base_score - cvd_penalty - initial_color_penalty)
    
    if (!is.finite(final_score)) {
      return(MAX_PENALTY)
    }
    
    return(final_score)
  }, error = function(e) {
    return(MAX_PENALTY)
  })
}

scientific_objective_function <- function(numeric_vector, metric_weights, initial_colors) {
  MAX_PENALTY <- 1e6
  
  tryCatch({
    if (length(numeric_vector) == 0 || length(numeric_vector) %% 3 != 0) {
      return(MAX_PENALTY)
    }
    
    numeric_vector <- pmax(0, pmin(1, numeric_vector))
    palette <- unflatten_palette(numeric_vector)
    
    # Preserve initial colors
    initial_rgb <- t(sapply(initial_colors, function(color) {
      rgb <- col2rgb(color)/255
      as.vector(rgb)
    }))
    current_rgb <- matrix(numeric_vector[1:(length(initial_colors) * 3)], 
                          ncol = 3, byrow = TRUE)
    initial_color_penalty <- sum((initial_rgb - current_rgb)^2) * 1000
    
    # Calculate enhanced perceptual distances
    enhanced_distances <- numeric()
    for(i in 1:(length(palette)-1)) {
      for(j in (i+1):length(palette)) {
        enhanced_distances <- c(enhanced_distances, 
                                enhanced_perceptual_distance(palette[i], palette[j]))
      }
    }
    
    # Calculate luminance monotonicity
    monotonicity_penalty <- calc_luminance_monotonicity(palette) * 50
    
    # Calculate standard metrics
    metrics <- calculate_palette_metrics(palette)
    if (!is_valid_metrics(metrics)) {
      return(MAX_PENALTY)
    }
    
    # Enhanced CVD handling based on Viridis principles
    cvd_score <- validate_cvd(palette)
    cvd_penalty <- if(cvd_score < 0.15) {
      300
    } else if(cvd_score < 0.2) {
      150
    } else {
      0
    }
    
    # Calculate perceptual uniformity score
    rgb_vals <- t(sapply(palette, function(color) {
      col2rgb(color)/255
    }))
    lab_vals <- convert_colour(rgb_vals, from="rgb", to="lab")
    
    lightness_steps <- diff(lab_vals[,1])
    chroma_steps <- diff(sqrt(lab_vals[,2]^2 + lab_vals[,3]^2))
    
    uniformity_score <- -sd(lightness_steps) - sd(chroma_steps)
    
    # Combine all components
    base_score <- sum(c(
      min(enhanced_distances) * metric_weights["min_distance"],
      mean(enhanced_distances) * metric_weights["mean_distance"],
      metrics$min_contrast * metric_weights["min_contrast"],
      metrics$lightness_contrast_spread * metric_weights["lightness_contrast_spread"],
      metrics$hue_diversity * metric_weights["hue_diversity"],
      uniformity_score * 0.1
    ))
    
    final_score <- -(base_score - cvd_penalty - initial_color_penalty - monotonicity_penalty)
    
    if (!is.finite(final_score)) {
      return(MAX_PENALTY)
    }
    
    return(final_score)
  }, error = function(e) {
    return(MAX_PENALTY)
  })
}

harmonious_objective_function <- function(numeric_vector, metric_weights, initial_colors) {
  MAX_PENALTY <- 1e6
  
  tryCatch({
    if (length(numeric_vector) == 0 || length(numeric_vector) %% 3 != 0) {
      return(MAX_PENALTY)
    }
    
    numeric_vector <- pmax(0, pmin(1, numeric_vector))
    palette <- unflatten_palette(numeric_vector)
    
    # Preserve initial colors
    initial_rgb <- t(sapply(initial_colors, function(color) {
      rgb <- col2rgb(color)/255
      as.vector(rgb)
    }))
    current_rgb <- matrix(numeric_vector[1:(length(initial_colors) * 3)], 
                          ncol = 3, byrow = TRUE)
    initial_color_penalty <- sum((initial_rgb - current_rgb)^2) * 1000
    
    # Calculate enhanced perceptual distances
    enhanced_distances <- numeric()
    for(i in 1:(length(palette)-1)) {
      for(j in (i+1):length(palette)) {
        enhanced_distances <- c(enhanced_distances, 
                                enhanced_perceptual_distance(palette[i], palette[j]))
      }
    }
    
    # Calculate harmony score
    harmony_score <- calc_harmony_score(palette)
    
    # Calculate standard metrics
    metrics <- calculate_palette_metrics(palette)
    if (!is_valid_metrics(metrics)) {
      return(MAX_PENALTY)
    }
    
    # More balanced CVD handling
    cvd_score <- validate_cvd(palette)
    cvd_penalty <- if(cvd_score < 0.15) {
      200  # Reduced penalty to allow more harmony
    } else if(cvd_score < 0.2) {
      100
    } else {
      0
    }
    
    # Calculate perceptual uniformity with more emphasis on smooth transitions
    rgb_vals <- t(sapply(palette, function(color) {
      col2rgb(color)/255
    }))
    lab_vals <- convert_colour(rgb_vals, from="rgb", to="lab")
    
    lightness_steps <- diff(lab_vals[,1])
    chroma_steps <- diff(sqrt(lab_vals[,2]^2 + lab_vals[,3]^2))
    
    uniformity_score <- -sd(lightness_steps) - sd(chroma_steps)
    
    # Combine all components with harmony
    base_score <- sum(c(
      min(enhanced_distances) * metric_weights["min_distance"] * 0.8,  # Reduced weight
      mean(enhanced_distances) * metric_weights["mean_distance"] * 0.8,
      metrics$min_contrast * metric_weights["min_contrast"],
      metrics$lightness_contrast_spread * metric_weights["lightness_contrast_spread"],
      metrics$hue_diversity * metric_weights["hue_diversity"] * 0.7,  # Reduced weight
      uniformity_score * 0.15,
      harmony_score * 0.3  # New harmony component
    ))
    
    final_score <- -(base_score - cvd_penalty - initial_color_penalty)
    
    if (!is.finite(final_score)) {
      return(MAX_PENALTY)
    }
    
    return(final_score)
  }, error = function(e) {
    return(MAX_PENALTY)
  })
}

categorical_objective_function <- function(numeric_vector, metric_weights, initial_colors) {
  MAX_PENALTY <- 1e6
  
  tryCatch({
    if (length(numeric_vector) == 0 || length(numeric_vector) %% 3 != 0) {
      return(MAX_PENALTY)
    }
    
    numeric_vector <- pmax(0, pmin(1, numeric_vector))
    palette <- unflatten_palette(numeric_vector)
    
    # 1. Preserve initial colors
    initial_rgb <- t(sapply(initial_colors, function(color) {
      rgb <- col2rgb(color)/255
      as.vector(rgb)
    }))
    current_rgb <- matrix(numeric_vector[1:(length(initial_colors) * 3)], 
                          ncol = 3, byrow = TRUE)
    initial_color_penalty <- sum((initial_rgb - current_rgb)^2) * 1000
    
    # 2. Calculate perceptual distances between all pairs
    distances <- matrix(0, nrow = length(palette), ncol = length(palette))
    for(i in 1:length(palette)) {
      for(j in 1:length(palette)) {
        if(i != j) {
          distances[i,j] <- calc_perceptual_distance(palette[i], palette[j])
        }
      }
    }
    
    # 3. Convert to LAB space for luminance analysis
    rgb_vals <- t(sapply(palette, function(color) {
      col2rgb(color)/255
    }))
    lab_vals <- convert_colour(rgb_vals, from="rgb", to="lab")
    
    # 4. Key components for categorical effectiveness:
    
    # a) Minimum distance between any pair - crucial for categorical distinction
    min_dist <- min(distances[distances > 0])
    min_dist_score <- min_dist * 2.0  # High weight as this is crucial
    
    # b) Variance of pairwise distances - should be low for equal distinctiveness
    dist_variance <- -sd(distances[distances > 0]) * 0.5
    
    # c) Luminance uniformity - colors should have similar "weight"
    luminance_spread <- -sd(lab_vals[,1]) * 1.0
    
    # d) Penalty for clusters - discourage groups of similar colors
    cluster_penalty <- sum(distances[distances > 0 & distances < 0.2]) * 0.5
    
    # e) Penalize extreme luminance values
    extreme_luminance_penalty <- sum(lab_vals[,1] < 20 | lab_vals[,1] > 90) * 50
    
    # f) Color blindness considerations
    cvd_score <- validate_cvd(palette)
    cvd_penalty <- if(cvd_score < 0.15) {
      200
    } else if(cvd_score < 0.2) {
      100
    } else {
      0
    }
    
    # g) Chroma variation - ensure colors aren't too desaturated
    chroma_vals <- sqrt(lab_vals[,2]^2 + lab_vals[,3]^2)
    chroma_penalty <- -sum(chroma_vals < 30) * 30
    
    # h) Hue spacing - encourage good hue distribution
    lch_vals <- convert_colour(rgb_vals, from="rgb", to="lch")
    hue_diffs <- diff(sort(lch_vals[,3]))
    hue_spacing_score <- -sd(hue_diffs) * 0.3
    
    # Combine all components
    final_score <- -(
      min_dist_score +
        dist_variance +
        luminance_spread +
        cluster_penalty +
        extreme_luminance_penalty +
        chroma_penalty +
        hue_spacing_score
    ) - cvd_penalty - initial_color_penalty
    
    if (!is.finite(final_score)) {
      return(MAX_PENALTY)
    }
    
    return(final_score)
  }, error = function(e) {
    return(MAX_PENALTY)
  })
}

optimize_simple_palette <- function(initial_palette, weights, max_attempts = 10) {
  if (length(initial_palette) == 0) {
    stop("Initial palette cannot be empty")
  }
  
  initial_numeric <- flatten_palette(initial_palette)
  initial_colors <- initial_palette[1:2]  # Store original colors
  
  if (length(initial_numeric) == 0 || length(initial_numeric) %% 3 != 0) {
    stop("Invalid initial numeric vector")
  }
  
  best_result <- NULL
  best_score <- Inf
  
  for (attempt in 1:max_attempts) {
    if (attempt > 1) {
      # Add smaller noise and preserve initial colors
      noise <- rnorm(length(initial_numeric), 0, 0.05)
      noise[1:(length(initial_colors) * 3)] <- 0  # No noise for initial colors
      current_initial <- pmax(0, pmin(1, initial_numeric + noise))
    } else {
      current_initial <- initial_numeric
    }
    
    result <- tryCatch({
      optim(
        par = current_initial,
        fn = simple_objective_function,
        metric_weights = weights,
        initial_colors = initial_colors,
        method = "L-BFGS-B",
        lower = rep(0, length(initial_numeric)),
        upper = rep(1, length(initial_numeric)),
        control = list(
          maxit = 2000,          # Increased maximum iterations
          factr = 1e14,          # Decreased factr for better precision
          pgtol = 1e-6           # Decreased pgtol for better convergence
        )
      )
    }, error = function(e) NULL)
    
    if (!is.null(result) && is.finite(result$value) && result$value < best_score) {
      best_result <- result
      best_score <- result$value
    }
  }
  
  if (is.null(best_result)) {
    stop("Optimization failed to find valid solution")
  }
  
  optimized_palette <- unflatten_palette(best_result$par)
  
  return(list(
    palette = optimized_palette,
    score = best_result$value,
    convergence = best_result$convergence,
    initial_colors = initial_colors
  ))
}

optimize_scientific_palette <- function(initial_palette,
                                        weights = scientific_weights,
                                        max_attempts = 15) {
  if (length(initial_palette) == 0) {
    stop("Initial palette cannot be empty")
  }
  
  initial_numeric <- flatten_palette(initial_palette)
  initial_colors <- initial_palette[1:2]
  
  best_result <- NULL
  best_score <- Inf
  
  for (attempt in 1:max_attempts) {
    if (attempt > 1) {
      noise <- rnorm(length(initial_numeric), 0, 0.03)
      noise[1:(length(initial_colors) * 3)] <- 0
      current_initial <- pmax(0, pmin(1, initial_numeric + noise))
    } else {
      current_initial <- initial_numeric
    }
    
    result <- tryCatch({
      optim(
        par = current_initial,
        fn = harmonious_objective_function,
        metric_weights = weights,
        initial_colors = initial_colors,
        method = "L-BFGS-B",
        lower = rep(0, length(initial_numeric)),
        upper = rep(1, length(initial_numeric)),
        control = list(
          maxit = 2500,
          factr = 1e13,
          pgtol = 1e-7
        )
      )
    }, error = function(e) NULL)
    
    if (!is.null(result) && is.finite(result$value) && result$value < best_score) {
      best_result <- result
      best_score <- result$value
    }
  }
  
  if (is.null(best_result)) {
    stop("Optimization failed to find valid solution")
  }
  
  optimized_palette <- unflatten_palette(best_result$par)
  
  return(list(
    palette = sort_palette(optimized_palette),
    score = best_result$value,
    convergence = best_result$convergence
  ))
}

optimize_categorical_palette <- function(initial_palette,
                                        weights = scientific_weights,
                                        max_attempts = 15) {
  if (length(initial_palette) == 0) {
    stop("Initial palette cannot be empty")
  }
  
  initial_numeric <- flatten_palette(initial_palette)
  initial_colors <- initial_palette[1:2]
  
  best_result <- NULL
  best_score <- Inf
  
  for (attempt in 1:max_attempts) {
    if (attempt > 1) {
      noise <- rnorm(length(initial_numeric), 0, 0.03)
      noise[1:(length(initial_colors) * 3)] <- 0
      current_initial <- pmax(0, pmin(1, initial_numeric + noise))
    } else {
      current_initial <- initial_numeric
    }
    
    result <- tryCatch({
      optim(
        par = current_initial,
        fn = categorical_objective_function,
        metric_weights = weights,
        initial_colors = initial_colors,
        method = "L-BFGS-B",
        lower = rep(0, length(initial_numeric)),
        upper = rep(1, length(initial_numeric)),
        control = list(
          maxit = 2500,
          factr = 1e13,
          pgtol = 1e-7
        )
      )
    }, error = function(e) NULL)
    
    if (!is.null(result) && is.finite(result$value) && result$value < best_score) {
      best_result <- result
      best_score <- result$value
    }
  }
  
  if (is.null(best_result)) {
    stop("Optimization failed to find valid solution")
  }
  
  optimized_palette <- unflatten_palette(best_result$par)
  
  return(list(
    palette = sort_palette(optimized_palette),
    score = best_result$value,
    convergence = best_result$convergence
  ))
}

#' Convert a hex color palette to a numeric vector
#' @param palette Vector of hex colors
#' @return Numeric vector representing RGB values (scaled 0-1)
flatten_palette <- function(palette) {
  # Convert each hex color to RGB values
  rgb_values <- t(sapply(palette, function(color) {
    rgb <- col2rgb(color)/255
    as.vector(rgb)
  }))
  
  # Flatten the matrix into a vector
  as.vector(rgb_values)
}

#' Convert a numeric vector back to hex colors
#' @param numeric_vector Vector of RGB values (must be multiple of 3)
#' @return Vector of hex colors
unflatten_palette <- function(numeric_vector) {
  # Check if length is multiple of 3
  if (length(numeric_vector) %% 3 != 0) {
    stop("Numeric vector length must be multiple of 3")
  }
  
  # Ensure numeric_vector is not empty
  if (length(numeric_vector) == 0) {
    stop("Numeric vector cannot be empty")
  }
  
  # Reshape into matrix with 3 columns (R,G,B)
  n_colors <- length(numeric_vector) / 3
  rgb_matrix <- matrix(numeric_vector, nrow = n_colors, ncol = 3, byrow = TRUE)
  
  # Clamp values to valid RGB range [0,1]
  rgb_matrix[] <- pmax(0, pmin(1, rgb_matrix))
  
  # Convert to hex colors
  hex_colors <- apply(rgb_matrix, 1, function(rgb) {
    rgb(rgb[1], rgb[2], rgb[3])
  })
  
  return(hex_colors)
}

test_simple <- function() {
  base_colors <- c("#F9A904", "#00559D")
  
  metric_weights <- c(
    min_distance = 0.2,
    mean_distance = 0.15,
    max_distance = 0.1,
    min_contrast = 0.15,
    mean_contrast = 0.1,
    lightness_contrast_spread = 0.1,
    hue_diversity = 0.1,
    chroma_diversity = 0.05,
    lightness_stddev = 0.05,
    homogeneity_score = 0.05
  )

base_variants <- generate_base_variants(base_colors,
                                          max_delta_e = 0.03,
                                          n_variations = 15)
compare_palettes(base_variants)

initial_palettes <- lapply(
  base_variants,
  function(i) {
    generate_simple_palettes(i, n_colors = 8)
  }
)

compare_palettes(lapply(initial_palettes, sort_palette))

cat("Starting optimization...\n")
result <- future_lapply(initial_palettes,
                        FUN = optimize_simple_palette,
                        weights = weights,
                        future.seed = TRUE)
cat("Optimization complete\n")
# Compare the palettes
compare_palettes(lapply(result, function(x) x$palette))
# Check convergence
print(sapply(result, function(x) x$convergence))

return(results)
}

test_scientific <- function(){
  base_colors <- c("#F9A904", "#00559D")
  weights <- c(
    min_distance = 0.25,
    mean_distance = 0.15,
    min_contrast = 0.20,
    lightness_contrast_spread = 0.15,
    hue_diversity = 0.15,
    chroma_diversity = 0.10
  )
  
  base_variants <- generate_base_variants(base_colors,
                                          max_delta_e = 0.03,
                                          n_variations = 15)
  compare_palettes(base_variants)
  
  initial_palettes <- lapply(
    base_variants,
    function(i) {
      generate_simple_palettes(i, n_colors = 8)
    }
  )
  
  compare_palettes(lapply(initial_palettes, sort_palette))
  
  
  cat("Starting optimization...\n")
  result <- future_lapply(initial_palettes,
                          FUN = optimize_scientific_palette,
                          weights = weights,
                          future.seed = TRUE)
  compare_palettes(lapply(result, function(x) x$palette))
  # Check perceptual uniformity
  rgb_vals <- t(sapply(result[[2]]$palette, function(color) col2rgb(color)/255))
  lab_vals <- convert_colour(rgb_vals, from="rgb", to="lab")
  plot(lab_vals[,1], type="l", main="Lightness Progression")

  return(result)
}

test_categorical <- function(){
  base_colors <- c("#F9A904", "#00559D")
  weights <- c(
    min_distance = 2.0,        # Highest priority - colors must be distinct
    dist_variance = 0.5,       # Medium priority - ensure equal distinctiveness
    luminance_spread = 1.0,    # High priority - similar visual weight
    cluster_penalty = 0.5,     # Medium priority - avoid similar colors
    extreme_luminance = 50,    # High penalty for too dark/light colors
    chroma_penalty = 30,       # Significant penalty for desaturated colors
    hue_spacing = 0.3         # Lower priority - allow some hue clustering if needed
  )
  
  base_variants <- generate_base_variants(base_colors,
                                          max_delta_e = 0.03,
                                          n_variations = 15)
  compare_palettes(base_variants)
  
  initial_palettes <- lapply(
    base_variants,
    function(i) {
      generate_simple_palettes(i, n_colors = 8)
    }
  )
  
  compare_palettes(lapply(initial_palettes, sort_palette))
  
  
  cat("Starting optimization...\n")
  result <- future_lapply(initial_palettes,
                          FUN = optimize_categorical_palette,
                          weights = weights,
                          future.seed = TRUE)
  compare_palettes(lapply(result, function(x) x$palette))
  # Check perceptual uniformity
  rgb_vals <- t(sapply(result[[2]]$palette, function(color) col2rgb(color)/255))
  lab_vals <- convert_colour(rgb_vals, from="rgb", to="lab")
  plot(lab_vals[,1], type="l", main="Lightness Progression")
  
  return(result)
}


simple_results = test_simple()

sciency_result = test_scientific()



#####################################################################################



# Force-directed layout in LAB space
simulate_color_repulsion <- function(n_colors,
                                                 max_iterations = 100,
                                                 learning_rate = 50,
                                                 base_colors = NULL,
                                                 save_every = 5,
                                                 boundary_force = 0.3,
                                     return_states = FALSE) { # New parameter for boundary force
  
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
    
    # Calculate minimum color distance to any other point
    min_color_dist <- Inf
    if(n_random > 0) {
      for(i in (n_base + 1):n_colors) {
        for(j in 1:n_colors) {
          if(i != j) {
            rgb_i <- convert_colour(matrix(points[i,], ncol=3), from="lab", to="rgb")
            rgb_j <- convert_colour(matrix(points[j,], ncol=3), from="lab", to="rgb")
            dist <- compare_colour(rgb_i, rgb_j, "rgb", "lab", method="cie2000")
            min_color_dist <- min(min_color_dist, dist)
          }
        }
      }
    }
    
    # More gradual learning rate adaptation with escape mechanism
    force_factor <- 1 / (1 + 0.05 * avg_force_magnitude)  # Much less sensitive to force magnitude
    
    # Increase learning rate when points are too close
    distance_boost <- if(min_color_dist < 15) {  # Boost if points are too close
      2.0  # Significant boost to escape local minima
    } else {
      1.0
    }
    
    # Modified distance factor that maintains movement when needed
    distance_threshold <- 25  # Higher threshold
    distance_factor <- if(avg_color_dist > distance_threshold) {
      1 / (1 + 0.02 * (avg_color_dist - distance_threshold))  # Much gentler reduction
    } else {
      1.2  # Slight boost when colors aren't well spread
    }
    
    # Very gentle progress decay
    progress_factor <- 1 - 0.3 * (iter/max_iterations)^0.1  # Even slower decay
    
    # Combine factors with a higher minimum learning rate
    current_lr <- learning_rate * force_factor * distance_factor * progress_factor * distance_boost
    current_lr <- max(current_lr, learning_rate * 0.2)  # Never go below 20% of initial rate
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
  
  if(return_states){
    return(states)    
  } else {
    return(tail(states, n = 1)[[1]])
  }
}

generate_base_distribution <- function(n_colors, base_colors = NULL, iterations = 100) {
  # Use force-directed layout to get well-distributed points
  lab_points <- simulate_color_repulsion(n_colors,
                                         base_colors = base_colors,
                                         max_iterations = iterations
                                         )

  
  # Convert to RGB
  rgb_colors <- convert_colour(lab_points, from="lab", to="rgb")/255
  
  # Clip to valid RGB space
  #rgb_colors <- matrix(pmax(0, pmin(1, rgb_colors)), ncol = 3, byrow = TRUE)
  
  # Convert to hex
  hex_colors <- rgb(rgb_colors[,1], rgb_colors[,2], rgb_colors[,3])
  
  return(list(
    hex = hex_colors,
    lab = lab_points,
    rgb = rgb_colors
  ))
}

adjust_for_cvd <- function(colors, iterations = 100, base_colors = NULL) {
  lab_points <- convert_colour(255 * colors$rgb, from="rgb", to="lab")
  L_range <- if(!is.null(base_colors)) {
    rgb_base <- t(sapply(base_colors, function(c) col2rgb(c)))
    lab_base <- convert_colour(rgb_base, from="rgb", to="lab")
    c(min(lab_base[,1]), max(lab_base[,1]))
  }
  
  # Find indices of base colors if provided
  if(!is.null(base_colors)) base_indices <- which(colors$hex %in% base_colors)
  modify_indices <- setdiff(1:nrow(lab_points), base_indices)
  
  best_score <- validate_cvd(colors$hex)
  best_points <- lab_points
  
  for(i in 1:iterations) {
    # Create noise matrix
    noise <- matrix(0, nrow=nrow(lab_points), ncol=3)
    noise[modify_indices,1] <- matrix(
      rnorm(length(modify_indices),
            0,
            0.1 * mean(abs(lab_points[,1]))),
      ncol=1)
    
    noise[modify_indices,2] <- matrix(
      rnorm(length(modify_indices),
            0,
            0.1 * mean(abs(lab_points[,2]))),
      ncol=1)
    
    noise[modify_indices,3] <- matrix(
      rnorm(length(modify_indices),
            0,
            0.1 * mean(abs(lab_points[,3]))),
      ncol=1)
    
    new_points <- lab_points + noise
    
    # Constrain to valid LAB space
    new_points[,1] <- pmax(L_range[[1]], pmin(L_range[[2]], new_points[,1]))
    new_points[,2] <- pmax(-80, pmin(80, new_points[,2]))
    new_points[,3] <- pmax(-80, pmin(80, new_points[,3]))
    
    # Convert to RGB for evaluation
    rgb_test <- convert_colour(new_points, from="lab", to="rgb")/255
    # Check if in gamut
    if(any(rgb_test < 0 | rgb_test > 1)) next
    
    hex_test <- rgb(rgb_test[,1], rgb_test[,2], rgb_test[,3])
    score <- validate_cvd(hex_test)
    
    if(score > best_score) {
      best_score <- score
      best_points <- new_points
    }
  }
  
  # Convert best result back to RGB and hex
  best_rgb <- convert_colour(best_points, from="lab", to="rgb")/255
  best_hex <- rgb(best_rgb[,1], best_rgb[,2], best_rgb[,3])
  
  return(list(
    hex = best_hex,
    lab = best_points,
    rgb = best_rgb
  ))
}

adjust_for_cvd_cie <- function(colors, iterations = 50, base_colors = NULL) {
  lab_points <- convert_colour(255 * colors$rgb, from="rgb", to="lab")
  L_range <- if(!is.null(base_colors)) {
    rgb_base <- t(sapply(base_colors, function(c) col2rgb(c)))
    lab_base <- convert_colour(rgb_base, from="rgb", to="lab")
    c(min(lab_base[,1]), max(lab_base[,1]))
  }
  
  # Find indices of base colors if provided
  if(!is.null(base_colors)) base_indices <- which(colors$hex %in% base_colors)
  
  best_score <- validate_cvd(colors$hex)
  best_points <- lab_points
  
  for(iter in 1:iterations) {
    # Calculate gradients based on CIE2000 distances
    gradients <- matrix(0, nrow=nrow(lab_points), ncol=3)
    
    # For each pair of colors
    for(i in 1:(nrow(lab_points)-1)) {
      for(j in (i+1):nrow(lab_points)) {
        # Current CIE2000 distance
        rgb_i <- convert_colour(matrix(lab_points[i,], ncol=3), from="lab", to="rgb")
        rgb_j <- convert_colour(matrix(lab_points[j,], ncol=3), from="lab", to="rgb")
        current_dist <- compare_colour(rgb_i, rgb_j, "rgb", "lab", method="cie2000")
        
        # Small perturbation to estimate gradient
        eps <- 0.1
        for(dim in 1:3) {
          lab_i_plus <- lab_points[i,]
          lab_i_plus[dim] <- lab_i_plus[dim] + eps
          
          rgb_i_plus <- convert_colour(matrix(lab_i_plus, ncol=3), from="lab", to="rgb")
          new_dist <- compare_colour(rgb_i_plus, rgb_j, "rgb", "lab", method="cie2000")
          
          # Gradient estimate
          grad <- (new_dist - current_dist) / eps
          gradients[i,dim] <- gradients[i,dim] + grad
          gradients[j,dim] <- gradients[j,dim] - grad
        }
      }
    }
    gradients[base_indices,] = 0
    # Apply gradients with learning rate
    lr <- 0.01 * (1 - iter/iterations)
    new_points <- lab_points + lr * gradients
    
    # Keep lightness within range
    new_points[,1] <- pmax(L_range[[1]], pmin(L_range[[2]], new_points[,1]))
    
    # Convert to RGB to check if in gamut
    rgb_test <- convert_colour(new_points, from="lab", to="rgb")/255
    if(any(rgb_test < 0 | rgb_test > 1)) next
    
    hex_test <- rgb(rgb_test[,1], rgb_test[,2], rgb_test[,3])
    score <- validate_cvd(hex_test)
    
    if(score > best_score) {
      best_score <- score
      best_points <- new_points
    }
  }
  rgb_colors = convert_colour(best_points, from="lab", to="rgb")/255
  return(list(
    hex = rgb(rgb_colors[,1], rgb_colors[,2], rgb_colors[,3]),
    lab = best_points,
    rgb = rgb_colors
  ))
}

fine_tune_aesthetics <- function(colors, base_colors = NULL, 
                                 min_chroma = 30, target_chroma = 60, 
                                 maintain_separation = TRUE) {
  lab_points <- convert_colour(255 * colors$rgb, from="rgb", to="lab")
  target_L <- lab_points[,1]
  
  # Find indices of base colors if provided
  base_indices <- if(!is.null(base_colors)) {
    which(colors$hex %in% base_colors)
  } else numeric(0)
  
  modify_indices <- setdiff(1:nrow(lab_points), base_indices)
  
  if(length(modify_indices) > 0) {
    # Convert to LCH for easier chroma manipulation
    lch_points <- convert_colour(lab_points, from="lab", to="lch")
    
    # Function to check if colors are still distinguishable
    check_separation <- function(new_lab) {
      min_dist <- Inf
      for(i in 1:(nrow(new_lab)-1)) {
        for(j in (i+1):nrow(new_lab)) {
          rgb_i <- convert_colour(matrix(new_lab[i,], ncol=3), from="lab", to="rgb")
          rgb_j <- convert_colour(matrix(new_lab[j,], ncol=3), from="lab", to="rgb")
          dist <- compare_colour(rgb_i, rgb_j, "rgb", "lab", method="cie2000")
          min_dist <- min(min_dist, dist)
        }
      }
      return(min_dist)
    }
    
    # Get initial minimum separation
    #initial_separation <- check_separation(lab_points)
    initial_separation <- validate_cvd(colors$hex)
    
    
    # Increase chroma gradually while maintaining separation
    for(i in modify_indices) {
      current_chroma <- lch_points[i,2]
      
      if(current_chroma < target_chroma) {
        # Try to increase chroma
        test_lch <- lch_points
        
        # Find maximum possible chroma increase that maintains RGB gamut
        max_chroma <- current_chroma
        step_size <- 1
        
        while(TRUE) {
          test_chroma <- max_chroma + step_size
          if(test_chroma > target_chroma) {break}
          test_lch[i,2] <- test_chroma
          
          # Convert to RGB to check if in gamut
          test_lab <- convert_colour(colour = matrix(test_lch[i,], ncol = 3), from="lch", to="lab")
          test_rgb <- convert_colour(test_lab, from="lab", to="rgb")
          
          # Check if RGB values are valid and maintain separation
          if(all(test_rgb >= 0 & test_rgb <= 255)) {
            if(!maintain_separation) {
              max_chroma <- test_chroma
            } else {
              # Check if color separation is maintained
              test_all_lab <- convert_colour(test_lch, from="lch", to="lab")
              
              rgb_colors <- convert_colour(test_all_lab, from="lab", to="rgb")/255
              hex_colors <- rgb(rgb_colors[,1], rgb_colors[,2], rgb_colors[,3])
              if(validate_cvd(hex_colors) >= initial_separation * 0.95) {
              
              #if(check_separation(test_all_lab) >= initial_separation * 0.95) {
                max_chroma <- test_chroma
              } else {
                break
              }
            }
          } else {
            break
          }
        }
        
        # Set final chroma to maximum found or target, whichever is smaller
        lch_points[i,2] <- min(max_chroma, target_chroma)
      }
    }
    
    # Ensure minimum chroma
    low_chroma <- lch_points[modify_indices,2] < min_chroma
    if(any(low_chroma)) {
      lch_points[modify_indices[low_chroma],2] <- min_chroma
    }
    
    # Convert back to LAB
    lab_points[modify_indices,] <- convert_colour(lch_points[modify_indices,], from="lch", to="lab")
  }
  
  # Convert to RGB
  rgb_colors <- convert_colour(lab_points, from="lab", to="rgb")/255
  hex_colors <- rgb(rgb_colors[,1], rgb_colors[,2], rgb_colors[,3])
  
  return(list(
    hex = hex_colors,
    lab = lab_points,
    rgb = rgb_colors
  ))
}

generate_categorical_palette <- function(n_colors, base_colors = NULL, iterations = 100) {
  # Stage 1: Generate well-distributed colors
  cat("Stage 1: Generating base distribution...\n")
  colors <- generate_base_distribution(n_colors = n_colors, base_colors = base_colors, iterations = iterations)
  hex1 = colors$hex
  
  # Stage 2: Adjust for CVD
  cat("Stage 2: Adjusting for color vision deficiency...\n")
  colors <- adjust_for_cvd(colors, base_colors = base_colors, iterations = iterations)
  #colors <- adjust_for_cvd_cie(colors, base_colors = base_colors, iterations = iterations)
  hex2 = colors$hex
  
  # Stage 3: Fine-tune aesthetics
  cat("Stage 3: Fine-tuning aesthetics...\n")
  colors <- fine_tune_aesthetics(colors,  base_colors = base_colors)
  hex3 = colors$hex
  
  compare_palettes(list(hex1, hex2, hex3), labels = c("repulsion", "cvd-adjustment", "aesthetics"),add_hex = TRUE)
  return(colors)
}

plot_palette_info <- function(colors) {
  par(mfrow=c(2,2))
  
  # Plot colors
  plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", main="Palette")
  for(i in seq_along(colors$hex)) {
    rect((i-1)/length(colors$hex), 0, i/length(colors$hex), 1, 
         col=colors$hex[i], border=NA)
  }
  
  # Plot LAB space distribution
  plot(colors$lab[,2], colors$lab[,3], 
       col=colors$hex, pch=16, cex=2,
       xlab="a", ylab="b", main="LAB Space Distribution")
  
  # Plot lightness distribution
  hist(colors$lab[,1], main="Lightness Distribution",
       xlab="L", breaks=10)
  
  # Plot CVD simulation
  deutan_sim <- deutan(colors$hex)
  plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", 
       main="Deuteranopia Simulation")
  for(i in seq_along(deutan_sim)) {
    rect((i-1)/length(deutan_sim), 0, i/length(deutan_sim), 1, 
         col=deutan_sim[i], border=NA)
  }
}

result <- generate_categorical_palette(8, base_colors = c("#E69F00", "#0072B2"),
                                       iterations = 500)

result <- generate_categorical_palette(8, base_colors = c("#001959", "#114761", "#FDB0AA", "#F9CCF9"),
                                       iterations = 500)

plot_palette_info(result)

# Compare with original base colors
compare_palettes(list(
  foo,
  c(result$hex[c(1,2)], result$hex[5:8], result$hex[c(3,4)]) 
), labels = c("batlow8", "batlow mutation"), add_hex = TRUE)
