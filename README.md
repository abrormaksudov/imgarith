# imgarith

<!-- badges: start -->
<!-- badges: end -->

## Overview

`imgarith` is an R package that provides tools for performing arithmetic operations on images. It extends the functionality of the `magick` package by allowing direct arithmetic operations between images and numeric values.

## Features

- **Arithmetic operations on images:**
  - **Addition (`+`)**: Combine images or increase pixel values
  - **Subtraction (`-`)**: Difference between images or decrease pixel values
  - **Multiplication (`*`)**: Scale pixel values
  - **Division (`/`)**: Divide pixel values

- **Image manipulation utilities:**
  - `image_adjust()`: Modify pixel values in specific image regions
  - `image_resize_all()`: Resize multiple images to matching dimensions
  - `image_scale()`: Scale images by integer factors (ideal for pixel art)

## Installation

You can install the development version of `imgarith` from GitHub:

```r
# install.packages("devtools")
devtools::install_github("abrormaksudov/imgarith")
```

## Usage
### Basic Arithmetic

```r
library(imgarith)
library(magick)
```

# Create test images
```r
img1 <- magick::image_blank(100, 100, "rgb(100,150,200)")
img2 <- magick::image_blank(100, 100, "rgb(200,100,50)")
```

# Image-image operations
```r
sum_img <- img1 + img2      # Add images
diff_img <- img1 - img2     # Subtract images
prod_img <- img1 * img2     # Multiply images
quot_img <- img1 / img2     # Divide images
```

# Image-number operations
```r
bright <- img1 + 50         # Increase brightness
dark <- img1 - 30           # Decrease brightness
double <- img1 * 2          # Double pixel values
half <- img1 / 2            # Halve pixel values
```

## Image Manipulation

# Adjust specific region
```r
result <- image_adjust(img1, 1:50, 1:50, 100, channels = 1)  # Make upper left quarter more red
```

# Scale image
```r
scaled <- image_scale(img1, factor = 2, how = "both")  # Double size in both dimensions
```

# Resize multiple images to match
```r
resized <- image_resize_all(img1, img2, width = 800, height = 600)
```

## Requirements

- magick

## License
 
MIT License

