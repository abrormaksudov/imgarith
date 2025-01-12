#' Arithmetic Operations for Magick Images
#'
#' @description
#' Implements pixel-wise arithmetic operations for magick images.
#' Operations can be performed between:
#' * Two magick images
#' * A magick image and a numeric value
#' * A numeric value and a magick image (for commutative operations)
#'
#' @param e1 First operand (magick image or numeric)
#' @param e2 Second operand (magick image or numeric)
#'
#' @return A new magick image object resulting from the operation
#'
#' @details
#' Operations between two images are performed channel-wise.
#' Operations with a numeric value apply the operation to all pixels.
#' All results wrap around in the range 0-255.
#'
#' Valid operations:
#' * Addition: `img + img` or `img + n` or `n + img` (commutative)
#' * Subtraction: `img - img` or `img - n` (but not `n - img`)
#' * Multiplication: `img * img` or `img * n` or `n * img` (commutative)
#' * Division: `img / img` or `img / n` (but not `n / img`)
#'
#' @examples
#' # Create test images
#' img1 <- magick::image_blank(100, 100, "rgb(100,150,200)")
#' img2 <- magick::image_blank(100, 100, "rgb(200,100,50)")
#'
#' # Image-image operations
#' sum_img <- img1 + img2
#' diff_img <- img1 - img2
#' prod_img <- img1 * img2
#' quot_img <- img1 / img2
#'
#' # Image-number operations
#' bright <- img1 + 50     # Add 50 to all channels
#' dark <- img1 - 30       # Subtract 30 from all channels
#' double <- img1 * 2      # Double all values
#' half <- img1 / 2        # Halve all values
#'
#' # Commutative operations
#' also_bright <- 50 + img1  # Same as img1 + 50
#' also_double <- 2 * img1   # Same as img1 * 2
#'
#' @name magick-arithmetic
NULL

# Helper Functions ----------------------------------------

#' Process a single channel with an operation
#'
#' @param channel_data Integer array of channel data
#' @param value Second operand (integer array or numeric)
#' @param op Function implementing the operation
#' @return Raw array with operation result
process_channel <- function(channel_data, value, op) {
  result <- op(channel_data, value)
  as.raw(result %% 256)
}

#' Validate image dimensions match
#'
#' @param dim1,dim2 Dimension vectors to compare
#' @return TRUE if valid, stops with error if not
validate_dimensions <- function(dim1, dim2) {
  if (!identical(dim1, dim2)) {
    stop("Images must have same dimensions")
  }
  invisible(TRUE)
}

#' Process image-image operation
#'
#' @param img1,img2 Magick image objects
#' @param op Operation function
#' @return New magick image object
process_image_operation <- function(img1, img2, op) {
  data1 <- magick::image_data(img1)
  data2 <- magick::image_data(img2)

  validate_dimensions(dim(data1), dim(data2))

  result_raw <- array(raw(0), dim = dim(data1))

  for (channel in 1:3) {
    channel_data1 <- as.integer(data1[channel,,])
    channel_data2 <- as.integer(data2[channel,,])
    result_raw[channel,,] <- process_channel(channel_data1, channel_data2, op)
  }

  magick::image_read(result_raw)
}

#' Process image-numeric operation
#'
#' @param img Magick image object
#' @param num Numeric value
#' @param op Operation function
#' @return New magick image object
process_numeric_operation <- function(img, num, op) {
  if (is.nan(num) || is.infinite(num)) {
    stop("Numeric operand must be finite")
  }

  data <- magick::image_data(img)
  result_raw <- array(raw(0), dim = dim(data))

  for (channel in 1:3) {
    channel_data <- as.integer(data[channel,,])
    result_raw[channel,,] <- process_channel(channel_data, num, op)
  }

  magick::image_read(result_raw)
}

# Operator Implementations ----------------------------------------

#' @rdname magick-arithmetic
#' @export
`+.magick-image` <- function(e1, e2) {
  # Case 1: number + image or image + number
  if (is.numeric(e1) && inherits(e2, "magick-image") ||
      inherits(e1, "magick-image") && is.numeric(e2)) {
    img <- if(is.numeric(e1)) e2 else e1
    num <- if(is.numeric(e1)) e1 else e2
    return(process_numeric_operation(img, num, `+`))
  }

  # Case 2: image + image
  if (!inherits(e1, "magick-image") || !inherits(e2, "magick-image")) {
    stop("Both operands must be magick-image objects or one must be numeric")
  }

  process_image_operation(e1, e2, `+`)
}

#' @rdname magick-arithmetic
#' @export
`-.magick-image` <- function(e1, e2) {
  # Case 1: image - number
  if (is.numeric(e2)) {
    return(process_numeric_operation(e1, e2, `-`))
  }

  # Case 2: image - image
  if (!inherits(e2, "magick-image")) {
    stop("Second operand must be a magick-image object or numeric value")
  }

  process_image_operation(e1, e2, `-`)
}

#' @rdname magick-arithmetic
#' @export
`*.magick-image` <- function(e1, e2) {
  # Case 1: number * image or image * number
  if (is.numeric(e1) && inherits(e2, "magick-image") ||
      inherits(e1, "magick-image") && is.numeric(e2)) {
    img <- if(is.numeric(e1)) e2 else e1
    num <- if(is.numeric(e1)) e1 else e2
    return(process_numeric_operation(img, num, `*`))
  }

  # Case 2: image * image
  if (!inherits(e1, "magick-image") || !inherits(e2, "magick-image")) {
    stop("Both operands must be magick-image objects or one must be numeric")
  }

  process_image_operation(e1, e2, `*`)
}

#' @rdname magick-arithmetic
#' @export
`/.magick-image` <- function(e1, e2) {
  # Case 1: image / number
  if (is.numeric(e2)) {
    if (e2 == 0) stop("Division by zero")
    return(process_numeric_operation(e1, e2, `/`))
  }

  # Case 2: image / image
  if (!inherits(e2, "magick-image")) {
    stop("Second operand must be a magick-image object or numeric value")
  }

  # Custom division operation to handle division by zero
  safe_division <- function(x, y) {
    y[y == 0] <- 1  # Prevent division by zero
    x / y
  }

  process_image_operation(e1, e2, safe_division)
}
