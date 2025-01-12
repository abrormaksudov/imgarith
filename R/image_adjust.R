#' Adjust pixel values in an image region
#'
#' @description
#' Modifies pixel values in a specified rectangular region of an image by adding a value.
#' The resulting values can either wrap around cyclically within 0-255 range or be clamped
#' to these bounds.
#'
#' @param img Magick image object
#' @param x_range Numeric vector specifying range of x coordinates (width)
#' @param y_range Numeric vector specifying range of y coordinates (height)
#' @param value Numeric value to add to pixel values (can be negative)
#' @param channels Numeric vector specifying color channels to modify:
#'        1 for Red, 2 for Green, 3 for Blue.
#'        Defaults to c(1,2,3) for all channels.
#' @param clamp Logical, if TRUE values exceeding 255 or below 0 will be clamped
#'        to those bounds. If FALSE (default), values wrap around cyclically.
#'
#' @return Modified magick image object
#'
#' @details
#' The function modifies pixel values according to the clamp parameter:
#'
#' When clamp = FALSE (default):
#' * Values wrap around in the range 0-255 using modular arithmetic
#' * Adding 10 to 250 gives 4 (as 260 %% 256 = 4)
#' * Subtracting 10 from 5 gives 251 (as -5 %% 256 = 251)
#'
#' When clamp = TRUE:
#' * Values exceeding 255 are set to 255
#' * Values below 0 are set to 0
#'
#' Image dimensions and coordinates use the following system:
#' * Width (x) starts from 1 at the left edge
#' * Height (y) starts from 1 at the top edge
#' * Channels are: 1=Red, 2=Green, 3=Blue
#'
#' @examples
#' # Create a test image
#' img <- magick::image_blank(100, 100, "white")
#'
#' # Make upper left quarter more red by adding 100 (with wraparound)
#' result1 <- image_adjust(img, 1:50, 1:50, 100, channels = 1)
#'
#' # Subtract 50 from all channels in center region (with clamping)
#' result2 <- image_adjust(img, 25:75, 25:75, -50, clamp = TRUE)
#'
#' # Modify green channel only with clamping
#' result3 <- image_adjust(img, 1:100, 1:100, 200, channels = 2, clamp = TRUE)
#'
#' @export
image_adjust <- function(img, x_range, y_range, value, channels = 1:3, clamp = FALSE) {
  if (!inherits(img, "magick-image")) stop("Input must be a magick image object")
  if (!is.numeric(value)) stop("Value must be numeric")
  if (!all(channels %in% 1:3)) stop("Channels must be values between 1 and 3")

  data <- magick::image_data(img)

  if (max(x_range) > dim(data)[2]) stop("x_range exceeds image width")
  if (max(y_range) > dim(data)[3]) stop("y_range exceeds image height")
  if (min(x_range) < 1) stop("x_range must be >= 1")
  if (min(y_range) < 1) stop("y_range must be >= 1")

  region <- as.integer(data[channels, x_range, y_range])
  modified <- region + value

  if (clamp) {
    modified <- pmin(pmax(modified, 0), 255)
  } else {
    modified <- modified %% 256
  }

  data[channels, x_range, y_range] <- as.raw(modified)
  magick::image_read(data)
}
