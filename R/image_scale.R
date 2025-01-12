#' Scale image by integer factor
#'
#' @description
#' Scales an image by repeating each pixel specified number of times.
#' Can scale width, height or both dimensions. Uses nearest-neighbor
#' interpolation, making it ideal for pixel art or when maintaining
#' sharp edges is desired.
#'
#' @param img A magick image object
#' @param factor Positive integer specifying scaling factor
#' @param how Character string specifying scaling direction:
#'        "both" scales both dimensions,
#'        "width" scales only width,
#'        "height" scales only height
#'
#' @return A scaled magick image object with dimensions modified according
#'         to the scaling factor and direction
#'
#' @details
#' The function uses pixel replication for scaling, which means each pixel
#' is repeated factor times in the specified direction(s). This preserves
#' sharp edges and is particularly useful for:
#' * Pixel art upscaling
#' * Creating blocky/pixelated effects
#' * Maintaining crisp edges in icons or sprites
#'
#' @examples
#' img <- image_blank(100, 100, "red")
#'
#' # Scale both dimensions by 2 (resulting size: 200x200)
#' scaled <- image_scale(img, factor = 2, how = "both")
#'
#' # Scale only width by 3 (resulting size: 300x100)
#' wide <- image_scale(img, factor = 3, how = "width")
#'
#' # Scale only height by 2 (resulting size: 100x200)
#' tall <- image_scale(img, factor = 2, how = "height")
#'
#' @export
image_scale <- function(img, factor = 2, how = "both") {
  if (!inherits(img, "magick-image"))
    stop("Input must be a magick image object")
  if (!is.numeric(factor))
    stop("'factor' must be numeric")
  if (!factor == round(factor))
    stop("'factor' must be a whole number")
  if (factor <= 0)
    stop("'factor' must be positive")
  if (!how %in% c("both", "width", "height"))
    stop("'how' must be one of: 'both', 'width', 'height'")

  data <- image_data(img)
  new_width <- if(how %in% c("both", "width")) dim(data)[2] * factor else dim(data)[2]
  new_height <- if(how %in% c("both", "height")) dim(data)[3] * factor else dim(data)[3]

  scaled_data <- array(
    raw(3 * new_width * new_height),
    dim = c(3, new_width, new_height)
  )

  for(ch in 1:3) {
    channel <- data[ch,,]
    scaled_data[ch,,] <- scale_channel(channel, factor, how)
  }

  magick::image_read(scaled_data)
}

scale_channel <- function(channel, factor, how) {
  if(how %in% c("both", "width")) {
    channel <- matrix(
      as.raw(as.vector(t(apply(channel, 1, function(row) rep(row, each = factor))))),
      nrow = dim(channel)[1]
    )
  }

  if(how %in% c("both", "height")) {
    channel <- matrix(
      as.raw(rep(channel, each = factor)),
      ncol = ncol(channel)
    )
  }

  channel
}
