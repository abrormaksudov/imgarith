#' Resize Multiple Images to Matching Dimensions
#'
#' @description
#' Resizes multiple images to consistent dimensions. The target dimensions can be:
#' 1) mean dimensions of all input images
#' 2) dimensions of a specific image from the input set
#' 3) custom user-specified dimensions
#'
#' @param ... Two or more image objects (magick format) to be resized
#' @param match_to Either 'mean' (default) to use average dimensions of all images,
#'   or a positive integer N to match dimensions of the Nth image
#' @param width Optional. Target width in pixels (must be positive).
#'   If NULL, width is determined by match_to setting
#' @param height Optional. Target height in pixels (must be positive).
#'   If NULL, height is determined by match_to setting
#'
#' @return A vector of magick image objects, all resized to the same dimensions
#'
#' @details
#' The function enforces aspect ratio changes to ensure all images match exactly.
#' This means images may appear stretched or compressed if their original aspect
#' ratios differ significantly from the target dimensions. The '!' in the resize
#' specification forces exact dimensions without preserving aspect ratio.
#'
#' Negative dimensions are converted to their absolute values with a warning.
#' Invalid dimensions (zero or non-numeric) are ignored with a warning.
#'
#' @examples
#' \dontrun{
#' # Load two images
#' img1 <- magick::image_read("path/to/image1.jpg")
#' img2 <- magick::image_read("path/to/image2.jpg")
#'
#' # Resize to mean dimensions
#' resized <- image_resize_all(img1, img2)
#'
#' # Resize to match first image
#' resized <- image_resize_all(img1, img2, match_to = 1)
#'
#' # Resize to specific dimensions
#' resized <- image_resize_all(img1, img2, width = 800, height = 600)
#' }
#'
#' @export
image_resize_all <- function(..., match_to = 'mean', width = NULL, height = NULL) {
  images <- list(...)
  if (length(images) < 2) stop('At least two images are required')
  if (!all(vapply(images, inherits, logical(1), "magick-image"))) {
    stop("All inputs must be magick image objects")
  }
  if (any(vapply(images, is.null, logical(1)))) {
    stop("NULL images are not allowed")
  }

  process_dimension <- function(dim_value, dim_name) {
    if (is.null(dim_value)) return(NULL)
    if (!is.numeric(dim_value) || dim_value == 0) {
      warning(sprintf('Invalid %s provided. Using %s from match_to instead.',
                      dim_name, dim_name))
      return(NULL)
    }
    if (dim_value < 0) {
      warning(sprintf('Negative %s provided. Using absolute value instead.',
                      dim_name))
    }
    abs(dim_value)
  }

  width <- process_dimension(width, "width")
  height <- process_dimension(height, "height")

  if (is.numeric(match_to)) {
    if (match_to < 1 || match_to > length(images)) {
      stop(sprintf("match_to must be 'mean' or a number between 1 and %d", length(images)))
    }
  } else if (match_to != 'mean') {
    stop("match_to must be 'mean' or a positive integer")
  }

  info_list <- vapply(images, function(img) {
    info <- magick::image_info(img)
    c(width = info$width, height = info$height)
  }, numeric(2))

  dims_str <- sprintf("Image %d: %dx%d",
                      seq_len(length(images)),
                      info_list["width", ],
                      info_list["height", ])
  message('Initial dimensions:\n', paste(dims_str, collapse = "\n"))

  target_dims <- if (match_to == 'mean') {
    round(rowMeans(info_list))
  } else {
    info_list[, match_to]
  }
  final_width <- if(is.null(width)) target_dims["width"] else width
  final_height <- if(is.null(height)) target_dims["height"] else height

  message(sprintf(
    '\nResizing all images to dimensions: %dx%d (width: %s, height: %s)',
    final_width, final_height,
    if (is.null(width)) 'from match_to' else 'user-specified',
    if (is.null(height)) 'from match_to' else 'user-specified'
  ))

  resize_spec <- sprintf("%dx%d!", final_width, final_height)
  resized_images <- lapply(images, magick::image_resize, resize_spec)

  do.call(c, resized_images)
}
