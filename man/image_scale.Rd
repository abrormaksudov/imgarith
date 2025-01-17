% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/image_scale.R
\name{image_scale}
\alias{image_scale}
\title{Scale image by integer factor}
\usage{
image_scale(img, factor = 2, how = "both")
}
\arguments{
\item{img}{A magick image object}

\item{factor}{Positive integer specifying scaling factor}

\item{how}{Character string specifying scaling direction:
"both" scales both dimensions,
"width" scales only width,
"height" scales only height}
}
\value{
A scaled magick image object with dimensions modified according
to the scaling factor and direction
}
\description{
Scales an image by repeating each pixel specified number of times.
Can scale width, height or both dimensions. Uses nearest-neighbor
interpolation, making it ideal for pixel art or when maintaining
sharp edges is desired.
}
\details{
The function uses pixel replication for scaling, which means each pixel
is repeated factor times in the specified direction(s). This preserves
sharp edges and is particularly useful for:
\itemize{
\item Pixel art upscaling
\item Creating blocky/pixelated effects
\item Maintaining crisp edges in icons or sprites
}
}
\examples{
img <- magick::image_blank(100, 100, "red")

# Scale both dimensions by 2 (resulting size: 200x200)
scaled <- image_scale(img, factor = 2, how = "both")

# Scale only width by 3 (resulting size: 300x100)
wide <- image_scale(img, factor = 3, how = "width")

# Scale only height by 2 (resulting size: 100x200)
tall <- image_scale(img, factor = 2, how = "height")

}
