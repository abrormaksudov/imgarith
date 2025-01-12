# Create standard test images
create_test_images <- function() {
  list(
    black = magick::image_blank(10, 10, "black"),     # All zeros
    white = magick::image_blank(10, 10, "white"),     # All 255s
    grey = magick::image_blank(10, 10, "grey50"),     # All 128s
    red = magick::image_blank(10, 10, "red"),         # R=255, G=0, B=0
    custom = magick::image_blank(10, 10, "rgb(100,150,200)")  # Custom values
  )
}

# Helper to get raw pixel values for a specific channel
get_channel_values <- function(img, channel) {
  data <- magick::image_data(img)
  as.integer(data[channel,,])
}

# Helper to check if all pixels in a channel have a specific value
# Enhanced helper function to actually show us the values
check_channel_value <- function(img, channel, expected_value) {
  values <- get_channel_values(img, channel)
  unique_values <- unique(as.vector(values))
  if (!all(values == expected_value)) {
    message(sprintf("Channel %d contains values: %s (expected %d)",
                    channel,
                    paste(unique_values, collapse=", "),
                    expected_value))
  }
  all(values == expected_value)
}
