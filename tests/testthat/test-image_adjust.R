test_that("image_adjust handles basic adjustments correctly", {
  test_img <- magick::image_blank(100, 100, "white")

  result <- image_adjust(test_img, 1:50, 1:50, 100, channels = 1)
  expect_s3_class(result, "magick-image")

  expect_equal(magick::image_info(result)$width, 100)
  expect_equal(magick::image_info(result)$height, 100)
})

test_that("image_adjust handles clamping correctly", {
  test_img <- magick::image_blank(100, 100, "white")

  result_high <- image_adjust(test_img, 1:10, 1:10, 300, clamp = TRUE)
  pixels_high <- as.integer(magick::image_data(result_high)[1:3, 1:10, 1:10])
  expect_true(all(pixels_high <= 255))

  result_low <- image_adjust(test_img, 1:10, 1:10, -300, clamp = TRUE)
  pixels_low <- as.integer(magick::image_data(result_low)[1:3, 1:10, 1:10])
  expect_true(all(pixels_low >= 0))
})

test_that("image_adjust handles cycling correctly", {
  white_img <- magick::image_blank(100, 100, "white")
  result_white <- image_adjust(white_img, 1:10, 1:10, 257)
  pixels_white <- as.integer(magick::image_data(result_white)[1:3, 1:10, 1:10])
  expect_equal(pixels_white[1], (255 + 257) %% 256)

  black_img <- magick::image_blank(100, 100, "black")

  result1 <- image_adjust(black_img, 1:10, 1:10, 257)
  pixels1 <- as.integer(magick::image_data(result1)[1:3, 1:10, 1:10])
  expect_equal(pixels1[1], 1)

  result2 <- image_adjust(black_img, 1:10, 1:10, 258)
  pixels2 <- as.integer(magick::image_data(result2)[1:3, 1:10, 1:10])
  expect_equal(pixels2[1], 2)

  result3 <- image_adjust(black_img, 1:10, 1:10, -2)
  pixels3 <- as.integer(magick::image_data(result3)[1:3, 1:10, 1:10])
  expect_equal(pixels3[1], 254)
})

test_that("image_adjust validates inputs correctly", {
  test_img <- magick::image_blank(100, 100, "white")

  expect_error(image_adjust("not_an_image", 1:10, 1:10, 100),
               "Input must be a magick image object")

  expect_error(image_adjust(test_img, 1:10, 1:10, 100, channels = 4),
               "Channels must be values between 1 and 3")

  expect_error(image_adjust(test_img, 1:101, 1:10, 100),
               "x_range exceeds image width")
  expect_error(image_adjust(test_img, 1:10, 1:101, 100),
               "y_range exceeds image height")

  expect_error(image_adjust(test_img, 0:10, 1:10, 100),
               "x_range must be >= 1")
  expect_error(image_adjust(test_img, 1:10, 0:10, 100),
               "y_range must be >= 1")

  expect_error(image_adjust(test_img, 1:10, 1:10, "100"),
               "Value must be numeric")
})

test_that("image_adjust handles different channel selections", {
  test_img <- magick::image_blank(100, 100, "white")

  result_single <- image_adjust(test_img, 1:50, 1:50, 100, channels = 1)
  expect_s3_class(result_single, "magick-image")

  result_double <- image_adjust(test_img, 1:50, 1:50, 100, channels = c(1,2))
  expect_s3_class(result_double, "magick-image")

  result_all <- image_adjust(test_img, 1:50, 1:50, 100, channels = 1:3)
  expect_s3_class(result_all, "magick-image")
})
