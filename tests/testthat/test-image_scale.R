test_that("image_scale handles basic scaling correctly", {
  test_img <- magick::image_blank(100, 100, "red")

  result_both <- image_scale(test_img, factor = 2, how = "both")
  info_both <- magick::image_info(result_both)
  expect_equal(info_both$width, 200)
  expect_equal(info_both$height, 200)

  result_width <- image_scale(test_img, factor = 2, how = "width")
  info_width <- magick::image_info(result_width)
  expect_equal(info_width$width, 200)
  expect_equal(info_width$height, 100)

  result_height <- image_scale(test_img, factor = 2, how = "height")
  info_height <- magick::image_info(result_height)
  expect_equal(info_height$width, 100)
  expect_equal(info_height$height, 200)
})

test_that("image_scale maintains pixel values correctly", {
  test_img <- magick::image_blank(2, 2, "red")

  result <- image_scale(test_img, factor = 2, how = "both")
  pixels <- as.integer(magick::image_data(result)[1, 1:4, 1:4])

  original_pixels <- as.integer(magick::image_data(test_img)[1, 1:2, 1:2])
  expect_true(all(pixels == original_pixels[1]))
})

test_that("image_scale validates inputs correctly", {
  test_img <- magick::image_blank(100, 100, "red")

  expect_error(image_scale("not_an_image", factor = 2),
               "Input must be a magick image object")

  expect_error(image_scale(test_img, factor = 0),
               "'factor' must be positive")
  expect_error(image_scale(test_img, factor = -1),
               "'factor' must be positive")
  expect_error(image_scale(test_img, factor = 1.5),
               "'factor' must be a whole number")
  expect_error(image_scale(test_img, factor = "2"),
               "'factor' must be numeric")

  expect_error(image_scale(test_img, factor = 2, how = "invalid"),
               "'how' must be one of: 'both', 'width', 'height'")
})

test_that("image_scale works with different factor values", {
  test_img <- magick::image_blank(50, 50, "red")

  result1 <- image_scale(test_img, factor = 1, how = "both")
  info1 <- magick::image_info(result1)
  expect_equal(info1$width, 50)
  expect_equal(info1$height, 50)

  result3 <- image_scale(test_img, factor = 3, how = "both")
  info3 <- magick::image_info(result3)
  expect_equal(info3$width, 150)
  expect_equal(info3$height, 150)
})

test_that("image_scale preserves image channels", {
  test_img <- magick::image_blank(2, 2, "blue")
  result <- image_scale(test_img, factor = 2, how = "both")

  expect_equal(dim(magick::image_data(result))[1], 3)

  channels <- magick::image_data(result)
  blue_channel <- as.integer(channels[3, 1, 1])
  red_channel <- as.integer(channels[1, 1, 1])
  green_channel <- as.integer(channels[2, 1, 1])

  expect_true(blue_channel > red_channel)
  expect_true(blue_channel > green_channel)
})
