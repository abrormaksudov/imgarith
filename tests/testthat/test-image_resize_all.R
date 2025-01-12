test_that("image_resize_all handles basic resizing with mean dimensions", {
  img1 <- magick::image_blank(100, 150, "red")
  img2 <- magick::image_blank(200, 250, "blue")

  result <- image_resize_all(img1, img2)
  info <- magick::image_info(result)

  expect_true(all(info$width == 150))
  expect_true(all(info$height == 200))
  expect_equal(length(info$width), 2)
  expect_equal(length(info$height), 2)
})

test_that("image_resize_all handles resizing to match specific image", {
  img1 <- magick::image_blank(100, 150, "red")
  img2 <- magick::image_blank(200, 250, "blue")
  img3 <- magick::image_blank(300, 350, "green")

  result1 <- image_resize_all(img1, img2, img3, match_to = 1)
  info1 <- magick::image_info(result1)
  expect_true(all(info1$width == 100))
  expect_true(all(info1$height == 150))
  expect_equal(length(info1$width), 3)

  result2 <- image_resize_all(img1, img2, img3, match_to = 2)
  info2 <- magick::image_info(result2)
  expect_true(all(info2$width == 200))
  expect_true(all(info2$height == 250))
  expect_equal(length(info2$width), 3)
})

test_that("image_resize_all handles custom dimensions", {
  img1 <- magick::image_blank(100, 150, "red")
  img2 <- magick::image_blank(200, 250, "blue")

  result <- image_resize_all(img1, img2, width = 400, height = 300)
  info <- magick::image_info(result)

  expect_true(all(info$width == 400))
  expect_true(all(info$height == 300))
  expect_equal(length(info$width), 2)
})

test_that("image_resize_all preserves image count and dimensions", {
  img1 <- magick::image_blank(100, 150, "red")
  img2 <- magick::image_blank(200, 250, "blue")
  img3 <- magick::image_blank(300, 350, "green")

  result2 <- image_resize_all(img1, img2)
  result3 <- image_resize_all(img1, img2, img3)

  info2 <- magick::image_info(result2)
  info3 <- magick::image_info(result3)

  expect_equal(length(info2$width), 2)
  expect_equal(length(info3$width), 3)

  expect_true(length(unique(info2$width)) == 1)
  expect_true(length(unique(info2$height)) == 1)
  expect_true(length(unique(info3$width)) == 1)
  expect_true(length(unique(info3$height)) == 1)
})
