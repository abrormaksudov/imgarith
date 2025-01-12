# GENERAL -----------------------
test_that("airthmetic properties hold correctly", {
  imgs <- create_test_images()

  e1 <- imgs$black * 2
  e2 <- 2 * imgs$black
  e3 <- imgs$black + imgs$black
  e4 <- imgs$black + imgs$black - imgs$black + imgs$black
  e5 <- 2 * imgs$black + imgs$black - imgs$black * 5 + 4 * imgs$black

  expect_equal(image_data(e1), image_data(e2))
  expect_equal(image_data(e1), image_data(e3))
  expect_equal(image_data(e1), image_data(e4))
  expect_equal(image_data(e2), image_data(e5))

  e1 <- 5 * imgs$custom * 5
  e2 <- 25 * imgs$custom
  e3 <- imgs$custom * 25

  expect_equal(image_data(e1), image_data(e2))
})


# ADDITION -----------------------
test_that("basic color addition works correctly", {
  imgs <- create_test_images()

  result <- imgs$black + imgs$black
  expect_true(check_channel_value(result, 1, 0))
  expect_true(check_channel_value(result, 2, 0))
  expect_true(check_channel_value(result, 3, 0))

  result <- imgs$grey + imgs$grey
  expect_true(check_channel_value(result, 1, 254))
  expect_true(check_channel_value(result, 2, 254))
  expect_true(check_channel_value(result, 3, 254))

  result <- imgs$red + imgs$black
  expect_true(check_channel_value(result, 1, 255))
  expect_true(check_channel_value(result, 2, 0))
  expect_true(check_channel_value(result, 3, 0))
})

test_that("numeric addition works correctly", {
  imgs <- create_test_images()

  # adding to custom color (100,150,200)
  result <- imgs$custom + 50
  expect_true(check_channel_value(result, 1, 150))  # 100+50
  expect_true(check_channel_value(result, 2, 200))  # 150+50
  expect_true(check_channel_value(result, 3, 250))  # 200+50

  # adding negative numbers
  result <- imgs$custom + (-50)
  expect_true(check_channel_value(result, 1, 50))   # 100-50
  expect_true(check_channel_value(result, 2, 100))  # 150-50
  expect_true(check_channel_value(result, 3, 150))  # 200-50

  # adding zero makes no change
  result <- imgs$custom + 0
  expect_equal(
    magick::image_data(result),
    magick::image_data(imgs$custom)
  )
})

test_that("wraparound behavior works correctly", {
  imgs <- create_test_images()
  custom_data <- magick::image_data(imgs$custom)
  result <- imgs$custom + 255

  expect_true(check_channel_value(result, 1, 99))
  expect_true(check_channel_value(result, 2, 149))
  expect_true(check_channel_value(result, 3, 199))

  result <- imgs$white + 1
  expect_true(check_channel_value(result, 1, 0))
  expect_true(check_channel_value(result, 2, 0))
  expect_true(check_channel_value(result, 3, 0))

  result <- imgs$white + 254
  expect_true(check_channel_value(result, 1, 253))
  expect_true(check_channel_value(result, 2, 253))
  expect_true(check_channel_value(result, 3, 253))
})

test_that("commutative property holds", {
  imgs <- create_test_images()

  # image + number = number + image
  result1 <- imgs$custom + 50
  result2 <- 50 + imgs$custom
  expect_equal(
    magick::image_data(result1),
    magick::image_data(result2)
  )

  # image + image is commutative
  result1 <- imgs$red + imgs$custom
  result2 <- imgs$custom + imgs$red
  expect_equal(
    magick::image_data(result1),
    magick::image_data(result2)
  )
})

test_that("error conditions are handled correctly", {
  imgs <- create_test_images()

  expect_error(imgs$black + "not an image")
  expect_error(imgs$black + NA)
  expect_error(imgs$black + NULL)
  expect_error(imgs$black + list())

  expect_error(imgs$black + Inf)
  expect_error(imgs$black + NaN)

  big_img <- magick::image_blank(20, 20, "black")
  expect_error(imgs$black + big_img)
  small_img <- magick::image_blank(5, 5, "black")
  expect_error(imgs$black + small_img)
})

test_that("large number addition works correctly", {
  imgs <- create_test_images()

  result <- imgs$black + 1000
  expect_true(check_channel_value(result, 1, 232))  # 1000 %% 256 = 232

  result <- imgs$black + 1000000
  expect_true(check_channel_value(result, 1, 64))   # 1000000 %% 256 = 64
})


# SUBTRACTION -----------------------
test_that("basic color subtraction works correctly", {
  imgs <- create_test_images()

  result <- imgs$black - imgs$black
  expect_true(check_channel_value(result, 1, 0))
  expect_true(check_channel_value(result, 2, 0))
  expect_true(check_channel_value(result, 3, 0))

  result <- imgs$white - imgs$white
  expect_true(check_channel_value(result, 1, 0))
  expect_true(check_channel_value(result, 2, 0))
  expect_true(check_channel_value(result, 3, 0))

  result <- imgs$red - imgs$black
  expect_true(check_channel_value(result, 1, 255))
  expect_true(check_channel_value(result, 2, 0))
  expect_true(check_channel_value(result, 3, 0))
})

test_that("numeric subtraction works correctly", {
  imgs <- create_test_images()

  # subtracting from custom color (100,150,200)
  result <- imgs$custom - 50
  expect_true(check_channel_value(result, 1, 50))   # 100-50
  expect_true(check_channel_value(result, 2, 100))  # 150-50
  expect_true(check_channel_value(result, 3, 150))  # 200-50

  result <- imgs$custom - 0
  expect_equal(
    magick::image_data(result),
    magick::image_data(imgs$custom)
  )
})

test_that("wraparound behavior in subtraction works correctly", {
  imgs <- create_test_images()

  result <- imgs$black - 1
  expect_true(check_channel_value(result, 1, 255))
  expect_true(check_channel_value(result, 2, 255))
  expect_true(check_channel_value(result, 3, 255))

  result <- imgs$custom - 150  # (100,150,200)
  expect_true(check_channel_value(result, 1, 206))  # (100 - 150) %% 256 = 206
  expect_true(check_channel_value(result, 2, 0))    # (150 - 150) %% 256 = 0
  expect_true(check_channel_value(result, 3, 50))   # (200 - 150) %% 256 = 50
})

test_that("error conditions for subtraction are handled correctly", {
  imgs <- create_test_images()

  expect_error(imgs$black - "not an image")
  expect_error(imgs$black - NA)
  expect_error(imgs$black - NULL)
  expect_error(imgs$black - list())

  expect_error(imgs$black - Inf)
  expect_error(imgs$black - NaN)

  big_img <- magick::image_blank(20, 20, "black")
  expect_error(imgs$black - big_img)
  small_img <- magick::image_blank(5, 5, "black")
  expect_error(imgs$black - small_img)
})

test_that("large number subtraction works correctly", {
  imgs <- create_test_images()

  result <- imgs$white - 1000
  expect_true(check_channel_value(result, 1, 23))

  result <- imgs$white - 1000000
  expect_true(check_channel_value(result, 1, 191))
})


# MULTIPLICATION -----------------------
test_that("basic color multiplication works correctly", {
  imgs <- create_test_images()

  result <- imgs$black * imgs$black
  expect_true(check_channel_value(result, 1, 0))
  expect_true(check_channel_value(result, 2, 0))
  expect_true(check_channel_value(result, 3, 0))

  result <- imgs$white * imgs$white  # 255 * 255 = 65025 % 256 = 1
  expect_true(check_channel_value(result, 1, 1))
  expect_true(check_channel_value(result, 2, 1))
  expect_true(check_channel_value(result, 3, 1))

  result <- imgs$custom * imgs$black
  expect_true(check_channel_value(result, 1, 0))
  expect_true(check_channel_value(result, 2, 0))
  expect_true(check_channel_value(result, 3, 0))
})

test_that("numeric multiplication works correctly", {
  imgs <- create_test_images()

  # multiplying custom color (100,150,200) by 2
  result <- imgs$custom * 2
  expect_true(check_channel_value(result, 1, 200))  # 100*2
  expect_true(check_channel_value(result, 2, 44))   # 150*2 = 300 % 256 = 44
  expect_true(check_channel_value(result, 3, 144))  # 200*2 = 400 % 256 = 144

  result <- imgs$custom * 1
  expect_equal(
    magick::image_data(result),
    magick::image_data(imgs$custom)
  )

  result <- imgs$custom * 0
  expect_true(check_channel_value(result, 1, 0))
  expect_true(check_channel_value(result, 2, 0))
  expect_true(check_channel_value(result, 3, 0))
})

test_that("wraparound behavior in multiplication works correctly", {
  imgs <- create_test_images()
  custom_data <- magick::image_data(imgs$custom)
  result <- imgs$custom * 256

  expect_true(check_channel_value(result, 1, 0))
  expect_true(check_channel_value(result, 2, 0))
  expect_true(check_channel_value(result, 3, 0))

  result <- imgs$grey * imgs$grey
  expected <- (127 * 127) %% 256
  expect_true(check_channel_value(result, 1, expected))
  expect_true(check_channel_value(result, 2, expected))
  expect_true(check_channel_value(result, 3, expected))

  result <- imgs$custom * 257
  expect_true(check_channel_value(result, 1, 100))  # 100 * 257 % 256 = 100
  expect_true(check_channel_value(result, 2, 150))  # 150 * 257 % 256 = 150
  expect_true(check_channel_value(result, 3, 200))  # 200 * 257 % 256 = 200
})

test_that("multiplication is commutative", {
  imgs <- create_test_images()

  # number * image = image * number
  result1 <- imgs$custom * 3
  result2 <- 3 * imgs$custom
  expect_equal(
    magick::image_data(result1),
    magick::image_data(result2)
  )

  # image1 * image2 = image2 * image1
  result1 <- imgs$grey * imgs$custom
  result2 <- imgs$custom * imgs$grey
  expect_equal(
    magick::image_data(result1),
    magick::image_data(result2)
  )
})

test_that("error conditions for multiplication are handled correctly", {
  imgs <- create_test_images()

  expect_error(imgs$black * "not an image")
  expect_error(imgs$black * NA)
  expect_error(imgs$black * NULL)
  expect_error(imgs$black * list())

  expect_error(imgs$black * Inf)
  expect_error(imgs$black * NaN)

  big_img <- magick::image_blank(20, 20, "black")
  expect_error(imgs$black * big_img)
  small_img <- magick::image_blank(5, 5, "black")
  expect_error(imgs$black * small_img)
})

test_that("large number multiplication works correctly", {
  imgs <- create_test_images()
  result <- imgs$custom * 1000
  custom_data <- magick::image_data(imgs$custom)

  expect_true(check_channel_value(result, 1, 160))
  expect_true(check_channel_value(result, 2, 240))
  expect_true(check_channel_value(result, 3, 64))

  result <- imgs$custom * 10
  expect_true(check_channel_value(result, 1, 232))
  expect_true(check_channel_value(result, 2, 220))
  expect_true(check_channel_value(result, 3, 208))
})

test_that("fractional multiplication works correctly", {
  imgs <- create_test_images()

  result <- imgs$custom * 0.5
  expect_true(check_channel_value(result, 1, 50))   # 100 * 0.5 = 50
  expect_true(check_channel_value(result, 2, 75))   # 150 * 0.5 = 75
  expect_true(check_channel_value(result, 3, 100))  # 200 * 0.5 = 100
})

