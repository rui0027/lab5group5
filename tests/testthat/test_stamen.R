library(ggmap)
test_that("Wrong zoom level, zoom_l hsould be [9,12]", {
  expect_error(findmap("stockholm",8,'toner'))
  expect_error(findmap("stockholm",13,'toner'))
})

test_that("Can not find the map type,try (terrain, toner or watercolor)", {
  expect_error(findmap("stockholm",10,'watermap'))
  expect_error(findmap("stockholm",10,'weathermap'))
})

test_that("Missing parameter", {
  expect_error(findmap("stockholm",'toner'))
})

test_that("Zoom level is greater than 8", {
  a = findmap("stockholm",11,'toner')
  expect_true(a$zoom_l > 9)
})

test_that("Zoom level is less than 13", {
  a = findmap("stockholm",11,'toner')
  expect_true(a$zoom_l < 13)
})

test_that("Zoom level is numeric", {
  a = findmap("stockholm",11,'toner')
  expect_true(is.numeric(a$zoom_l))
})

test_that("name is character", {
  a = findmap("stockholm",11,'toner')
  expect_true(is.character(a$name))
})

test_that("map_type is character", {
  a = findmap("stockholm",11,'toner')
  expect_true(is.character(a$map_type))
})

test_that("zoom_l is numeric", {
  b = findmap("stockholm",11,'toner')
  expect_equal(b$zoom_l,as.numeric(11))
})


test_that("return a list", {
  b = findmap("stockholm",11,'toner')
  expect_type(b,"list")
})
