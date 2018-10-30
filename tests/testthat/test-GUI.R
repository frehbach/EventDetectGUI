library(testthat)
library(shinytest)
library(EventDetectGUI)

context("Shiny GUI Tests")

test_that("Application works", {
    skip_on_appveyor()
    expect_error(testApp(system.file("appTests",package = "EventDetectGUI")), regexp = NA)
})
