## The NFDB polygon download goes through reproducible::prepInputs(), so reproducible caches it and
## records it in the download ledger. The network is mocked: prepInputs() returns the unzipped names.

test_that("the NFDB polygons are fetched with prepInputs()", {
  dst <- withr::local_tempdir()
  seen <- list()
  testthat::local_mocked_bindings(
    prepInputs = function(url, destinationPath, fun, ...) {
      seen <<- list(url = url, destinationPath = destinationPath, fun = fun)
      file.path(destinationPath, c("NFDB_poly.zip", "NFDB_poly_1972to2020_20250630.shp",
                                   "NFDB_poly_1972to2020_20250630.dbf",
                                   "NFDB_poly_2021to2024_20250630.shp",
                                   "NFDB_poly_update_log.txt"))
    },
    .package = "reproducible")
  shp <- nfdbShapefiles(dst)
  expect_equal(basename(shp), c("NFDB_poly_1972to2020_20250630.shp", "NFDB_poly_2021to2024_20250630.shp"))
  expect_match(seen$url, "NFDB_poly[.]zip$")
  expect_equal(seen$destinationPath, dst)
  expect_true(is.na(seen$fun))
})
