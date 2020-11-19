library(amt)
data(amt_fisher)
set.seed(123)

mini_fisher <- amt_fisher[1:50, ]
mcp <- hr_mcp(mini_fisher)
loc <- hr_locoh(mini_fisher)
kde <- hr_kde(mini_fisher)

m <- fit_ctmm(mini_fisher, "iid")
rd <- hr_akde(mini_fisher, model = m)
od <- hr_od(mini_fisher, model = m)

# MCP and KDE have the right class

expect_true(is(mcp, "mcp"))
expect_true(is(mcp, "hr"))
expect_true(is(kde, "kde"))
expect_true(is(kde, "hr"))
expect_true(is(loc, "locoh"))
expect_true(is(loc, "hr"))
expect_true(is(rd, "akde"))
expect_true(is(rd, "hr"))

# More tests for mcp
tinytest::expect_error(hr_mcp(mini_fisher, keep.data = c(TRUE, FALSE)))
tinytest::expect_error(hr_mcp(mini_fisher, keep.data = mini_fisher))
tinytest::expect_true(is(mcp$data, "track_xy"))
tinytest::expect_true(is.null(hr_mcp(mini_fisher, keep.data = FALSE)$data))

# Test home-range model

# keep.data
tinytest::expect_true(is(mcp$data, "track_xyt"))
tinytest::expect_true(is(loc$data, "track_xyt"))
tinytest::expect_true(is(kde$data, "track_xyt"))
tinytest::expect_true(is(rd$data, "track_xyt"))
tinytest::expect_true(is(od$data, "track_xyt"))

# levels
tinytest::expect_true(mcp$levels == 0.95)
tinytest::expect_true(loc$levels == 0.95)
tinytest::expect_true(kde$levels == 0.95)
tinytest::expect_true(rd$levels == 0.95)
tinytest::expect_true(od$levels == 0.95)

# CRS
tinytest::expect_true(identical(mcp$crs, get_crs(mini_fisher)))
tinytest::expect_true(identical(loc$crs, get_crs(mini_fisher)))
tinytest::expect_true(identical(kde$crs, get_crs(mini_fisher)))
tinytest::expect_true(identical(od$crs, get_crs(mini_fisher)))
tinytest::expect_true(identical(rd$crs, get_crs(mini_fisher)))

# hr_area
tinytest::expect_true(is(hr_area(mcp), "tbl_df"))
tinytest::expect_true(is(hr_area(loc), "tbl_df"))
tinytest::expect_true(is(hr_area(kde), "tbl_df"))
tinytest::expect_true(is(hr_area(rd), "tbl_df"))
tinytest::expect_true(is(hr_area(od), "tbl_df"))

# hr_isopleth
tinytest::expect_true(is(hr_isopleths(mcp), "sf"))
tinytest::expect_true(is(hr_isopleths(loc), "sf"))
tinytest::expect_true(is(hr_isopleths(kde), "sf"))
tinytest::expect_true(is(hr_isopleths(od), "sf"))
tinytest::expect_true(is(hr_isopleths(rd), "sf"))

# hr_overlap
tinytest::expect_equal(hr_overlap(mcp, mcp), 1)
tinytest::expect_equal(hr_overlap(loc, loc), 1)
tinytest::expect_equal(hr_overlap(kde, kde), 1)
tinytest::expect_equal(hr_overlap(od, od), 1)
tinytest::expect_equal(hr_overlap(od, od), 1)

tinytest::expect_true(all(names(hr_area(rd)) == c("level", "what", "area")))
tinytest::expect_true(all(names(hr_area(loc)) == c("level", "what", "area")))
tinytest::expect_true(all(names(hr_area(mcp)) == c("level", "what", "area")))
tinytest::expect_true(all(names(hr_area(kde)) == c("level", "what", "area")))
tinytest::expect_true(all(names(hr_isopleths(rd)) == c("level", "what", "area", "geometry")))
tinytest::expect_true(all(names(hr_isopleths(loc)) == c("level", "what", "area", "geometry")))
tinytest::expect_true(all(names(hr_isopleths(mcp)) == c("level", "what", "area", "geometry")))
tinytest::expect_true(all(names(hr_isopleths(kde)) == c("level", "what", "area", "geometry")))

hr_isopleths(od)
class(od)
