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
expect_error(hr_mcp(mini_fisher, keep.data = c(TRUE, FALSE)))
expect_error(hr_mcp(mini_fisher, keep.data = mini_fisher))
expect_true(is(mcp$data, "track_xy"))
expect_true(is.null(hr_mcp(mini_fisher, keep.data = FALSE)$data))

# Test home-range model

# keep.data
expect_true(is(mcp$data, "track_xyt"))
expect_true(is(loc$data, "track_xyt"))
expect_true(is(kde$data, "track_xyt"))
expect_true(is(rd$data, "track_xyt"))
expect_true(is(od$data, "track_xyt"))

# levels
expect_true(mcp$levels == 0.95)
expect_true(loc$levels == 0.95)
expect_true(kde$levels == 0.95)
expect_true(rd$levels == 0.95)
expect_true(od$levels == 0.95)

# CRS
expect_true(identical(mcp$crs, get_crs(mini_fisher)))
expect_true(identical(loc$crs, get_crs(mini_fisher)))
expect_true(identical(kde$crs, get_crs(mini_fisher)))
expect_true(identical(od$crs, get_crs(mini_fisher)))
expect_true(identical(rd$crs, get_crs(mini_fisher)))

# hr_area
expect_true(is(hr_area(mcp), "tbl_df"))
expect_true(is(hr_area(loc), "tbl_df"))
expect_true(is(hr_area(kde), "tbl_df"))
expect_true(is(hr_area(rd), "tbl_df"))
expect_true(is(hr_area(od), "tbl_df"))

# hr_isopleth
expect_true(is(hr_isopleths(mcp), "sf"))
expect_true(is(hr_isopleths(loc), "sf"))
expect_true(is(hr_isopleths(kde), "sf"))
expect_true(is(hr_isopleths(od), "sf"))
expect_true(is(hr_isopleths(rd), "sf"))

expect_equal(hr_overlap(mcp, mcp)$overlap, 1)
expect_equal(hr_overlap(loc, loc)$overlap, 1)
expect_equal(hr_overlap(kde, kde)$overlap, 1)
expect_equal(hr_overlap(od, od)$overlap, 1)

expect_true(all(names(hr_area(rd)) == c("level", "what", "area")))
expect_true(all(names(hr_area(loc)) == c("level", "what", "area")))
expect_true(all(names(hr_area(mcp)) == c("level", "what", "area")))
expect_true(all(names(hr_area(kde)) == c("level", "what", "area")))
expect_true(all(names(hr_isopleths(rd)) == c("level", "what", "area", "geometry")))
expect_true(all(names(hr_isopleths(loc)) == c("level", "what", "area", "geometry")))
expect_true(all(names(hr_isopleths(mcp)) == c("level", "what", "area", "geometry")))
expect_true(all(names(hr_isopleths(kde)) == c("level", "what", "area", "geometry")))

# Test units of hr_area
# hr_locoh
expect_true(is.numeric(hr_area(loc)$area))
expect_true(is.numeric(hr_area(loc, units = FALSE)$area))
expect_true(is(hr_area(loc, units = TRUE)$area, "units"))

f1 <- make_track(mini_fisher, x_, y_)
loc1 <- hr_locoh(f1)
expect_true(is.numeric(hr_area(loc1)$area))
expect_true(is.numeric(hr_area(loc1, units = FALSE)$area))
expect_true(is.numeric(hr_area(loc1, units = TRUE)$area))
expect_warning(hr_area(loc1, units = TRUE))

# hr_mcp
expect_true(is.numeric(hr_area(mcp)$area))
expect_true(is.numeric(hr_area(mcp, units = FALSE)$area))
expect_true(is(hr_area(mcp, units = TRUE)$area, "units"))

mcp1 <- hr_mcp(f1)
expect_true(is.numeric(hr_area(mcp1)$area))
expect_true(is.numeric(hr_area(mcp1, units = FALSE)$area))
expect_true(is.numeric(hr_area(mcp1, units = TRUE)$area))
expect_warning(hr_area(mcp1, units = TRUE))

# hr_kde
expect_true(is.numeric(hr_area(kde)$area))
expect_true(is.numeric(hr_area(kde, units = FALSE)$area))
expect_true(is(hr_area(kde, units = TRUE)$area, "units"))

kde1 <- hr_kde(f1)
expect_true(is.numeric(hr_area(kde1)$area))
expect_true(is.numeric(hr_area(kde1, units = FALSE)$area))
expect_true(is.numeric(hr_area(kde1, units = TRUE)$area))
expect_warning(hr_area(kde1, units = TRUE))
