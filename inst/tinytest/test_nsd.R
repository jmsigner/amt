data(deer)

nsd0 <- deer |> nsd()
nsd1 <- deer |> add_nsd()
nsd2 <- deer |> steps() |> add_nsd()

expect_true(is(nsd0, "numeric"))
expect_true(is(nsd1, "data.frame"))
expect_true(is(nsd2, "data.frame"))
expect_true(is(nsd1, "track_xyt"))
expect_true(is(nsd2, "steps_xyt"))

expect_true(all(nsd0 == nsd1$nsd_))
expect_true(all(nsd0[-1] == nsd2$nsd_))
