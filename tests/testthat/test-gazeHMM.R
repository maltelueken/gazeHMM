test_that("gazeHMM runs", {

  data(video)

  res <- c(1024, 768)
  dim <- c(380, 300)
  dist <- 670
  fr <- 500

  df <- video[video$subject == 1,]

  res <- gazeHMM(x = df$x, y = df$y, t = df$t, unit = "px",
                 res = res, dim = dim, dist = dist, fr = fr, blink = c(0, 0),
                 nstates = 4, sf = c(100, 100), random.respstart = F,
                 min.sac = 0.01)

  expect_equal(class(res), "gazeHMM")
  expect_equal(length(res), 4)
})
