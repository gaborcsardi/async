
context("async_debug")

test_that("async_next", {

  new_el <- push_event_loop()
  on.exit({ new_el$cancel_all(); pop_event_loop() }, add = TRUE)
  `__async_synchronise_frame__` <- TRUE

  eps <- 0
  res <- delay(eps)$
    then(function() delay(eps))$
    then(function() delay(eps))
  priv <- get_private(res)
  priv$null()
  priv$run_action()

  al <- async_list()
  expect_equal(nrow(al), 3)
  expect_true(all(al$state == "pending"))

  async_next()
  al <- async_list()
  expect_equal(sort(al$state), c("fulfilled", rep("pending", 2)))

  async_next()
  al <- async_list()
  expect_equal(sort(al$state), c("pending", "pending"))

  async_next()
  al <- async_list()
  expect_equal(sort(al$state), c("fulfilled", "pending"))
})

test_that("async_list", {
  new_el <- push_event_loop()
  on.exit({ new_el$cancel_all(); pop_event_loop() }, add = TRUE)
  `__async_synchronise_frame__` <- TRUE

  eps <- 1/100000
  p1 <- delay(eps)
  p2 <- p1$then(function() "foo")
  res <- p2$then(function() "bar")

  priv <- get_private(res)
  priv$null()
  priv$run_action()

  sh <- get_private(p1)$id - 1L
  al <- async_list()
  expect_equal(al$id, 3:1 + sh)
  expect_equal(unclass(al$parents), list(2L + sh, 1L + sh, integer()))
  expect_equal(vcapply(al$call, typeof), rep("language", 3))
  expect_equal(
    as.character(al$call),
    c("p2$then(function() \"bar\")",
      "p1$then(function() \"foo\")",
      "delay(eps)")
  )
  expect_equal(unclass(al$children), list(integer(), 3L + sh, 2L + sh))
  expect_match(al$type[1], "^then-")
  expect_match(al$type[2], "^then-")
  expect_equal(al$type[3], "delay")
  expect_true(all(al$running))
  expect_equal(al$state, rep("pending", 3))
  expect_true(all(!al$cancelled))
  expect_true(all(!al$shared))
})

test_that("async_tree", {
  new_el <- push_event_loop()
  on.exit({ new_el$cancel_all(); pop_event_loop() }, add = TRUE)
  `__async_synchronise_frame__` <- TRUE

  eps <- 1/100000
  p1 <- delay(eps)
  p2 <- p1$then(function() "foo")
  res <- p2$then(function() "bar")

  priv <- get_private(res)
  priv$null()
  priv$run_action()

  tree <- async_tree()
  expect_s3_class(tree, "tree")
  prn <- format(tree)
  expect_equal(length(prn), 3)
  expect_match(prn[1], "p2$then", fixed = TRUE)
  expect_match(prn[2], "p1$then", fixed = TRUE)
  expect_match(prn[3], "delay(eps)", fixed = TRUE)
})

test_that("async_debug", {
  new_el <- push_event_loop()
  on.exit({ new_el$cancel_all(); pop_event_loop() }, add = TRUE)
  `__async_synchronise_frame__` <- TRUE

  eps <- 0
  p1 <- delay(eps)
  tf  <- function() "foo"
  p2 <- p1$then(tf)
  res <- p2$then(function() "bar")

  priv <- get_private(res)
  priv$null()
  priv$run_action()

  async_debug(get_private(p2)$id)
  expect_true(isdebugged(get_private(p2)$parent_resolve))
  expect_true(isdebugged(get_private(p2)$parent_reject))

  async_wait_for(get_private(p1)$id)
  expect_message(async_debug(get_private(p1)$id), "already resolved")

  res <- deferred$new()
  priv <- get_private(res)
  priv$null()
  expect_message(async_debug(get_private(res)$id), "has no action")

  res <- deferred$new(action = function() { })
  priv <-  get_private(res)
  priv$null()
  expect_message(async_debug(get_private(res)$id), "debugging action")
})

test_that("async_wait_for", {
  new_el <- push_event_loop()
  on.exit({ new_el$cancel_all(); pop_event_loop() }, add = TRUE)
  `__async_synchronise_frame__` <- TRUE

  eps <- 1/100000
  p1 <- delay(eps)
  p2 <- p1$then(function() "foo")
  res <- p2$then(function() "bar")

  priv <- get_private(res)
  priv$null()
  priv$run_action()

  async_wait_for(get_private(p2)$id)
  expect_equal(get_private(p1)$state, "fulfilled")
  expect_equal(get_private(p2)$state, "fulfilled")
  expect_equal(get_private(res)$state, "pending")
})

test_that("async_where", {

  id <- NULL
  do <- function() {
    p <- delay(1/10000)$
      then(function() "foo")$
      then(function() async_where())
    id <<- get_private(p)$id
    p
  }

  res <- synchronise(do())
  expect_true(any(res$async))
  aframe <- utils::tail(which(res$async), 1)
  expect_equal(res$def_id[aframe],  id)
  expect_equal(res$def_cb_type[aframe], "parent")
  expect_equal(typeof(res$def_call[[aframe]]), "language")
})

test_that("format.async_where", {
  id <- NULL
  do <- function() {
    p <- delay(1/10000)$
      then(function() "foo")$
      then(function() async_where())
    id <<- get_private(p)$id
    p
  }

  res <- synchronise(do())
  prn <- format(res)
  expect_match(prn, paste0(id, " parent .*async_where"))
})
