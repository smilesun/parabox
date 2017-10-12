context("ParamTree")

test_that("test if ParamTree constructor works", {
  ps = ParamSetTree$new(ns = ParamCategorical$new(id = "Model", values = c("svm", "rf")))
  pn = ParamSetTree$new(ns = ParamInt$new(id = "degree", lower = 1L, upper = 10L), depend = list(id = "kernel", val = "poly"))
  pf = ParamCategorical$new(id = "Kernel", values = c("rbf", "poly", "linear"))
  pt = ParamSetTree$new(ns = pf,  depend = list(id = "model",val = "svm"))
})


test_that("test if ParamTree sample works", {
  ps = ParamSetTree$new(ns = ParamCategorical$new(id = "Model", values = c("svm", "rf")))
  pn = ParamSetTree$new(ns = ParamInt$new(id = "degree", lower = 1L, upper = 10L), depend = list(id = "kernel", val = "poly"))
  pf = ParamCategorical$new(id = "Kernel", values = c("rbf", "poly", "linear"))
  pt = ParamSetTree$new(ns = pf,  depend = list(id = "model",val = "svm"))
  res = ps$handle$addMandChild(pt$handle)
  res$addCondChild(pn$handle)
  ps$sample()
})

test_that("simple construction works", {
  # that is how i would like it to work

  # ParamSetFlat can only be terminal leaf
  # A ParamTree consists of multiple ParamSimple and children
  # ParamSimple can not have Children
  # Only ParamTree can have children
  # A child of a ParamTree can be a ParamSimple/ParamSetTree/ParamSetFlat(only terminal-leaf)
  # Children are conditional on all Parameters of the Parent Tree
  #
  # ParVals for trees are stored as list
  # A 
  if (FALSE) {
    ps = ParamSetTree$new(id = "treeParam", params = list(
      ParamCategorical$new(id = "method", values = c("a", "b"))))
    ps.a = ParamSetFlat$new(id = "a.flatParam", params = list(th.param.int, th.param.real))
    ps.b = ParamSetTree$new(id = "b.treeParam", params = list(
      ParamInt$new(id = 'b.int.1', lower = 0, upper = 10),
      ParamInt$new(id = 'b.int.2', lower = 0, upper = 20),
      ParamCategorical$new(id = 'b.choice', values = c("b1", "b2"))))
    p.b.b1 = ParamReal$new(id = 'b.b1.x', lower = -1, upper = 1)
    ps$addChild(ps.a, condition = quote(method == "a"))
    ps.b$addChild(p.b.b1, condition = quote(b.choice == "b1"))
    ps$addChild(ps.b, condition = quote(method == "b"))

    pv = list(
      treeParam = list(
        method = "b", 
        b.treeParam = list(
          b.int.1 = 4,
          b.int.2 = 18,
          b.choice = "b1",
          b.b1.x = list(
            b.b1.x = 0.4)
          )
        )
      )

    expect_true(ps$test(pv))


  }
})
