test_that("supervised", {
  expect_equal({
    data(genotypes)
    set.seed(1313)
    modelSupervised = supervised(knowns=genotypes$knowns, 
                                 class=genotypes$labels)
    class(modelSupervised)
  }, c("supervisedModel", "mModel" ))
})

test_that("semisupervised", {
  expect_equal({
    data(genotypes)
    set.seed(1313)
    modelSemiSupervised = semisupervised(X=genotypes$X, 
                                         knowns=genotypes$knowns, class = genotypes$labels)
    class(modelSemiSupervised)
  }, c("semisupervisedModel", "mModel" ))
})

test_that("belief", {
  expect_equal({
    data(genotypes)
    set.seed(1313)
    modelBelief = belief(X=genotypes$X, 
                         knowns=genotypes$knowns, B=genotypes$B)
    class(modelBelief)
  }, c("beliefModel", "mModel" ))
})

test_that("soft", {
  expect_equal({
    data(genotypes)
    set.seed(1313)
    modelSoft = soft(X=genotypes$X, 
                     knowns=genotypes$knowns, P=genotypes$B)
    class(modelSoft)
  }, c("softModel", "mModel" ))
})

test_that("unsupervised", {
  expect_equal({
    data(genotypes)
    set.seed(1313)
    modelUnSupervised = unsupervised(X=genotypes$X, k=3)
    class(modelUnSupervised)
  }, c("softModel", "mModel" ))
})

test_that("crossval.belief", {
  expect_equal({
    data(genotypes)
    set.seed(1313)
    modelBelief = belief(X=genotypes$X, 
                         knowns=genotypes$knowns, B=genotypes$B)
    res <- crossval(model = modelBelief,folds=10)
    length(res)
  }, 3)
})

test_that("crossval.supervised", {
  expect_equal({
    data(genotypes)
    set.seed(1313)
    res <- crossval(model = supervised(knowns=genotypes$knowns,  
                                class=genotypes$labels),folds=10,
             fun = supervised)
    length(res)
  }, 3)
})

test_that("beliefList_high_dim_8", {
  expect_equal({
    set.seed(1313)
    simulated = simulateData(d=8, k=3, n=100, m=70, cov="0", within="E", n.labels=2)
    models3 = beliefList(X=simulated$X, knowns=simulated$knowns, B=simulated$B,
                         kList=2:4, mean="D", within="D")
    class(models3)[1]
  }, "mModelList")
})

test_that("beliefList_high_dim_200", {
  expect_equal({
    set.seed(1313)
    simulated = simulateData(d=200, k=3, n=100, m=70, cov="0", within="E", n.labels=3)
    models3 = beliefList(X=simulated$X, knowns=simulated$knowns, B=simulated$B,
                         kList=3:4, mean="D", within="D")
    class(models3)[1]
  }, "mModelList")
})

test_that("semisupervisedList_high_dim_8", {
  expect_equal({
    set.seed(1313)
    simulated = simulateData(d=8, k=3, n=100, m=70, cov="0", within="E", n.labels=3)
    models3 = semisupervisedList(X=simulated$X, knowns=simulated$knowns, class=simulated$Ytrue[31:100],
                         kList=3:4, mean="D", within="D")
    class(models3)[1]
  }, "mModelList")
})

