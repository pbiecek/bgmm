test_that("supervised", {
  expect_equal({
    data(genotypes)
    modelSupervised = supervised(knowns=genotypes$knowns, 
                                 class=genotypes$labels)
    class(modelSupervised)
  }, c("supervisedModel", "mModel" ))
})

test_that("semisupervised", {
  expect_equal({
    data(genotypes)
    modelSemiSupervised = semisupervised(X=genotypes$X, 
                                         knowns=genotypes$knowns, class = genotypes$labels)
    class(modelSemiSupervised)
  }, c("semisupervisedModel", "mModel" ))
})

test_that("belief", {
  expect_equal({
    data(genotypes)
    modelBelief = belief(X=genotypes$X, 
                         knowns=genotypes$knowns, B=genotypes$B)
    class(modelBelief)
  }, c("beliefModel", "mModel" ))
})

test_that("soft", {
  expect_equal({
    data(genotypes)
    modelSoft = soft(X=genotypes$X, 
                     knowns=genotypes$knowns, P=genotypes$B)
    class(modelSoft)
  }, c("softModel", "mModel" ))
})

test_that("unsupervised", {
  expect_equal({
    data(genotypes)
    modelUnSupervised = unsupervised(X=genotypes$X, k=3)
    class(modelUnSupervised)
  }, c("softModel", "mModel" ))
})
