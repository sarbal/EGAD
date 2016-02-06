context("AUROC checks")

test_that("All AUROC calculations act as expected", {

  scores <- 1:11
  labels <- rep(0,11)
  labels[6] <- 1 
  expect_equal( auroc_analytic(scores,labels), 0.5)  
  roc <- get_roc(scores,labels)
  expect_equal( get_auc(roc[,1], roc[,2]) , 0.5)

  labels[1:5] <- 1
  expect_equal( auroc_analytic(scores,labels), 0)
  roc <- get_roc(scores,labels)
  expect_equal( get_auc(roc[,1], roc[,2]) , 0)

  labels[1:5] <- 0
  labels[6:11] <- 1
  expect_equal( auroc_analytic(scores,labels), 1)
  roc <- get_roc(scores,labels)
  expect_equal( get_auc(roc[,1], roc[,2]) , 1)


  })



