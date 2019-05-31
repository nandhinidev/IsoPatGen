source(file.path(getwd(),"..","R","functions.R"))

context('Testing Data type and dimensions')
test_that('Correct data type and dimensions',{
  tdata = envelope(3)
  expect_is(tdata,'data.table')
  expect_equal(ncol(tdata),8)
  expect_true(max(tdata$sum) == 3)
  expect_true(min(tdata$sum) == 0)
})

context('Testing Probability Distribution values')
test_that('PD',{
  cpd = cdist(100,3)
  hpd = hdist(300,1)
  npd = ndist(100,1)
  opd = odist(150,2,0)
  spd = sdist(5,0,2)
  spdn = sdist(2,1,3)
  expect_true(cpd <= 1)
  expect_true(hpd <= 1)
  expect_true(npd <= 1)
  expect_true(opd <= 1)
  expect_true(spd <= 1)
  expect_equal(spdn,0)
})

context('Testing Data integrity')
test_that('Data',{
  niso = 3
  tdata = envelope(niso)
  tmat = tdata[sum == 2]
  elements = c(98,150,27,29,0)
  names(elements) = c("carbon","hydrogen","nitrogen","oxygen","sulphur")
  tabund = pred.int(tdata,niso)
  expect_equal(unique(tmat$sum),2)
  expect_true(is.data.frame(tabund))
})
