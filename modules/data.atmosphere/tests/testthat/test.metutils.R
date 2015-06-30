context("testing met utility functions")

test_that("sw2par, par2ppfd, sw2ppfd are consistent ",{
    expect_equal(sw2par(1000), 486)
    expect_equal(par2ppfd(486), 2068.08510638298)
    expect_equal(sw2ppfd(1000), par2ppfd(sw2par(1000)))
    expect_equal(sw2ppfd(0:1000), par2ppfd(sw2par(0:1000)))
})

test_that("qair2rh and rh2qair work and are inverse",{
    expect_equal(qair2rh(qair = 0.01, temp = 20, press = 1013.25),
                 0.69286089020191)
    expect_equal(qair2rh(qair = 0.001, temp = 20, press = 1013.25),
                 0.0696648158949306)

 
    rh2qair(rh = qair2rh(qair = 0.01, temp = 20, press = 1013.25), T = 20 + 272.15, press = 1013.25)
                   
})
