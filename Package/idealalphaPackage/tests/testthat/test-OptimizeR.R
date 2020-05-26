library(OptimizeR)


context("Core OptimizeR functionality")

test_that('returned alpha is correct', {
        n <- 100
        effsize <- 0.3
        prior <- 0.5
        posttrue <- 0.8
        postfalse <- 0.2

        ac <- alphaConfirmation(n, effsize, prior, posttrue)
        ad <- alphaDisconfirmation(n, effsize, prior, postfalse)
        pp <- probpower(effsize, prior, postfalse, posttrue)
        ia <- idealalpha(n, effsize, prior)

        expect_equal(round(postConfirm(ac$power,ac$alpha,  prior), digits = 1), posttrue)
        expect_equal(round(postDisconfirm(ad$power, ad$alpha, prior), digits =1), postfalse)
        expect_equal(round(postConfirm(pp$power,pp$alpha,  prior), digits =1), posttrue)
        expect_equal(round(postDisconfirm(pp$power,pp$alpha,  prior), digits =1), postfalse)
        expect_equal(round(information(postConfirm(ia$power, ia$alpha, prior),postDisconfirm(ia$power, ia$alpha, prior),ia$power, ia$alpha, prior),2), round(ia$Learning, digits = 2))
})


