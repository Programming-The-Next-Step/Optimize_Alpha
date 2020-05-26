library(OptimizeR)


context("Core OptimizeR functionality")

test_that('returned alpha is correct', {
        n <- 40
        effsize <- 0.2
        prior <- 0.5
        posttrue <- 0.95
        postfalse <- 0.05
        test <- "t-test"
        alternative <- "greater"
        type <- "one.sample"


        ac <- alphaConfirmation(n, effsize, prior, posttrue, test = test, alternative = alternative, type = type)
        ad <- alphaDisconfirmation(n, effsize, prior, postfalse, test = test, alternative = alternative, type = type)
        pp <- probPower(effsize, prior, postfalse, posttrue, test = test, alternative = alternative, type = type)
        ia <- idealAlpha(n, effsize, prior, test = test, alternative = alternative, type = type)
        expect_equal(round(postConfirm(ac$power,ac$alpha,  prior), digits = 2), posttrue)
        expect_equal(round(postDisconfirm(ad$power, ad$alpha, prior), digits =2), postfalse)
        expect_equal(round(postConfirm(pp$power,pp$alpha,  prior), digits =2), posttrue)
        expect_equal(round(postDisconfirm(pp$power,pp$alpha,  prior), digits =2), postfalse)
        expect_equal(round(information(postConfirm(ia$power, ia$alpha, prior),postDisconfirm(ia$power, ia$alpha, prior),ia$power, ia$alpha, prior),2), round(ia$Learning, digits = 2))
})


