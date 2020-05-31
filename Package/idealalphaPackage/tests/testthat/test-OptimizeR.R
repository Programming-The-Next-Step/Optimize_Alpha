library(OptimizeR)
#please note that the tests are intended to fail in some situations.
#These are when the alpha level that marginally indicates the intended posterior
#probability is higher/lower than the intended posterior probability
#then the code will default to the most informative alpha level (for more information
#the package documentation or the vignette)
#In addition, ProbPower might result in higher/lower posterior probabilites than intended
#for high effect size since participants can minimum be added one at a time.

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


