#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pwr)
library(shinydashboard)
# Define UI for application that draws a histogram

ui <- dashboardPage(
    dashboardHeader(title = "Optimize Alpha"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Optimize Learning", tabName = "ideal", icon = icon("calculator")),
            menuItem("Posterior|Significance", tabName = "alphaC", icon = icon("calculator")),
            menuItem("Posterior|Non-Significance", tabName = "alphaD", icon = icon("calculator")),
            menuItem("Probability Based Power Analysis", tabName = "PBPA", icon = icon("calculator"))
            )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "ideal",
                    fluidRow(
                        box(
                            # select number of participants, effect size and prior
                            numericInput("n", "Sample Size Per Group", value = 20, min = 0, max = 500, step = 1),
                            
                            #select effect size
                            numericInput("d", "Expected Effect Size", value = 0.5, min = 0, max = 2, step = 0.1),
                            
                            #prior
                            numericInput("prior", "Prior Probability of H1", value = 0.5, min = 0, max = 1, step = 0.05),
                            
                            #Kind of statistical test. Can be "t-test", "anova" and "correlation".
                            
                            selectInput("test", "Select Statistical Test", c("t-test" = "t-test","anova" = "anova")),
                            #Indicate whether it is a "one.sample", or "two.sample" test (for t-test only)
                            
                            selectInput("type", "Within- or between-sample (for t-test)",
                                        c("within" = "one.sample", "between" = "two.sample")),
                            #Indicate whether the alternative is "two.sided", "less" or "greater"
                            
                            selectInput("alternative", "Direction of Hypothesis (for t-test )",
                                        c("two-sided" = "two.sided", "greater" = "greater")),
                            #Number of groups for anova.
                            numericInput("K", "Number of Groups (for ANOVA)", value = 4, min = 2, max = 16)
                        ),
                        # Show a plot of the generated distribution
                        box(
                            plotOutput("OptimalAlpha"),
                            textOutput("Results")
                        )
                    )
                ),
            
        # Second tab content
        tabItem(tabName = "alphaC",
                fluidRow(
                    box(
                        # select number of participants, effect size and prior
                        numericInput("nC", "Sample Size Per Group", value = 20, min = 0, max = 500, step = 1),
                        
                        #select effect size
                        numericInput("dC", "Expected Effect Size", value = 0.5, min = 0, max = 2, step = 0.1),
                        
                        #prior
                        numericInput("priorC", "Prior Probability of H1", value = 0.5, min = 0, max = 1, step = 0.05),
                        
                        #intended posterior
                        numericInput("postC", "Intended Posterior Probability after a Significant Result", value = 0.5, min = 0, max = 1, step = 0.05),
                        
                        #Kind of statistical test. Can be "t-test", "anova" and "correlation".
                        
                        selectInput("testC", "Select Statistical Test", c("t-test" = "t-test","anova" = "anova")),
                        #Indicate whether it is a "one.sample", or "two.sample" test (for t-test only)
                        
                        selectInput("typeC", "Within- or between-sample (for t-test)",
                                    c("within" = "one.sample", "between" = "two.sample")),
                        #Indicate whether the alternative is "two.sided", "less" or "greater"
                        
                        selectInput("alternativeC", "Direction of Hypothesis (for t-test )",
                                    c("two-sided" = "two.sided", "greater" = "greater")),
                        #Number of groups for anova.
                        numericInput("KC", "Number of Groups (for ANOVA)", value = 4, min = 2, max = 16)
                    ),
                    # Show a plot of the generated distribution
                    box(
                        plotOutput("AlphaConfirmation"),
                        textOutput("ResultsC")
                    )
            )
            
        ),
        tabItem(tabName = "alphaD",
                fluidRow(
                    box(
                        # select number of participants, effect size and prior
                        numericInput("nD", "Sample Size Per Group", value = 20, min = 0, max = 500, step = 1),
                        
                        #select effect size
                        numericInput("dD", "Expected Effect Size", value = 0.5, min = 0, max = 2, step = 0.1),
                        
                        #prior
                        numericInput("priorD", "Prior Probability of H1", value = 0.5, min = 0, max = 1, step = 0.05),
                        
                        #intended posterior
                        numericInput("postD", "Intended Posterior Probability after a Non-Significant Result", value = 0.3, min = 0, max = 1, step = 0.05),
                        
                        #Kind of statistical test. Can be "t-test", "anova" and "correlation".
                        
                        selectInput("testD", "Select Statistical Test", c("t-test" = "t-test","anova" = "anova")),
                        #Indicate whether it is a "one.sample", or "two.sample" test (for t-test only)
                        
                        selectInput("typeD", "Within- or between-sample (for t-test)",
                                    c("within" = "one.sample", "between" = "two.sample")),
                        #Indicate whether the alternative is "two.sided", "less" or "greater"
                        
                        selectInput("alternativeD", "Direction of Hypothesis (for t-test )",
                                    c("two-sided" = "two.sided", "greater" = "greater")),
                        #Number of groups for anova.
                        numericInput("KD", "Number of Groups (for ANOVA)", value = 4, min = 2, max = 16)
                    ),
                    # Show a plot of the generated distribution
                    box(
                        plotOutput("AlphaDisconfirmation"),
                        textOutput("ResultsD")
                    )
                )
                
        ),
        tabItem(tabName = "PBPA",
                fluidRow(
                    box(
                        
                        #select effect size
                        numericInput("dP", "Expected Effect Size", value = 0.5, min = 0, max = 2, step = 0.1),
                        
                        #prior
                        numericInput("priorP", "Prior Probability of H1", value = 0.5, min = 0, max = 1, step = 0.05),
                        
                        #intended posterior
                        numericInput("postCP", "Intended Posterior Probability after a Significant Result", value = 0.7, min = 0, max = 1, step = 0.05),
                        
                        #intended posterior
                        numericInput("postDP", "Intended Posterior Probability after a Non-Significant Result", value = 0.3, min = 0, max = 1, step = 0.05),
                        #Kind of statistical test. Can be "t-test", "anova" and "correlation".
                        
                        selectInput("testP", "Select Statistical Test", c("t-test" = "t-test","anova" = "anova")),
                        #Indicate whether it is a "one.sample", or "two.sample" test (for t-test only)
                        
                        selectInput("typeP", "Within- or between-sample (for t-test)",
                                    c("within" = "one.sample", "between" = "two.sample")),
                        #Indicate whether the alternative is "two.sided", "less" or "greater"
                        
                        selectInput("alternativeP", "Direction of Hypothesis (for t-test )",
                                    c("two-sided" = "two.sided", "greater" = "greater")),
                        #Number of groups for anova.
                        numericInput("KP", "Number of Groups (for ANOVA)", value = 4, min = 2, max = 16)
                    ),
                    # Show a plot of the generated distribution
                    box(
                        plotOutput("ProbPower"),
                        textOutput("ResultsP")
                    )
                )
                
        )
    )
)
)

#Define server logic to calculate optimal alpha
server <- function(input, output) {

    postConfirm <- function(power, alpha, prior)  {
        (power * prior) / (power * prior + alpha * (1 - prior))
    }
    
    
    postDisconfirm <- function(power, alpha, prior) {
        (1 - power) * prior / (( 1 - alpha) * (1 - prior) + (1 - power) * prior)
    }
    
    #define power function for t.test and anova based on pwr package
    get_power <- function(alpha, effsize, n, test = "t-test", type = "two.sample", alternative = "two.sided", k = 4) { #get_power for different alpha levels, two tail independent samples t-test
        if(test == "t-test") {
            power <- pwr.t.test(n, effsize, alpha, type = type, alternative = alternative)
        }
        else if(test == "anova") {
            power <- pwr.anova.test(k, n, effsize, alpha)
        }
        return(power$power)
    }
    
    #calculate the expected correct change in believe
    #only exportet for testing
    #'@export
    information <- function(confirm, disconfirm, power, alpha, prior) #calcualte the amount of information per test as %correct change in believe about the hypothesis
    {
        rightupdate <- (prior * power * (confirm - prior) + (1 - prior) * (1 - alpha) * (prior - disconfirm))
        wrongupdate <- ((1 - prior) * alpha * (confirm - prior) + (1 - power) * prior * (prior - disconfirm)) #you could also update incorrectly for false postives and false negatives
        information <- rightupdate - wrongupdate #the expected correct change in belief in the hypothesis
    }
    
    #### Panel1 ####
    
    output$OptimalAlpha <- renderPlot({ 
        n <- input$n
        effsize <- input$d 
        prior <- input$prior 
        test = input$test
        type = input$type
        alternative = input$alternative 
        k = input$K
        alpha  <- seq(0, 1, .00001) #supply a vector of alphas
        power  <- get_power(alpha, effsize, n, test = test, type = type, alternative = alternative, k = k) # calculate power
        confirm <- postConfirm(power, alpha, prior) #posterior after confirming
        disconf <- postDisconfirm(power, alpha, prior) #posterior after disconfirming
        info <- information(confirm, disconf, power, alpha, prior) #expected correct change in believe
        d <- as.data.frame(cbind(alpha, power, info))
        maxinf <- max(na.omit(info))
        dmax <- as.data.frame(subset(d, info == maxinf)) #return valus with highes correct change in believe
        d05 <- as.data.frame(subset(d, alpha == 0.05)) #compare to 5% alpha
        plot(alpha, info,
             ylab = "Expected Correct Change in Belief",
             xlab = "Significance Level",
             type = "l", main = "Optimal Alpha",
             xlim = c(0,1), axes = F,
             ylim = c(0,round(maxinf + 0.005, digits = 2)))
        abline(v = 0.05, col = "red")
        abline(v = dmax$alpha, col = "blue")
        axis(side= 2, at=c(seq(0, (maxinf + 0.01), 0.01)))
        axis(side= 1, at=c(seq(0, 1, 0.05)))
        mtext(side = 3, paste("Alpha = ", round(dmax$alpha, digits = 2), " Power = ", round(dmax$power, digits = 2), "Learning =", round(dmax$info*100, digits = 2), "%"))
    })
    
    #output table with results
    
    output$Results <- renderText({ 
        n <- input$n
        effsize <- input$d 
        prior <- input$prior 
        test = input$test
        type = input$type
        alternative = input$alternative 
        k = input$K
        alpha  <- seq(0, 1, .00001) #supply a vector of alphas
        power  <- get_power(alpha, effsize, n, test = test, type = type, alternative = alternative, k = k) # calculate power
        confirm <- postConfirm(power, alpha, prior) #posterior after confirming
        disconf <- postDisconfirm(power, alpha, prior) #posterior after disconfirming
        info <- information(confirm, disconf, power, alpha, prior) #expected correct change in believe
        d <- as.data.frame(cbind(alpha, power, info))
        maxinf <- max(na.omit(info))
        dmax <- as.data.frame(subset(d, info == maxinf)) #return valus with highes correct change in believe
        d05 <- as.data.frame(subset(d, alpha == 0.05)) #compare to 5% alpha
        dmax <- cbind(dmax, d05$info)
        colnames(dmax) <- c("alpha", "power", "Learning", "Learning05")
        if(dmax$alpha > 0.99) stop("Hard to estimate alpha/power with so few participants/small effect size.
                               Consider increasing the sample size")
        paste("The ideal alpha level is ", round(dmax$alpha, digits = 2), " with a power of ", round(dmax$power, digits = 2),
              " and a Learning of ", round(dmax$Learning, digits = 2), ". This is " ,round(dmax$Learning/dmax$Learning05, digits = 2),
              " times as much learning as with the conventional alpha level of 0.05 ", sep = "")
    })
    
    #### Panel2 ####
    
    output$AlphaConfirmation <- renderPlot({ 
        n <- input$nC
        effsize <- input$dC
        prior <- input$priorC
        posttrue <- input$postC
        test = input$testC
        type = input$typeC
        alternative = input$alternativeC
        k = input$KC
        if(prior > posttrue) #stop if posterior smaller than prior
        {
            stop("Prior must be smaller than posterior")
        }
        alpha   <- seq(0, 1, .00001) #define vector of alphas
        #calculate powers
        power  <- get_power(alpha, effsize, n, test = test, type = type, alternative = alternative, k = k)
        #calculate posteriors after confirming or disconfirming and expected learning
        confirm <- postConfirm(power, alpha, prior)
        disconf <- postDisconfirm(power, alpha, prior)
        info <- information(confirm, disconf, power, alpha, prior)
        df <- data.frame(alpha, power, info, confirm)
        #calculate the smallest confirming posterior that is above the user specified posterior
        #the alpha level corresponding to this posterior is what we are looking for.
        maxinf <- max(na.omit(info))
        postt <- as.data.frame(subset(df, confirm > posttrue))
        if (nrow(postt) == 0) stop("You need more participants to enable the intended in probability")
        postt <- max(postt$info)
        d <- subset(df, info == postt)
        if(nrow(d) > 1) {
            stop ("You have unreasonably high power,
                        therefore, there is not enough precision to calculate
                        a unique best alpha level. Consider a smaller sample size")
        }
        #plot
        plot(alpha,info,
             ylab = "Expected Change in Belief",
             xlab = "Signifcance Level", type = "l",
             main = paste("Alpha for",posttrue, "Probability after Significance") ,
             xlim = c(0,1), axes = F, ylim =c(0,round(maxinf + 0.005, digits = 2)))
        abline(v = 0.05, col = "red")
        abline(v = d$alpha, col = "blue")
        axis(side = 2, at = c(seq(0,(maxinf+0.01),0.01)))
        axis(side = 1, at = c(seq(0,1,0.05)))
        mtext(side = 3, paste("Alpha = ", round(d$alpha, digits = 2), " Power = ", round(d$power, digits = 2), "Learning =", round(d$info * 100, digits = 2), "%"))
        if(d$alpha[1] > 0.99) {
            stop("Hard to estimate alpha/power with so few participants/small effect size.
                               Consider increasing the sample size")
        }
        if(round(d$confirm,1) != round(posttrue,1)) {
            warning("You have a lot of power, therefore, you might achieve
                  even higher probability than intended")
        }
    })
    
    output$ResultsC <- renderText({
        n <- input$nC
        effsize <- input$dC
        prior <- input$priorC
        posttrue <- input$postC
        test = input$testC
        type = input$typeC
        alternative = input$alternativeC
        k = input$KC
        if(prior > posttrue) #stop if posterior smaller than prior
        {
            stop("Prior must be smaller than posterior")
        }
        alpha   <- seq(0, 1, .00001) #define vector of alphas
        #calculate powers
        power  <- get_power(alpha, effsize, n, test = test, type = type, alternative = alternative, k = k)
        #calculate posteriors after confirming or disconfirming and expected learning
        confirm <- postConfirm(power, alpha, prior)
        disconf <- postDisconfirm(power, alpha, prior)
        info <- information(confirm, disconf, power, alpha, prior)
        df <- data.frame(alpha, power, info, confirm)
        #calculate the smallest confirming posterior that is above the user specified posterior
        #the alpha level corresponding to this posterior is what we are looking for.
        maxinf <- max(na.omit(info))
        postt <- as.data.frame(subset(df, confirm > posttrue))
        if (nrow(postt) == 0) stop("You need more participants to enable the intended in probability")
        postt <- max(postt$info)
        d <- subset(df, info == postt)
        if(nrow(d) > 1) {
            stop ("You have unreasonably high power,
                        therefore, there is not enough precision to calculate
                        a unique best alpha level. Consider a smaller sample size")
        }
        if(d$alpha[1] > 0.99) {
            stop("Hard to estimate alpha/power with so few participants/small effect size.
                               Consider increasing the sample size")
        }
        if(round(d$confirm,1) != round(posttrue,1)) {
            warning("You have a lot of power, therefore, you might achieve
                  even higher probability than intended")
        }
        paste("The needed alpha level is ", round(d$alpha, digits = 2), " with a power of ", round(d$power, digits = 2),
              " and an actual posterior probability after significance of ", round(d$confirm, digits = 2), ".")
    })
    
    ### panel 3 ###
    output$AlphaDisconfirmation <- renderPlot({ 
        n <- input$nD
        effsize <- input$dD
        prior <- input$priorD
        postfalse <- input$postD
        test = input$testD
        type = input$typeD
        alternative = input$alternativeD
        k = input$KD
        if(postfalse > prior)
        {
            stop("Prior must be higher than posterior")
        }
        alpha   <- seq(0, 1, .00001)
        power  <- get_power(alpha, effsize, n, test = test, type = type, alternative = alternative, k = k) #power.prop.test(n, p1 = 0.8, p2 = 1, sig.level = alpha)$power
        confirm <- postConfirm(power, alpha, prior)
        disconf <- postDisconfirm(power, alpha, prior)
        info <- information(confirm, disconf, power, alpha, prior)
        df <- data.frame(alpha, power, info, disconf)
        #f <- max(na.omit(info))
        maxinf <- max(na.omit(info))
        postd <- as.data.frame(subset(df, disconf < postfalse))
        if (nrow(postd) == 0) stop("You need more participants to enable the intended in probability")
        postd <- max(postd$info)
        d <- subset(df, info == postd)
        if(nrow(d) > 1) {
            stop ("You have unreasonably high power,
                        therefore, there is not enough precision to calculate
                        a unique best alpha level. Consider a smaller sample size.")
        }
        #plot
        plot(alpha, info,
             ylab = "Expected Change in Belief",
             xlab = "Signifcance Level", type = "l",
             main = paste("Alpha for", round(postfalse, digits = 2), "Probability after Non-Significance") ,
             xlim = c(0,1), axes = F,
             ylim =c(0,round(maxinf + 0.005, digits = 2)))
        abline(v = 0.05, col = "red")
        abline(v = d$alpha, col = "blue")
        axis(side = 2, at = c(seq(0,(maxinf+0.01),0.01)))
        axis(side = 1, at = c(seq(0,1,0.05)))
        mtext(side = 3, paste("Alpha = ", round(d$alpha, digits = 2), " Power = ", round(d$power, digits = 2), "Learning =", round(d$info*100, digits = 2), "%"))
        if(d$alpha > 0.99) {
            stop("Hard to estimate alpha/power with so few participants/small effect size.
                               Consider increasing the sample size")
        }
        if(round(d$disconf,1) != round(postfalse,1)) {
            warning("You have a lot of power, therefore, you might achieve
                  even lower probability than intended")
        }
    })
        output$ResultsD <- renderText({
            n <- input$nD
            effsize <- input$dD
            prior <- input$priorD
            postfalse <- input$postD
            test = input$testD
            type = input$typeD
            alternative = input$alternativeD
            k = input$KD
            if(postfalse > prior)
            {
                stop("Prior must be higher than posterior")
            }
            alpha   <- seq(0, 1, .00001)
            power  <- get_power(alpha, effsize, n, test = test, type = type, alternative = alternative, k = k) #power.prop.test(n, p1 = 0.8, p2 = 1, sig.level = alpha)$power
            confirm <- postConfirm(power, alpha, prior)
            disconf <- postDisconfirm(power, alpha, prior)
            info <- information(confirm, disconf, power, alpha, prior)
            df <- data.frame(alpha, power, info, disconf)
            #f <- max(na.omit(info))
            maxinf <- max(na.omit(info))
            postd <- as.data.frame(subset(df, disconf < postfalse))
            if (nrow(postd) == 0) stop("You need more participants to enable the intended in probability")
            postd <- max(postd$info)
            d <- subset(df, info == postd)
            if(nrow(d) > 1) {
                stop ("You have unreasonably high power,
                        therefore, there is not enough precision to calculate
                        a unique best alpha level. Consider a smaller sample size.")
            }
            #plot
            if(d$alpha > 0.99) {
                stop("Hard to estimate alpha/power with so few participants/small effect size.
                               Consider increasing the sample size")
            }
            if(round(d$disconf,1) != round(postfalse,1)) {
                warning("You have a lot of power, therefore, you might achieve
                  even lower probability than intended")
            }
            paste("The needed alpha level is ", round(d$alpha, digits = 2), " with a power of ", round(d$power, digits = 2),
                  " and an actual posterior probability after significance of ", round(d$disconf, digits = 2), ".")
        })
    ### panel 4 ###
        output$ProbPower <- renderPlot({ 
            effsize <- input$dP
            prior <- input$priorP
            postfalse <- input$postDP
            posttrue <- input$postCP
            test = input$testP
            type = input$typeP
            alternative = input$alternativeP
            k = input$KP
            if(postfalse >= prior | prior >= posttrue)  {
                stop("Posterior after disconfirming needs to be smaller than prior, posterior after confirming larger")
            }
            alpha <- seq(.0001, 1, .001)
            #for 10-20 000 particpants calculte the alpha needed to achieve the posterior after a negative result
            #stop the loop when there is an option that at the same time has the needed posterior probability after a positive result
            for (i in 10:1000) {
                power <- get_power(alpha, effsize, i, test = test, type = type, alternative = alternative, k = k)
                #calculate power for all alphas for this sample size
                disconf <- postDisconfirm(power, alpha, prior)
                confirm <- postConfirm(power, alpha, prior)
                info <- information(confirm, disconf, power, alpha, prior)
                maxinf <- max(na.omit(info))
                df <- data.frame(alpha, power, disconf, confirm, info)
                postd <- as.data.frame(subset(df, disconf < postfalse))
                d <- subset(postd, confirm > posttrue)
                n <- i
                if(nrow(d) > 0) {
                    break
                }
            }
            d <- subset(d, info == max(info))
            d <- cbind(d,n)
            colnames(d) <- (c("alpha", "power", "disconf", "confirm", "info", "n"))
            plot(df$alpha, df$info,
                 ylab = "Expected Correct Change in Belief",
                 xlab = "Signifcance Level",
                 type = "l",
                 main = paste("Probability bas Power Analysis - PosteriorConfirmation:",posttrue, "  Posterior Disconfirmation:", postfalse) ,
                 xlim = c(0, 1),
                 axes = F,
                 ylim = c(0, round(maxinf + 0.005, digits = 2)))
            abline(v = 0.05, col = "red")
            abline(v = d$alpha, col = "blue")
            axis(side = 2, at = c(seq(0, (maxinf + 0.01), 0.01)))
            axis(side = 1, at = c(seq(0, 1, 0.05)))
            mtext(side = 3, paste("Sample Size", i, " Alpha = ", round(d$alpha, digits = 2), " Power = ", round(d$power, digits = 2), "Learning =", round(d$info*100, digits = 2), "%"))
        })
        output$ResultsP <- renderText({
            effsize <- input$dP
            prior <- input$priorP
            postfalse <- input$postDP
            posttrue <- input$postCP
            test = input$testP
            type = input$typeP
            alternative = input$alternativeP
            k = input$KP
            if(postfalse >= prior | prior >= posttrue)  {
                stop("Posterior after disconfirming needs to be smaller than prior, posterior after confirming larger")
            }
            alpha <- seq(.0001, 1, .001)
            #for 10-20 000 particpants calculte the alpha needed to achieve the posterior after a negative result
            #stop the loop when there is an option that at the same time has the needed posterior probability after a positive result
            for (i in 10:1000) {
                power <- get_power(alpha, effsize, i, test = test, type = type, alternative = alternative, k = k)
                #calculate power for all alphas for this sample size
                disconf <- postDisconfirm(power, alpha, prior)
                confirm <- postConfirm(power, alpha, prior)
                info <- information(confirm, disconf, power, alpha, prior)
                maxinf <- max(na.omit(info))
                df <- data.frame(alpha, power, disconf, confirm, info)
                postd <- as.data.frame(subset(df, disconf < postfalse))
                d <- subset(postd, confirm > posttrue)
                n <- i
                if(nrow(d) > 0) {
                    break
                }
            }
            d <- subset(d, info == max(info))
            d <- cbind(d,n)
            colnames(d) <- (c("alpha", "power", "disconf", "confirm", "info", "n"))
            paste("The needed alpha level is ", round(d$alpha, digits = 2), " with a power of ", round(d$power, digits = 2),
                  " and a required sample is is ", d$n, " participants per group. The actual posterior probabilites are ", round(d$confirm, digits = 2), 
                  " after significance and ", round(d$disconf, digits = 2), " after non-significance.", sep = "")
        })   
    }


# Run the application 
shinyApp(ui = ui, server = server)
