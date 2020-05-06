# Optimize_Alpha

The goal of my project is to develop several functions that can be used to justify the alpha level in a way that optimizes the informational value of studies or that results in specific posterior probabilities after a significant/non-significant result. 
I will first implement functions that can be used to calculate the prior and posterior probability after a significant/nonsignificant results according to Bayes theorem. In the next step, I will use this to make functions that can justify the alpha level based on optimizing informational value or based on aiming for specific posterior probabilities after a significant/non-significant result. I will implement these functions in an R package as well as a Shiny App. 

If I am short on time I will cut implementing the alpha level justifications based on posterior probabilities to the R package and Shiny app. On the other hand, if I have more time than expected I intend to add a function that combines justifying alpha levels with power analysis. 

## Code Description
The complete project will be programmed using R. To calculate the statistical power for different tests under different conditions I will use the pwr package.

Regarding the functions, I will first create a couple of "helper functions" to calculate posterior probabilities and informational value. I will then incorporate those in three different functions to justify the alpha based on (1) informational value, (2) posterior probability after a significant result, and (3) posterior probability after a non-significant result.
