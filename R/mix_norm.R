

#'  Mixture Normal Distribution
#'
#'  Generate n observations from a mixture of two normal distributions: generate U from a Bernoulli(p) distribution
#'  and if U=1, then draw Y from a normal distribution with mean mu1 and standard deviation sigma1, or if U=0, draw
#'  Y from the other normal distribution with mean mu2 and standard deviation sigma2
#'
#' @param p numeric, probability that Y is drawn from a normal distribution N(mu1,sigma1)
#' @param mu1 numeric, mean of first normal distribution
#' @param mu2 numeric, mean of second normal distribution
#' @param sigma1 numeric, standard deviation of first normal distribution
#' @param sigma2 numeric, standard deviation of second normal distribution
#'
#' @return
#' @export
#'
#' @examples
#' mix_norm(0.5,-2,3,2,1.5)

mix_norm <- function(p,mu1,mu2,sigma1,sigma2){

  y = rbernoulli(1,p)
  if (y == TRUE){
    z = rnorm(1, mean = mu1, sd = sigma1)
  }
  else {
    z = rnorm(1, mean = mu2, sd = sigma2)
  }
  return(z)
}
