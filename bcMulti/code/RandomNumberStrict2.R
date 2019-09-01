get.workaround.rnd.number <- function(iteration = 0) {
  rnd.num <-  sample(c(0.2,0.6,0.7,0.75,0.8,0.85,0.9,0.95,0.99), size=1, replace=T, prob=c(0.01,0.02,0.04,0.08,0.16,0.32,0.64,1.28,2.56))
  return(rnd.num)
}