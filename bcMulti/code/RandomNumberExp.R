
get.workaround.rnd.number <- function(iteration = 0) {
  rnd.num <-  sample(seq(from=0.1, to=0.99, by=0.1), size=1, replace=T, prob=c(0.01,0.02,0.04,0.08,0.16,0.32,0.64,1.28,2.56))
  return(rnd.num)
}