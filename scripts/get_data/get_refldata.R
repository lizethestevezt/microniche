get_refldata <- function(landcover, lctype = "ESA", alb, lai){
  x <- x_calc(landcover = landcover, lctype = "ESA")

  refldata <- reflectance_calc( alb = alb, lai = lai, x = x, maxiter = 75, 
                              tol = 0.001, bwgt = 0.7)
  return(refldata)}