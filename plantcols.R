plantcols <- function (n,
                       cont=FALSE,
                       name=c("atabapoese", "azlanii", "aurea", "blueoilfern", "brevirimosa", "cebublue", "confetti", "florida.bronze", "floraida.ghost", "painted.lady", "pinkprincess", "prince.of.orange",
                              "raven.var", "strawberry.ice", "strawberry.shake", "thai.constellation", "verrucosum","zebrina")){
  raven.var=rgb(c(45,198,139,98,186), c(35,116,93,65,151), c(32,101,87,62,151), maxColorValue=255)
  painted.lady=rgb(c(97,164,175,133,219), c(130,49,190,160,216), c(37,72,71,60,116), maxColorValue=255)
  cebublue=rgb(c(16,171,92,61,127), c(27,218,136,95,177), c(27,214,126,79,167), maxColorValue=255)
  brevirimosa=rgb(c(52,241,109,25,208), c(83,139,0,52,90), c(39,184,49,0,130), maxColorValue=255)
  verrucosum=rgb(c(116,191,97,49,139), c(103,211,128,67,178), c(45,94,11,22,36), maxColorValue=255)
  strawberry.shake=rgb(c(66,241,214,133,249), c(88,180,188,160,216), c(0,128,139,60,188), maxColorValue=255)
  prince.of.orange=rgb(c(20,216,177,133,204), c(87,85,191,160,181), c(23,26,22,60,30), maxColorValue=255)
  blueoilfern=rgb(c(3,135,13,5,26), c(34,213,117,73,191), c(39,218,159,88,232), maxColorValue=255)
  thai.constellation=rgb(c(27,240,133,72,252), c(74,240,160,97,249), c(34,212,60,34,185), maxColorValue=255)
  pinkprincess=rgb(c(27,237,151,33,198), c(74,180,142,54,137), c(34,206,135,34,153), maxColorValue=255)
  florida.ghost=rgb(c(72,240,169,97,216), c(97,240,193,130,223), c(34,212,103,37,149), maxColorValue=255)
  florida.bronze=rgb(c(74,27,155,127,73), c(65,74,102,76,129), c(45,34,45,22,73), maxColorValue=255)
  zebrina=rgb(c(48,206,108,121,130), c(28,210,156,100,157), c(8,184,81,65,9), maxColorValue=255)
  aurea=rgb(c(0,240,155,58,97,244), c(82,227,220,156,141,247), c( 0,4,36,1,4,155), maxColorValue=255)
  confetti=rgb(c(224,230,146,199,219), c(228,199,162,215,154), c(179,180,81,116,174), maxColorValue=255)
  atabapoese=rgb(c(100,72,161,134,97), c(71,97,203,78,141), c(80,34,56,74,4), maxColorValue=255)
  strawberry.ice=rgb(c(49,82,224,122,230), c(92,32,182,96,144), c(48,37,171,71,168), maxColorValue=255)
  azlanii=rgb(c(90,18,123,197,27), c(53,35,79,150,74), c(70,18,74,150,34), maxColorValue=255)
  
  name=match.arg(name)
  orig=eval(parse(text=name))
  
  #for continuous values, rearrange (initial) colors nicely
  if(cont == TRUE){
    orig<-orig[c(2,5,3,4,1)]
  }
  
  rgb=t(col2rgb(orig))
  
  #if n<5 return colors from initial palette, otherwise impute
  if(n <= length(orig)){
    #temp<- rgb[1:n, ]
    palette<- orig[1:n]
  }else{
    temp=matrix(NA, ncol=3, nrow=n)
    x=seq(0, 1, , length(orig))
    xg=seq(0, 1, , n)
    for (k in 1:3) {
      hold=spline(x, rgb[, k], n=n)$y
      hold[hold < 0] = 0 
      hold[hold > 255] = 255
      temp[, k]=round(hold)
    }
    palette=rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue=255)
  }
  palette
}