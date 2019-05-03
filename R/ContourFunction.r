
PlotContour<- function(Data,Probability,BandWidthX,BandWidthY,Colour){
  KernEstimPost<-bkde2D(Data,bandwidth=c(BandWidthX,BandWidthY))
  prob <- Probability
  dx <- diff(KernEstimPost$x1[1:2])
  dy <- diff(KernEstimPost$x2[1:2])
  sz <- sort(KernEstimPost$fhat)
  c1 <- cumsum(sz) * dx * dy
  levels <- sapply(prob, function(x) {
    approx(c1, sz, xout = 1 - x)$y})
  contour(KernEstimPost$x1,KernEstimPost$x2,KernEstimPost$fhat,col=Colour,levels=levels,labels="",labcex=0.01,add=TRUE)
}

