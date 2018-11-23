X<- rnorm(1000)
hist3Dnew<- function(X, alpha, beta){
  Xd <- hist(X,plot=F, breaks=10)
  
  values<-Xd$counts/max(Xd$counts)*50
  out<-c()
  outvalues<-c(0)
  width<-5
  widthiter<- 0
  for (x in values){
    
    out<-c(out,widthiter,widthiter+0.0001)
    outvalues<-c(outvalues,x,x)
    widthiter<-widthiter+width
  }
  out<-c(out,widthiter,widthiter+0.0001)
  outvalues<-c(outvalues,0 )
  z<-matrix(0,ncol=10,nrow=length(outvalues))
  
  z[,c(5,6)]<-outvalues
  
  y<-seq(0,40,5)
  y<-c(y[1:4],y[4]+0.0001, y[5],y[5]+0.0001,  y[6:8])
  persp( out,y, z, theta = alpha, phi = beta, col = "green3", scale = FALSE,
        ltheta = -120, shade = 0.75, box=FALSE)
  
}
  
hist3Dnew(X, 135, 30)
