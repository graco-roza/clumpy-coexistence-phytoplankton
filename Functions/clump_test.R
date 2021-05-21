clump_test<-function(body_size, abun, rand, n.clumps=NULL,y.limits=c(0,max(rowMeans(abundance, na.rm=TRUE))),col=1,cex.axis=1,cex=1,cex.main=1, give.results=FALSE, plot=TRUE,main)

  {
  #abundance - matriz con el biovolumen de cada especie en los diferentes muestreos de un mismo sistema {nrow(abundance)=length(body_size)}
  # rand - Numero de veces que aleatorizo la matriz para generar el modelo nulo
  
  if(length(n.clumps)<1){ n.clumps<- ceiling(max(log2(body_size),na.rm=T)) - floor(min(log2(body_size),na.rm=T)) +1 ; warning("!!! number of clumps automatically defined !!!")}
  
  ## Si abundance es un vector

  if (is.vector(abun)) abundance<-data.frame(abun,abun) else abundance <- abun
  
  ## ENTROPIA OBSERVADA
    # Housekeeping
  if (any(body_size==0,na.rm=T)) body_size[which(body_size==0)]<-NaN
  
  if(length(n.clumps)<1) n.clumps<- nclass.Sturges(log2(body_size))
  ##  ty
  S<-numeric()
  p<-rowMeans(abundance, na.rm=TRUE)/ sum(rowMeans(abundance),na.rm=T) # Calculo las fracciones de cada especie
  
  # Generating clumps
  
  clumps<- round(seq(floor(min(log2(body_size),na.rm=T)),ceiling(max(log2(body_size),na.rm=T)),length.out=n.clumps),2)
  
  # estimate entropy values for each clump
  for (i in 1:(length(clumps)-1)){ 
    #Selecting the species within the clump
    selVal<-which(log2(body_size)> clumps[i] & log2(body_size)<= clumps[i+1])
    
    S[i]<- -sum(p[selVal]* log2(p[selVal]),na.rm=T)
  }
  
  Entropy_obs<-c(S,S[length(S)])
  
  # Define the matrix for the randomized entropy 
  Entropy_random_mat<-matrix(NaN,nrow=rand,ncol=length(clumps))

  set.seed(69)
  for (jj in 1:rand){
    randVol<-runif(nrow(abundance),min(clumps),max(clumps))

    randMAT<-matrix(NaN,nrow=nrow(abundance),ncol=ncol(abundance))
    
    for (ii in 1:ncol(abundance)){
      randMAT[,ii]<-abundance[sample(1:nrow(abundance)),ii]
    }
    randMAT
    randMean<-rowMeans(randMAT, na.rm=T)
    pRand<-randMean/sum(randMean, na.rm=T)
    
    Srand<-numeric()
    
    for (i in 1:(length(clumps)-1)){ # Calcula entropia por clase
      #Selecciona especies que estan en la clase determinada
      selVal<-which(randVol> clumps[i] & randVol<= clumps[i+1])
      
      fracRand<-pRand[selVal]
      Srand[i]<- -sum(fracRand[fracRand>0]* log2(fracRand[fracRand>0]))
    }
    
    Entropy_random<-c(Srand,Srand[length(Srand)])
    
    Entropy_random_mat[jj,]<-Entropy_random
  }
  
  #Test
  # Criterio de decision: Si ESTAT > 95 o ESTAT < 5 Entonces la entropia para esa clase observada es significativamente MENOR o MAYOR que la entropia generada aleatoriamente
  probability<-numeric()
  
  for (A in 1:length(clumps)){
    probability[A]<-length(which(Entropy_random_mat[,A]> Entropy_obs[A]))/rand # valores mayores que el observado
  }
  significance <- sapply(probability, function(x) {ifelse(x <= 0.05,"*","")})
  significance[n.clumps] <- "" #The two last clumps are exactly the same, we thus removed one

  results<-list()
  results$clump<-clumps
  results$significance<-significance
  results$entropy<-Entropy_obs
  ## PLOT
if(plot == TRUE){
  abund<-rowMeans(abundance,na.rm=TRUE)

  par(mar = c(0,3,0,2))
  plot(log2(body_size), abund , xlim=c(2,15), ylim=y.limits,
       xlab='', ylab='',cex.axis=cex.axis, cex.main=2,
       type="h", col='black',
       cex.main=1, xaxt='n', lwd=2, lend=2)
  mtext(expression(bold("Biovolume \nsize-distribution")),3,line=1.5,adj=0,at=1.5,cex=1.2)
  mtext(expression("Mean biovolume (mm"^3*" L"^-1*")"),side=2,line=2, cex=cex)
  mtext(expression(Log[2]*" volume"),1,2, cex=cex)
  
  par(new=TRUE)
  plot(clumps, Entropy_obs, type="s", xlim=c(2,15), ylim=c(0,max(Entropy_obs)*1.1), lty=2 , lwd=2, col="red", axes=FALSE, xlab="",ylab="")
  text(clumps+0.5,Entropy_obs+0.04, significance, col='black', cex=3)
  axis(1, tck=-0.015, label = rep("", 7), at=seq(2, 15 ,2) ,cex.axis=cex.axis)
  axis(1, lwd=0, line=-.7, at=seq(2, 15 ,2),cex.axis=cex.axis)
  axis(4, tck=-0.015, at=seq(0,2,.5), label = rep("", 5),  cex.axis=cex.axis)
  axis(4, lwd=0, line=-.7, at=seq(0,2,.5), cex.axis=cex.axis)
  mtext(expression("Entropy (bits ind."^-1*")"), 4, line=1.5,cex=cex)

}  
 if(give.results==TRUE) return(results)
}





