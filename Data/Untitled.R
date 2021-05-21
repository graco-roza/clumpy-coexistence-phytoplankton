par(mfrow=c(2,2))

ErPB<-Entropy_size(vol,biovol,lwd=sz, Nclas=14, col='black', main= titulo ,col.g=all, plot=TRUE)

plot.beta(red.func, main='', group2=pch.clump, group3=trait.clumps$Clump, pch=c(16,15),
          col=PB_color2, label=FALSE,cex=beta.cex, lwd=1, seg.col='black', lty=1,
          seg.lty=3,, xlab='PCoA 1', ylab='PCoA 2',cex.lab = 1.5, cex.axis=1.4)

barplot(higher.log6$Biomass,  names.arg=rownames(higher.log6),
        font.axis=1, xlim=range(pretty(c(0,max(biovol)))), cex.main=1.5,
        horiz= TRUE, las=1, cex.names=.9 , col=PB_color2[higher.log6$MBFG], 
        border='black', beside=TRUE, main= "Clump I")

barplot(higher.log10$Biomass,  names.arg=rownames(higher.log10),
        font.axis=1, xlim=range(pretty(c(0,max(biovol)))), xlab='Biomass (%)',
        horiz= TRUE, las=1, cex.names=.9 , col=PB_color2[higher.log10$MBFG],
        border='black', cex.main=1.5, cex.lab = 1, beside=TRUE,main='Clump II')


par(mfrow=c(1,1))
# make an original plot
plot( 11:20, sample(51:60) )

# add some histograms

subplot( hist(rnorm(100)), 15, 55)
subplot( hist(runif(100),main='',xlab='',ylab=''), 11, 51, hadj=0, vadj=0)
subplot( hist(rexp(100, 1/3)), 20, 60, hadj=1, vadj=1, size=c(0.5,2) )
subplot( hist(rt(100,3)), c(12,16), c(57,59), pars=list(lwd=3,ask=FALSE) )
