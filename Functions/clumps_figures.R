clumps_figure<- function(body_size,abundance,posteriori,priori,species_code,main_graph='Clumpy coexistence',colour='black'){
  
  #'@body_size <- species body size without any log transformation
  #'@abundance <- can be a vector or matrix with species as rows and sites as columns.
  #'@traits <- functional traits of the species
  #'@FG <- Functional groups of the community 
  #'@species_code<- coding to identify species (e.g. numeric sequence or abbreviation) try a small one to fit in the graph
  
  #Testing parameters ----
  # species_code= data$Species_code
  # body_size=data$Volume
  # abundance=data$Biovolume
  # posteriori=data[,3:9]
  # priori=data$MBFG
  # plot= TRUE
  # main='test'
  # colour=Colour_pallete

  test<- clump_test(body_size=body_size,abun = abundance, n.clumps=NULL, rand=1000,cex.axis=1.5,cex=1.5, main='', plot=FALSE, give.results = TRUE)
  
  attach(test)
  clump_range<-as.data.frame(matrix(NA,nrow=length(which(significance == "*")), ncol=3))
  colnames(clump_range) <- c('Clump','min','max')
  for (i in 1:length(which(significance == "*"))){
    
    clump_range$Clump[i] <- paste0("C", i)
    clump_range$min[i]<- clump[which(significance == '*')[i]]  
    clump_range$max[i]<- clump[which(significance == '*')[i]+1]
  }
  detach(test)
  
  trait_clumps<-list()
  for ( i in 1:nrow(clump_range)){
    select.trait <- posteriori[log2(body_size) >= clump_range[i,2] &
                                 log2(body_size) < clump_range[i,3],]
    
    select.abun <-abundance[log2(body_size) >= clump_range[i,2] &
                              log2(body_size) < clump_range[i,3]]
    
    select_species<-species_code[log2(body_size) >= clump_range[i,2] &
                                    log2(body_size) < clump_range[i,3]]
    
    if (is.null(priori)) {
      select.priori<- rep('A',length(select.abun))} else{
        select.priori<- priori[log2(body_size) >= clump_range[i,2] &
                                 log2(body_size) < clump_range[i,3]]}
    
    trait_clumps[[i]]=data.frame(spp= select_species,
                                 abun=select.abun,
                                 clump.abun=round(select.abun/sum(select.abun),2),
                                 select.trait, select.priori,
                                 clump=rep(clump_range$Clump[i],nrow(select.trait)))
    
  }
  trait_clumps <-do.call(rbind, trait_clumps)
  
  clumps_abundance<-trait_clumps$clump.abun # species abundances
  clumps_traits<- subset(trait_clumps, select=names(posteriori)) # species traits
  clumps_code <- trait_clumps$clump # species clumps
  overall_abundance<-trait_clumps$abun
  a.priori=trait_clumps$select.priori

  

  clumps_homogeneity<-betadisper(gowdis(clumps_traits[which(clumps_abundance >= 0),]), clumps_code[which(clumps_abundance >= 0)], 'centroid',add=TRUE)
  max_fdist<-aggregate(clumps_homogeneity$distances,list(clumps_code[clumps_abundance >= 0]), 'max')
  
  distinct<-list()
    for (i in 1:nrow(clump_range)){

      fdist<-clumps_homogeneity$distances

      clump_i<-which(clumps_code[clumps_abundance >= 0] == levels(clumps_code)[i])
    distinct[[i]]<- fdist[clump_i]/max_fdist[i,2]
  }


gg<- data.frame(clump=clumps_homogeneity$group, x=clumps_homogeneity$vectors[,1],y=clumps_homogeneity$vectors[,2])
centroids <- aggregate(cbind(x,y)~clump,data=gg,mean)
gg<-merge(gg,centroids,by="clump",suffixes=c("",".centroid"))

panel_2<- ggplot(gg) + 
  geom_segment(aes(x=x.centroid, y=y.centroid, xend=x, yend=y))+
  geom_point(aes(x=x,y=y, size=clumps_abundance*100, fill=a.priori), shape=21) +
  geom_label_repel(data=centroids, aes(x=x, y=y),label= centroids$clump) +
  xlab('Niche dimension 1')+ ylab('Niche dimension 2') +
  scale_fill_manual(values=Colour_pallete, drop=FALSE)+
  scale_shape_manual(values=c(21,22,23))+ labs(fill='MBFG', size='Abundance within clumps')+
 ggthemes::theme_base(base_family = 'serif')+
  theme(legend.position='bottom', legend.box = 'vertical', rect= element_blank(),
        plot.title = element_text(family = "serif", size = 12, margin=margin(0,0,0,0)))+
  guides(fill=guide_legend(nrow=1))

panel_3<-trait_clumps  %>% arrange(desc(clump.abun)) %>% group_by(clump) %>% slice(1:5) %>%
ggplot(aes(x=factor(spp),y=clump.abun, fill=select.priori))+
  geom_bar(stat='identity', width=.5, colour='black' )+ coord_flip()+
  facet_wrap(~clump,scales = 'free_y', ncol=1)+
  scale_fill_manual(values=Colour_pallete, drop=FALSE)+ 
  ggthemes::theme_base(base_family  ='serif')+ ylab(expression("Mean Biovolume [ "*mu*"m"^3*" L"^-1*"]"))+
  xlab('Species code')+
  theme(legend.position='none',  strip.background = element_blank(),
        strip.text = element_text(size=8), rect=element_blank())+ labs(fill='MBFG')+
  guides(color=guide_legend("MBFG"), fill = FALSE)

final_plot<- (wrap_elements(panel = ~clump_test(body_size=body_size,
                                               abun=abundance,
                                               n.clumps=NULL,
                                               rand=1000,
                                               cex.axis=1,
                                               cex=1.3,
                                               main='',
                                               plot=TRUE), clip = FALSE))|
  panel_2 |
  panel_3 +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom',legend.box = "vertical")
return(final_plot)
}

