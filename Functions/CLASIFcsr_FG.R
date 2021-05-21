## FITO CLASIF BONILLA		
#Cargo la matriz de datos y le llamo data
CLASIFcsr_FG<-function(filename ,Pos=list(vol, SV, MLD, Flagella,Silice, Mucilage,Aerotopes)){#,saveTable=TRUE, plot=FALSE, savePlot=FALSE){

data<-read.table(filename,header=TRUE, dec=".", sep=";", row.names=1) ## "ParaClasifNew5%.csv"

# Check variables are correct (see max, min, etc.)
#summary(data)

#Create vectors from matrix
Volume <- data[,Pos$vol]
SV     <- data[,Pos$SV]
MLD    <- data[,Pos$MLD]
Flagella <- data[,Pos$Flagella]
Silice   <- data[,Pos$Silice]
Mucilage <- data[,Pos$Mucilage]
Aerotopes<- data[,Pos$Aerotopes]

# Create a vector for group clasification
FUNC_GROUP <- vector("numeric",length=nrow(data))
STRATEGY <- vector("numeric",length=nrow(data))
TAXO<- vector()
DIATO<-vector()
#Estrategia	BIO	S/V	MLD
Rmin<-c(	5000,	0.3,	80)
Rmax<-c(	100000,	2,	300)
Crmin<-c(	500,	0.3,	10)
Crmax<-c(	5000,	2,	80)
Cmin<-c(	5,	0.3,	3)
Cmax<-c(	500,	3,	10)
Cmin2<-c(	500,	2,	3)
Cmax2<-c(	5000,	3,	80)
Smin<-c(	10000,	0.03,	30)
Smax<-c(	10000000, 0.3,	500)

#codigo estrategias R=1 CR=2 C=3 S=4

for (i in 1:nrow(data)){
a=i
#Taxonomic group
#selTAXO<-which(data[a,7:15]!=0)+6
#TAXO[a]<-colnames(data)[selTAXO]
#if (colnames(data[selTAXO])=="DIATO") {selDIATO<-which(data[a,16:17]!=0); DIATO[a]<- colnames(data)[selDIATO+15]} else #DIATO[a]<-NaN 

#CLASIF REYNOLDS STRATEGYS
if (Volume[a]>Rmin[1]  & Volume[a]<Rmax[1]  & SV[a]>Rmin[2]  & SV[a]<Rmax[2]  & MLD[a]>Rmin[3]  & MLD[a]<Rmax[3])  STRATEGY[a]=1
if (Volume[a]>Crmin[1] & Volume[a]<Crmax[1] & SV[a]>Crmin[2] & SV[a]<Crmax[2] & MLD[a]>Crmin[3] & MLD[a]<Crmax[3]) STRATEGY[a]=2
if (Volume[a]>Cmin[1]  & Volume[a]<Cmax[1]  & SV[a]>Cmin[2]  & SV[a]<Cmax[2]  & MLD[a]>Cmin[3]  & MLD[a]<Cmax[3])  STRATEGY[a]=3
if (Volume[a]>Cmin2[1] & Volume[a]<Cmax2[1] & SV[a]>Cmin2[2] & SV[a]<Cmax2[2] & MLD[a]>Cmin2[3] & MLD[a]<Cmax2[3]) STRATEGY[a]=3
if (Volume[a]>Smin[1]  & Volume[a]<Smax[1]  & SV[a]>Smin[2]  & SV[a]<Smax[2]  & MLD[a]>Smin[3]  & MLD[a]<Smax[3])  STRATEGY[a]=4

#CLASIF FUNC GROUPS
if (Flagella[a]==1 & Silice[a]==1) {FUNC_GROUP[a]=2}
if (Flagella[a]==1 & Silice[a]==0 & MLD[a]< 2) {FUNC_GROUP[a]=1}
if (Flagella[a]==1 & Silice[a]==0 & MLD[a]> 2) {FUNC_GROUP[a]=5}

if (Flagella[a]==0 & Silice[a]==1) {FUNC_GROUP[a]=6}

if (Flagella[a]==0 & Silice[a]==0 & Mucilage[a]==1 & Aerotopes[a]==1 & SV[a]> 0.6) {FUNC_GROUP[a]=3}
if (Flagella[a]==0 & Silice[a]==0 & Mucilage[a]==1 & Aerotopes[a]==1 & SV[a]<= 0.6) {FUNC_GROUP[a]=7}

if (Flagella[a]==0 & Silice[a]==0 & Mucilage[a]==0 & Volume[a]< 30 & MLD[a]<= 20) {FUNC_GROUP[a]=1}
if (Flagella[a]==0 & Silice[a]==0 & Mucilage[a]==0 & Volume[a]< 30 & MLD[a]> 20) {FUNC_GROUP[a]=4}

if (Flagella[a]==0 & Silice[a]==0 & Mucilage[a]==1 & Aerotopes[a]==0 & Volume[a] <= 10) {FUNC_GROUP[a]=1}
if (Flagella[a]==0 & Silice[a]==0 & Mucilage[a]==1 & Aerotopes[a]==0 & Volume[a]> 10) {FUNC_GROUP[a]=7}

if (Flagella[a]==0 & Silice[a]==0 & Mucilage[a]==0 & Volume[a]> 30 & Aerotopes[a]==1) {FUNC_GROUP[a]=3}
if (Flagella[a]==0 & Silice[a]==0 & Mucilage[a]==0 & Volume[a]> 30 & Aerotopes[a]==0) {FUNC_GROUP[a]=4}
}

STRATEGY[STRATEGY==1]<- "R"
STRATEGY[STRATEGY==2]<- "CR"
STRATEGY[STRATEGY==3]<- "C"
STRATEGY[STRATEGY==4]<- "S"
STRATEGY[STRATEGY==0]<- "NC"

CLASIF <- cbind(data,STRATEGY,FUNC_GROUP)
CLASIF
}
