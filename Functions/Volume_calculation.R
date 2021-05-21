##############################################################################################################
##############################################################################################################
## Function to calculate phytoplankton volume based on geometric shapes according to Hillebrand et al 1999
##############################################################################################################
##############################################################################################################
# Function to calculate volume
#Inputs
# Shape- Forma geometrica a la cual aproximo el individuo (e.g. "Ellipsoid")
# Measures- Named Vector con las  Medidas requeridas para estimar ese volumen c(DimA= 12, DimB=9, DimC=29)

#Output
# Volume
# Surface ##Area

Volume_calculation<-function(x){ 
Shape= x$Shape
Measures= x[,2:7]
# According to Hillebrand et al 1999
Common_shapes<-c("Sphere","Prolate_spheroid", "Ellipsoid","Cylinder","Cone",
                 "Double_cone","Cone_half_sphere","Rectangular_box", "Prism_on_elliptic_base",
                "Prism_on_parallelogram_base", "cone_3cylinder", "cone_cylinder", "Double_cone_cylinder",
                "Gomphonemoid", "Double_cone_truncated", "Sickle_shaped_prism", "Cone_half_ellipsoid", "Cymbelloid","Cylindrer_half_sphere")

#Dimensiones<-c(1,2,3,2,3,3,3,3,3,3,6)# Dimensiones necesarias para calcular el volumen de esas formas geometricas
# Si la forma geometrica no esta entre las incluidas da un error

if(!Shape%in%Common_shapes) stop(paste("La forma geometrica", Shape ,"no coincide con ninguna de la base de datos"))

selShape<-which(Common_shapes==Shape) # Para elegir la forma

######################################################################################################################################################
Names<-names(Measures)

DimA<-Measures[which(Names=="DimA")]
DimB<-Measures[which(Names=="DimB")]
DimC<-Measures[which(Names=="DimC")]
DimD<-Measures[which(Names=="DimD")]
DimE<-Measures[which(Names=="DimE")]
DimF<-Measures[which(Names=="DimF")]

######################################################################################################################################################3
## Aqui estan las formula para calcular Volumen y Superficie de las diferentes figuras geométricas
if(selShape==1){#1 Sphere 
# DimA= d -diameter 
Vol= (pi/6)* DimA^3
Area= pi* DimA^2
} #sphere

if(selShape==2){#2- Prolate_spheroid

#DimA=d -diameter
#DimB=h -altura
Vol<- (pi/6)*DimB*DimA^2 # Hillebrand et al 99 Table 1
Area<- (pi*DimA/2) * (DimA + ((DimB^2 / sqrt(DimB^2 - DimA^2)) * asin(sqrt(DimB^2 -DimA^2)/DimB)))
} #Prolate_spheroid

if(selShape==3){# Ellipsoid
# DimA= a -diametro
# DimB= h- altura
#DimC= b - profundidad
Vol<-DimA*DimB*DimC*pi/6

p<-1.6075 #http://planetcalc.com/149/
t1<-(DimB/2)^p * (DimA/2)^p
t2<-(DimB/2)^p * (DimC/2)^p
t3<-(DimA/2)^p * (DimC/2)^p
Area<-4*pi*((t1+t2+t3)/3)^(1/p) #Knud Thomsen from Denmark  "surface ##Area of a general ellipsoid with a relative error < 1.42%"
} #Ellipsoid

if(selShape==4){#Cylinder
# DimA= d- diameter
# DimB= h - altura
Vol<- (pi/4)* DimA^2 * DimB 
Area<- pi* DimA* ((DimA/2)+DimB)
} #Cylinder

if(selShape==5){# Cone
# DimA= d -diameter
# DimB= h -altura
# DimC= l -lado
Vol<- (pi/12)*DimA^2 * DimB # Hillebrand etal 1999
Area<-(pi/2) *DimA * ((DimA/2) + sqrt((DimA/2)^2+DimB^2)) 
}

if(selShape==6){# Doble_cone
#DimA=d
#DimB=h
#DimC=l
Vol<-(pi/12)* DimA^2 * DimB
Area<-(pi/2) * DimA * sqrt(DimA^2+DimB^2)
Area
} #Double Cone

if(selShape==7){# Cone_half_sphere
#DimA= d -diameter
#DimB= z - altura (h)
#DimC= l - lado
Vol<- (pi/12)* DimA^2 * (DimB + DimA)
Area<- (pi/2)* DimA^2 * (DimA + sqrt((2*DimB^2-DimA*DimB + DimA^2)/2))
}

if(selShape==8){# Rectangular_box
# DimA= b -ancho
# DimB= a -larog
# DimC= c -profundidad
Vol<-DimA*DimB*DimC
Area<-2*DimA*DimB+ 2*DimB*DimC + 2*DimA*DimC
}

if(selShape==9){# Prism_on_elliptic_base
#DimA= b -ancho
#DimB= a -largo
#DimC= c -profundidad
Vol<- (pi/4)* DimA* DimB* DimC
Area<- (pi/2)*(DimA*DimB + (DimA+DimB)*DimC)
}

if(selShape==10){#Prism_on_parallelogram_base
#DimA=b - ancho
#DimB=a - largo
#DimC=c - profundidad
Vol<- DimA*DimB*DimC/2
Area<- DimB * DimA + (sqrt((DimB^2) + (DimA^2)) * 4/DimC)
}

if(selShape==11){# cone_3cylinder
#DimA=a1 - alto cono
#DimB=b1 - ancho cono
#DimC=a2 - Alto cilindro cola
#DimD=b2 - Ancho cilindro cola
#DimE=a3 - Alto patas
#DimF=b3 - Ancho patas
Vol<- (pi/4)*DimC*DimD^2 + (pi/2)*DimE* DimF^2 + (pi/12)* DimA * (DimB^2 + DimB*DimD + DimD^2)
Area<- (pi/2)*(DimB+DimD) * sqrt((DimA^2) + ((DimB-DimD)/2)^2 ) + (pi/4)*(DimB^2 + DimD^2) + 2*pi*(DimC*DimD + DimE*DimF) # REVISAR ESTA FORMULA!!

}

if(selShape==12){# cone_cylinder
  #DimA=a1 - alto cono
  #DimB=b1 - ancho cono
  #DimC=a2 - Alto cilindro
  #DimD=b2 - Ancho cilindro
  L=sqrt((DimB/2)^2 + DimA^2)
  Vol<- (pi/12)*DimA^2 * DimB  + (pi/4)* DimD^2 * DimC 
Area<- pi*(DimB/2)*(DimB/2 + L) + pi * DimD * (DimD/2+DimC)# REVISAR ESTA FORMULA!!

}

if(selShape==13){#Double_cone_cylinder  From Sun & Liu 2003
  #DimA= diameter
  #DimB= height

  Vol<- (pi/4)*DimA^2 * (DimB - (DimA/3)) 
  Area<- pi * DimA * (DimB - ((4-sqrt(3))/4)*DimA)
}

if(selShape==14){#Gomphonemoid  From Sun & Liu 2003
  #DimA= diameter
  #DimB= height
  #DimC= Width
  Vol<- DimB*DimA/4 * (DimB  + (pi/4 - 1) * DimA) * asin(DimC/(2*DimB)) 
  Area<- DimA/2 * (DimB*2 + pi*DimB* asin(DimC/(2*DimB)) + (pi/2 - 2) * DimA )
}

if(selShape==15){#Double_cone_truncated  From Sun & Liu 2003
  #DimA= Diameter1
  #DimB= Diameter2
  #DimC= Height
  #DimD= l
  Vol<- 2*(1/3*pi * ((DimA/2)^2 + ((DimA/2)+(DimB/2)) + (DimB/2)^2) * DimC) 
  Area<- 2*(pi * ((DimA/2)+(DimB/2)) * DimD)
}

if(selShape==16){#Sickled_shaped_cylinder  From Sun & Liu 2003
  #DimA= diameter
  #DimB= height
  
  Vol<- (pi/4)*DimA * DimB * DimC 
  Area<- (pi/4)* (DimB * DimA + DimA * DimC + DimB * DimC) + DimB * DimC
}

if(selShape==17){# Cone_half_ellipsoid
  #DimA= d -diameter
  #DimB= z - altura (h)
  #DimC= l - lado
  Vol<- (pi/6* DimA^2 *DimB)/2  + (pi/12)*DimA^2 * DimA/2
  Area<- ((pi*DimA/2) * (DimA + ((DimB^2 / sqrt(DimB^2 - DimA^2)) * asin(sqrt(DimB^2 -DimA^2)/DimB))))/2 +
        pi/4 * DimA * DimA + sqrt(DimA^2 + (4*(0.5*DimA)^2 + DimA^2))
}

if(selShape==18){#Cymbelloid  From Sun & Liu 2003
  #DimA= diameter
  #DimB= height
  #DimC= Width
  Vol<- 2/3*DimB * DimC^2 * asin(DimA/(2*DimC)) 
  Area<- pi/2*DimB*DimC+DimA*(DimC+DimB^2 / (2*sqrt(DimB^2-4*DimC^2)))*asin(sqrt(DimB^2-4*DimC^2)/DimB)
}

if(selShape==19){#1 Cylinder_half_Sphere    
  
  # DimA= d -diameter 
  #DimB = diametro cylinder
  #DimC= height cylinder
  Vol= 2/((pi/6)* DimA^3) +  (pi/4)* DimB^2 * DimC 
  Area= 2/(pi* DimA^2) + pi* DimB* ((DimB/2)+DimC)
  
}

AREAVOL=cbind(Vol,Area); colnames(AREAVOL) <- c("Vol", "Area"); rownames(AREAVOL) <- rownames(x)
  return(AREAVOL)
} # End Function

