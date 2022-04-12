#Analisis Regresi PDRB
data = read.csv("Analisis Regresi.csv", sep = ";")
#Melihat bagian atas data
head(data)

library(ggplot2)
p1 <-ggplot(data, aes(x=data$x1, y=data$y))+geom_point()+stat_smooth()
p2 <-ggplot(data, aes(x=data$x2, y=data$y))+geom_point()+stat_smooth()
p3 <-ggplot(data, aes(x=data$x3, y=data$y))+geom_point()+stat_smooth()
gridExtra::grid.arrange(p1,p2,p3,ncol=3)

#Membentuk vektor y
y	<- as.matrix(data[,1])
y

#Membentuk matriks X
##Kolom matriks X dari data
X0	<- as.matrix(data[,2:4])
X0

##Menentukan banyak unit pengamatan (baris dari vektor y) 
n	<- nrow(y)
n

##Membentuk vektor unit konstan
u	<- matrix(c(1),n,1)
X	<- cbind(u,X0)
X

#Taksiran kodefisien regresi
b	<- solve(t(X)%*%X)%*%t(X)%*%y
b

#Taksiran standar error
#Matriks Hat (H)
H	<- X%*%solve(t(X)%*%X)%*%t(X)
H

##Membentuk matriks identitas (nxn)
I	<- diag(c(1),n,n)
I

##Menghitung Sum Square Residual
SSE	<- t(y)%*%(I-H)%*%y
SSE

##Menghitung banyak regresor+1
p	<- ncol(X)
p

##Menghitung taksiran varians kekeliruan (MSE)
MSE	<- SSE/(n-p)
MSE
s	<- sqrt(MSE)
s

##Menghitung MSR (taksiran variansi regresi)
J <- matrix(c(1),n,n)
SSR <- t(y)%*%(H-(1/n)*J)%*%y
SSR
MSR <- SSR/(p-1)
MSR

## Residual
e	<- (I-H)%*%y
e

##Mencari SSTO
SSTO <- t(y)%*%(I-(1/n)*J)%*%y
SSTO

## UJI F
# H0 : b1=b2=..=bk=0, H1 : b tidak sama dengan 0
# tolak H0 jika Fhitung>Ftabel, ada pengaruh yang signifikan antara variabel independen dengan variabel dependen secara bersama-sama
Fhitung <- MSR/MSE
Fhitung
Ftabel <- qf(.05,p-1,n-p)
Ftabel
{
  if (Fhitung>Ftabel){hasilF="menolak H0"}
  else if (Fhitung<Ftabel){hasilF="menerima H0"}
}
print(hasilF)

##UJI t
# H0 : b1=b2=..=bk=0, H1 : b tidak sama dengan 0
#if - t table < t stat < + t table, then Ho is accepted, and the parameter is not significant
c <- as.numeric(diag(solve(t(X)%*%X)))
thitung <- (b)/(as.numeric(s)*sqrt(c))
thitungabs <- abs(as.numeric(thitung))
ttabel <- qt(1-0.05/2,n-p)
thitung
ttabel
hasilt <- function(i){
  for(i in thitungabs){
  if(i>ttabel){
    print("menolak H0")
  }else{
    print("menerima H0")
  }
  }
}
hasilt(i)


#model regresi instant
regresi <- lm(y~X0)
regresi
summary(regresi) 
#jika p-val<0.005  H0 ditolak, ada hubungan yang signifikan antara variabel X dan variabel Y


##Mencari koleniaritas
#multikoleniaritas menyebabkan estimasi tidak stabil karna meingkatkan varian koef regresi
#mencari VIF
R=cor(X0)
R
VIF=diag(solve(R))
VIF
reg <- lm(y~., data = data)
library(car)
vif(reg)
#Mencari det(X'X), misalkan det(X'X) #dibawah 5 tidak ada multikoleniaritas/ tidak ada hubungan antar variabel x
d=det(t(X)%*%X) #mendekati 0 koleniaritas semakin kuat
d

##Outlier dan influental observation

## Semistudentized residual
es	<- e/s[1,1];es
s

## studentized residual
r	<- e/matrix(c(s),n,1)/sqrt(diag(I-H)) 	#s dijadikan matrik (nX1)
r1	<- e/s[1,1]/sqrt(diag(I-H))			#s dijadikan skalar		
r

## deleted residual
d	<- e/diag(I-H)
d

## studentized deleted residual
# jika menerima H0 maka yi bukanlah outlier
MSEi	<- (matrix(c(SSE),n,1)-e*e/diag(I-H))/(n-p-1)
MSEi
vd	<- MSEi/diag(I-H)
vd
t	<- d/sqrt(vd)
alpha	<- 0.05
t_cri	<- qt(1-alpha/n/2,n-p-1)
## Uji hipotesis
#H0 : yi bukan outlier
#H1 : yi outlier
for(thitung in t){
  if (thitung > t_cri){
    print("menolak H0")
    }else{
        print("menerima H0")
        }
}
#atau
thit <- e*(((n-p-1)/(as.vector(SSE)*diag(I-H)-e^2)))
hasilt <- function(k){
  for(k in thit){
    if(k>t_cri){
      print("menolak H0")
    }else{
      print("menerima H0")
    }
  }
}
hasilt(k)

## leverage 
#semakin besar leverage, semakin kecil 2
#melihat outlier pada variabel x
h	<- diag(H)
h_cri	<-2*p/n
h
for(hii in h){
  if (hii > h_cri){
    print("outlier")
    }else{
      print("bukan outlier")
    }
  }


## DFFITS
#melihat pengamatan ke i sebagai pencilan berpengaruh terhadap model regresi yang ditinjau dari fitnya.
#melihat pengaruh pencilan terhadap estimasi nilai Y masing"
DFF	<- t*h/diag(I-H)
#untuk jumlah sampel kecil
DFFcri<-1
#untuk jumlah sampel besar
DFFcri2<- 2*sqrt(p/n)
for(DFFi in DFF){
  if (abs(DFFi) > DFFcri){
    print("influential")
    }else{
      print("not influential")
    }
  }


## Cook's Distance
#Melihat pengamatan ke i sebagai pencilan berpengaruh terhadap model regresi
#melihat pengaruh pencilan terhadap estimasi nilai Y keseluruhan
D	<- (e^2/p/MSE[1,1])*(h/diag(I-H)^2)
Dcri	<- qf(0.5,p,n-p)
for(Di in D){
  if (Di > Dcri){
    print("influential")
    }else{
     print("not influential")
   }
  }

## DFBETAS
#Melihat berapa pengamatan ke i sebagai pencilan berpengaruh terhadap koefisien regresi ke j
DFB	<- matrix(c(0),n,p)	#individual koefisien regresi
Da	<- matrix(c(0),n,1)	#semua koefisien regresi

for (i in 1:n){
  yi	<- y[-i]
  Xi	<- X[-i,]
  
  bi	<- solve(t(Xi)%*%Xi)%*%t(Xi)%*%yi
  
  #Matriks Hat (H)
  Hi	<- Xi%*%solve(t(Xi)%*%Xi)%*%t(Xi)
  
  ##Membentuk matriks identitas (nxn)
  Ii	<- diag(c(1),n-1,n-1)
  
  ##Menghitung Sum Square Residual
  SSEi	<- t(yi)%*%(Ii-Hi)%*%yi
  
  ##Menghitung taksiran varians kekeliruan (MSE)
  s2i	<- SSEi/(n-p-1)
  si	<- sqrt(s2i)
  
  DFBi	<- (b-bi)/si[1,1]/sqrt(diag(solve(t(X)%*%X)))
  DFB[i,]	<- DFBi
  
  Dai	<- t(b-bi)%*%t(X)%*%X%*%(b-bi)/p/MSE[1,1]
  Da[i,]	<- Dai
}
DFB
#untuk sample kecil
DFBcrik<-1
#untuk sampel besar
DFBcri	<- 2/sqrt(n)
#koefisien ke 1
for(z in DFB[,1]){
  if (z > DFBcri){
     print("influential")
   }else{
     print("not influential")
   }
}
#koefisien ke 2
for(z in DFB[,2]){
  if (z > DFBcri){
    print("influential")
  }else{
    print("not influential")
  }
}
#koefisien ke 3
for(z in DFB[,3]){
  if (z > DFBcri){
    print("influential")
  }else{
    print("not influential")
  }
}
#koefisien ke 4
for(z in DFB[,4]){
  if (z > DFBcri){
    print("influential")
  }else{
    print("not influential")
  }
}

#melihat pengamatan ke i sebagai pencilan berpengaruh terhadap semua dugaan koefisien regresi
Da
Dacri		<- Dcri
for(z in Da){
   if (z > Dacri){
     print("influential")
   }else{
     print("not influential")
   }
 }

