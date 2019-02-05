#Implementation
#Question 1
data = read.csv("~/Downloads/sp500hst.txt", header = TRUE)
BAC_DATA <-subset(data, Ticker=="BAC")
n1=11
n2=22
beta1=2/(n1+1)
beat2=2/(n2+1)
FAST<-EMA(BAC_DATA[,6],n=n1)
SLOW<-EMA(BAC_DATA[,6],n=n2)
MACD<-FAST-SLOW
SIGNAL<-EMA(MACD,n=9)
ts.plot(cbind(MACD,SIGNAL),col=c("black","red"))

#RSI Method
L=20
H=80
RSITRADING<-RSI(BAC_DATA[,6])
ts.plot(cbind(L,H,RSITRADING))

#Head&Shoulder Method
AA_DATA <- subset(data,Ticker=="AA")
AA_DATA<-AA_DATA[,6]
AVG<- mean(AA_DATA)
E3<-which.max(AA_DATA)
E1<-E2<-E4<-E5<-0
for (i in 1:length(AA_DATA)){
  if((AA_DATA[i]>(0.985*AVG)) && (AA_DATA[i]<(AVG*1.015)) && (i<E3)){
    E2 <- i
  }
}
for (i in 1:length(AA_DATA)){
  if((AA_DATA[i]>(0.985*AVG)) && (AA_DATA[i]<(AVG*1.015)) && (AA_DATA[i]>AA_DATA[E2]) && (i<E2)){
    E1 <- i
  }
}
for (i in 1:length(AA_DATA)){
  if((AA_DATA[length(AA_DATA)-i]>(0.985*AVG)) && (AA_DATA[length(AA_DATA)-i]<(AVG*1.015)) && ((length(AA_DATA)-i)>E3)){
    E4 <- (length(AA_DATA)-i)
  }
}
for (i in 1:length(AA_DATA)){
  if((AA_DATA[length(AA_DATA)-i]>(0.985*AVG)) && (AA_DATA[length(AA_DATA)-i]<(AVG*1.015)) && (AA_DATA[length(AA_DATA)-i]>AA_DATA[E4]) && ((length(AA_DATA)-i)>E4)){
    E5 <- (length(AA_DATA)-i)
  }
}
(AA_DATA[E2]+AA_DATA[E4])/2 #The neckline was established at $13 - a difference of $17.45-$13=$4.75
#The price objective is $13-$4.75=$8.25
ts.plot(AA_DATA)
