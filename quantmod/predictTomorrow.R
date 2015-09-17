
install.packages("quantmod")
library(quantmod)

getSymbols("^TWII")
TWII_Close = Cl(TWII)
Label_TWII_Close = cbind(TWII_Close,lag(TWII_Close,-1))[-length(TWII_Close),]
colnames(Label_TWII_Close) = c("T","Tp1")
Label_TWII_Close = cbind(Label_TWII_Close,sign(Label_TWII_Close$Tp1 - Label_TWII_Close$T))
Label_TWII_Close = cbind(Label_TWII_Close,Label_TWII_Close$Tp1 - Label_TWII_Close$T)
colnames(Label_TWII_Close)[3]="Label"
colnames(Label_TWII_Close)[4]="Diff"

MA_Series = c(5,10,20,60)
MA_Series_Features = SMA(Label_TWII_Close$T,5)

for (i in MA_Series[-1]){
  MA_Series_Features = cbind(MA_Series_Features,SMA(Label_TWII_Close$T,i))
}

colnames(MA_Series_Features)=unlist(lapply(MA_Series,function(x) paste("MA",x,sep="")))

Total_Features = MA_Series_Features



KD_Features = stoch(Label_TWII_Close$T)
Total_Features = cbind(Total_Features,KD_Features)


Total_Features = cbind(Total_Features,MACD(Label_TWII_Close$T))


Total_Data = as.data.frame(cbind(Label_TWII_Close$Label,Total_Features)["2007-04-09::"])


NData = length(Total_Data[,1])

Training_Index = sample(1:NData,500)
Training_Data = Total_Data[Training_Index,]
Training_Data$Label = as.factor(Training_Data$Label)

# install.packages("e1071")
library(e1071)
SVM_Model = svm(Label~.,data=Training_Data,kernel="polynomial")

table(Total_Data[,1],predict(SVM_Model,newdata=Total_Data[,-1]))








