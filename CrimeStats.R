install.packages("ggplot2")
library(ggplot2)
SF=read.csv("sanfrancisco_incidents_summer_2014.csv")
SF$Time=as.character(SF$Time)
SF$hour=as.numeric(substr(SF$Time,1,2))
SF$min=as.numeric(substr(SF$Time,4,5))
#SF$Date=as.character(SF$Date) 
nndays<<-92 # length(levels(SF$Date))
SF$Date=as.Date(SF$Date,"%m/%d/%Y")
qplot(SF$DayOfWeek)
SF$NewTime=SF$hour+SF$min/60
qplot(SF$NewTime)
library(scales)
q=ggplot(SF, aes(x=reorder(SF$Category, table(SF$Category)[SF$Category]))) + 
  geom_bar() #geom_bar(aes(nndays=nndays,y = (..count..)/nndays)) 
#  +scale_y_continuous(labels = percent_format())
q= q +coord_flip()
q + theme(text = element_text(size=16)) + ylab('Number of Incidents') + xlab("") 

q=ggplot(SF$Category)
q + theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=15))
SF <- transform(SF,Category = reorder(Category))
q2=qplot(SF$NewTime) 
q2

qplot(SF[SF$Category=="LARCENY/THEFT","NewTime"])
qplot(SF[SF$Category=="OTHER OFFENSES","NewTime"])
qplot(SF[SF$Category=="NON-CRIMINAL","NewTime"])
labl_num=seq(0,24,by=3)
labl_str=list()
for (i in 1:length(labl_num)){
  if (labl_num[i]<10) labl_str[i]=paste("0",as.character(labl_num[i]),":00",sep="")
  else labl_str[i]=paste(as.character(labl_num[i]),":00",sep="")
}
qplot(SF[SF$Category=="ASSAULT","NewTime"],binwidth=1) + scale_x_continuous(breaks=labl_num,labels=labl_str) 
length(labl_num)
labl_num
qplot(SF[SF$Category=="ROBBERY","NewTime"],binwidth=1) + scale_x_continuous(breaks=labl_num,labels=labl_str)

q1=qplot(SF[SF$Category=="LARCENY/THEFT","NewTime"],binwidth=1) + scale_x_continuous(breaks=labl_num,labels=labl_str) +
  xlab('Time of Day') + ylab("Number of Incidents") + ggtitle("LARCENY/THEFT")
q1=qplot(SF[SF$Category=="OTHER OFFENSES","NewTime"],binwidth=1) + scale_x_continuous(breaks=labl_num,labels=labl_str) +
  xlab('Time of Day') + ylab("Number of Incidents") + ggtitle("OTHER OFFENSES")
q1=qplot(SF[SF$Category=="NON-CRIMINAL","NewTime"],binwidth=1) + scale_x_continuous(breaks=labl_num,labels=labl_str) +
  xlab('Time of Day') + ylab("Number of Incidents") + ggtitle("NON-CRIMINAL")
q1=qplot(SF[SF$Category=="ASSAULT","NewTime"],binwidth=1) + scale_x_continuous(breaks=labl_num,labels=labl_str) +
  xlab('Time of Day') + ylab("Number of Incidents") + ggtitle("ASSAULT")
multiplot(q1, q2, q3, q4, cols=2)
qplot(SF[,"NewTime"],binwidth=1) + scale_x_continuous(breaks=labl_num,labels=labl_str) +
  xlab('Time of Day') + ylab("") + ggtitle('Number of Incidents in June - August of 2014')
