library(foreign)
library(plyr)
library(car)
wegmans=read.spss("Wegmans Survey 1.sav",to.data.frame = TRUE,
                  use.value.labels = TRUE,use.missings = TRUE,trim.factor.names = TRUE)


#--------- gender-------------------
sample.sex=table(wegmans$Question33Areyou)[2:3]
prop.table(sample.sex)
popul.sex=c(0.87,0.13)
chisq.test(sample.sex,p=popul.sex)

#-----------most important attribute---------------------
Importance = c("Question6Allnatural","Question6Blended","Question6Calorielevel","Question6Consistency","Question6Fatlevel","Question6Fruitonthebottom", "Question6Organic","Question6Price","Question6Proteinlevel","Question6rbSTfree","Question6Sidebysidecup", "Question6Taste","Question6Texture")
for (i in 1:13) {
  wegmans[[Importance[i]]]=revalue(wegmans[[Importance[i]]],c(Unsure=NA))
}
temp=sapply(wegmans[Importance],as.numeric)
sd=apply(temp,2,sd,na.rm=TRUE)
mean=colMeans(temp,na.rm = TRUE) 
ratings=data.frame(name=names(mean),mean,sd)
write.csv(ratings,file = "ratings.csv")

#---------brand perception-------------------------------
Importance2 = c("Question24Allnatural","Question24Price","Question24Taste")
Importance3 = c("Question30Allnatural","Question30Price","Question30Taste")
temp=data.frame(wegmans[Importance2],wegmans[Importance3])
for (i in 1:3) {#revalue unsure to NA
  temp[[Importance2[i]]]=revalue(wegmans[[Importance2[i]]],c(Unsure=NA))
  temp[[Importance3[i]]]=revalue(wegmans[[Importance3[i]]],c(Unsure=NA))
}
temp.num=as.data.frame(sapply(temp,as.numeric))
library(ggplot2)
for(i in 1:3){
  print(ggplot(temp.num,aes(temp.num[,Importance2[i]]))+geom_histogram())
  print(ggplot(temp.num,aes(temp.num[,Importance3[i]]))+geom_histogram())
}
#t test
for (i in 1:3) print(t.test(temp.num[,Importance2[i]],temp.num[,Importance3[i]]),paired=FALSE,
                            na.action=na.omit )

#Mann Whitney U test
?wilcox.test
for (i in 1:3) print(wilcox.test(data=temp,temp.num[,Importance2[i]],temp.num[,Importance3[i]]))

#-------different usage------------------------
wegmans$Question12DoyouuseGreekYogurtforcooking
#attributs to analyze
Importance4= names(wegmans)[c(47,53,56,54)]
#seperate by using yogurt to cook or not
rating.cook=wegmans[wegmans$Question12DoyouuseGreekYogurtforcooking=="Yes",Importance4]
rating.notcook=wegmans[wegmans$Question12DoyouuseGreekYogurtforcooking=="No ",Importance4]
for (i in 1:4) {#revalue unsure to NA
  rating.cook[[Importance4[i]]]=revalue(rating.cook[[Importance4[i]]],c(Unsure=NA))
  rating.notcook[[Importance4[i]]]=revalue(rating.notcook[[Importance4[i]]],c(Unsure=NA))
}
rating.cook=as.data.frame(sapply(rating.cook,as.numeric))
rating.notcook=as.data.frame(sapply(rating.notcook,as.numeric))
for(i in 1:4){#visualize the distributions of ratings
  print(ggplot(rating.cook,aes(rating.cook[,Importance4[i]]))+geom_histogram())
  print(ggplot(rating.notcook,aes(rating.notcook[,Importance4[i]]))+geom_histogram())
}
for (i in 1:4) {
  print(wilcox.test(rating.cook[,Importance4[i]],rating.notcook[,Importance4[i]]))  
}
rating.mean=data.frame(colMeans(rating.cook,na.rm = TRUE),colMeans(rating.notcook,na.rm = TRUE),
                       row.names=Importance4)
rating.mean
write.csv(rating.mean,file = "Q5.csv",row.names = TRUE)

#----正态假设检验--------
library(car)
qqPlot(lm(as.numeric(Question6Allnatural)~Question12DoyouuseGreekYogurtforcooking, data = wegmans), 
       simulate = TRUE, labels = FALSE)
