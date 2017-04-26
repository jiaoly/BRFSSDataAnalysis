

#Author:Sanjoy Basu


#  Exploring the BRFSS data


## Setup

### Load packages


library(ggplot2)
library(dplyr)
library(reshape2)
library(gridBase)
library(gridExtra)


### Load data



load("brfss2013.RData")






## Part 1: Data

The research is using 2013 data collected by BRFSS.
The Behavioral Risk Factor Surveillance System (BRFSS) with technical and methodological assistance from CDC, state health departments conduct health-related telephone surveys that collect state data about U.S. residents regarding their health-related risk behaviors. 
The survey is conducted using Random Digit Dialing (RDD) techniques on both landlines and cell phones.
So the data is from randomly sampled subjects from the population of United States. This data is generalizable of US population and can be used only for observational study.

* * *

## Part 2: Research questions

#**Research quesion 1:**
#Is there a relation between reported number of days of poor health condition (physical and mental) and lack of medical coverage?




#**Research quesion 2:**
#Is there a relation between good health and consumption of food such as fruits and  dark-green, orange vegetables ?




#Research quesion 3:**
#Is there a relation between  intensity of depression and number of hours of sleep?



## Part 3: Exploratory data analysis



#**Research quesion 1:**


phys_ment_statedistribution <- brfss2013 %>% filter(!is.na(cstate), !is.na(physhlth), !is.na(menthlth), cstate=="Yes") %>% group_by(X_state) %>% select (X_state, physhlth, menthlth) %>% summarise(phys=sum(physhlth), ment=sum(menthlth))
head(phys_ment_statedistribution, n=10)



state_cover <- brfss2013 %>% filter(!is.na(cstate), cstate=="Yes", !is.na(nocov121))  %>% group_by(X_state)  %>%  summarise(cnt=sum(nocov121=="Yes"))
head(state_cover, n=10)



phys_plot <- ggplot(data=phys_ment_statedistribution, aes(x=X_state, y=phys))+geom_bar(color="black", stat = "identity", fill="salmon") + scale_y_continuous(limits=c(0, 30000)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + xlab("US states and territories") + ylab("Days impacted in 2013") + geom_text(aes(label=phys, angle=90, hjust=0.019)) +  ggtitle("Days impacted due to physical health")
mntl_plot <- ggplot(data=phys_ment_statedistribution, aes(x=X_state, y=ment))+geom_bar(color="black", stat = "identity", fill="tomato2") + scale_y_continuous(limits=c(0, 30000)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + xlab("US states and territories") + ylab("Days impacted in 2013") + geom_text(aes(label=ment, angle=90, hjust=0.019)) +  ggtitle("Days impacted due to mental health")
cvrg_plot <- ggplot(data=state_cover, aes(x=X_state, y=cnt))+ geom_bar(stat="identity", fill="red") + scale_y_continuous(limits=c(0, 500)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + xlab("US states and territories") + ylab("Number without health coverage 2013") + geom_text(aes(label=cnt, angle=90, hjust=0.019)) +  ggtitle("Lack of health coverage")
#grid.arrange(phys_plot, mntl_plot, nrow=2)
phys_plot 
mntl_plot
cvrg_plot







#**Research quesion 2:**


genhlth_food <- brfss2013  %>% filter(!is.na(genhlth),  !is.na(fruitju1), !is.na(fruit1),!is.na(fvbeans),!is.na(fvgreen),!is.na(fvorang)) %>% select (genhlth, fruitju1, fruit1, fvbeans, fvgreen, fvorang)
genhlth_foodm<-melt(genhlth_food, id.vars = "genhlth")
genhlth_foodmpe <- genhlth_foodm %>%  filter(genhlth=="Excellent" | genhlth=="Poor")
gf_bar <- ggplot(data=genhlth_foodmpe, aes(variable, value, fill=genhlth)) + geom_bar(stat = "identity", position="dodge") + scale_fill_discrete(name="General Health") + xlab("FDA fruits and vegetables group")
gf_bar<-gf_bar + scale_x_discrete(labels=c("FruitJuice", "Fruits", "Beans/Lentils", "Green vegetable", "Orange colored vegetables")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
gf_bar




genhlth_foodmp <- genhlth_foodm %>% filter(genhlth=="Poor")
genhlth_foodme <- genhlth_foodm %>% filter(genhlth=="Excellent")
s1 <- ggplot(data=genhlth_foodme, aes (x=variable, y=value)) + geom_boxplot(fill="seagreen1") + xlab("FDA fruits and vegetables group") + scale_x_discrete(labels=c("FruitJuice", "Fruits", "Beans/Lentils", "Green vegetable", "Orange colored vegetables"))
s2 <- ggplot(data=genhlth_foodmp, aes (x=variable, y=value)) + geom_boxplot(fill="salmon1") + xlab("FDA fruits and vegetables group") + scale_x_discrete(labels=c("FruitJuice", "Fruits", "Beans/Lentils", "Green vegetable", "Orange colored vegetables"))
s1 <- s1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
s2 <- s2 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
grid.arrange(s1, s2, ncol=2)
`

#Research quesion 3:**


slp_mean <- brfss2013 %>% filter(!is.na(sleptim1), !is.na(misdeprd))  %>% group_by(misdeprd) %>% summarise(sleptim1_mean=mean(sleptim1)) %>% select (sleptim1_mean, misdeprd)
slp <- brfss2013 %>% filter(!is.na(sleptim1), !is.na(misdeprd)) %>% select (sleptim1, misdeprd)
d1 <- ggplot(data=slp_mean, aes(x=misdeprd, y=sleptim1_mean)) + geom_bar(stat="identity", color="black", fill="steelblue") + scale_y_continuous(limits = c(0, 10)) + geom_text(aes(label=round(sleptim1_mean, 2), , vjust=0.019)) + xlab("Intensity of Depression") + ylab("Average hours of sleep") 
d2 <- ggplot(data=slp, aes(x=misdeprd, y=sleptim1)) + geom_boxplot(fill="orange") + scale_y_continuous(limits = c(0, 24)) +xlab("Intensity of Depression") + ylab("Hours of sleep")
grid.arrange(d1, d2, ncol=2)
