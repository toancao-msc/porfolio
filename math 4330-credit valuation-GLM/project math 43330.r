devtools::install_github('gmonette/spida2')
library(spida2)
install.packages("lattice")

data<-read.csv("D:\\math\\math 4330\\project\\loan_data_set13.csv")
 
 tablemissing(data)
 
 str(data)
 sum(is.na(data$Credit_History))

#To reduce the data loss, I replace the missing data of Loan amount to the mean value.

data$LoanAmount[is.na(data$LoanAmount)] <- mean(data$LoanAmount, na.rm = TRUE)
 
data1<-na.omit(data)   #Omit the empty categroical rows
 
dt<-subset(data1,select=-c(Loan_ID))# Deleting the ID column

xqplot(dt)

tablemissing(dt)
#Object: determining factor affect the Loan _ Status.

#Relationships between pairs of variables***********************************************
    #Gender vs Loan Status
    tab(dt,~Gender+Loan_Status)
    tab__(dt,~Gender+Loan_Status) %>%
    barchart(
    horizontal = FALSE,
    ylab = 'Number of Loan Status',
    auto.key = list(
      space = 'right',
      reverse.rows = T,
      title = 'Released'
         )
    )
   # percent comparision
    tab__(dt,~Gender+Loan_Status,pct=1) %>%
    barchart(
    horizontal = FALSE,
    ylab = 'Number of Loan Status',
    auto.key = list(
      space = 'right',
      reverse.rows = T,
      title = 'Approve'
    )
  )
    #fisher and chisq test
    tab__(dt,~Gender+Loan_Status) %>% fisher.test
    tab__(dt,~Gender+Loan_Status) %>% chisq.test
    
    #P value lager than 0.05
#Dependents vs Loan status 

    tab(dt,~Dependents+Loan_Status)
    tab__(dt,~Dependents+Loan_Status) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Loan Status'
        )
      )
    # percent comparision
    tab__(dt,~Dependents+Loan_Status,pct=1) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Approve'
        )
      )
    #fisher and chisq test
    tab__(dt,~Dependents+Loan_Status) %>% fisher.test
    tab__(dt,~Dependents+Loan_Status) %>% chisq.test
    
    #p-value is larger than 0.05
#*************************************************
#Education vs Loan Status
    tab(dt,~Education+Loan_Status)
    
    tab__(dt,~Education+Loan_Status) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Loan Status'
        )
      )
    
    # percent comparision
    tab__(dt,~Education+Loan_Status,pct=1) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Approve'
        )
      )
    #fisher and chisq test
    tab__(dt,~Education+Loan_Status) %>% fisher.test
    tab__(dt,~Education+Loan_Status) %>% chisq.test
    
    # significant , p value smaller than 0.05
    
#Self_employed vs Loan Status
    tab(dt,~Self_Employed+Loan_Status)
    
    tab__(dt,~Self_Employed+Loan_Status) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Loan Status'
        )
      )
    
    # percent comparision
    tab__(dt,~Self_Employed+Loan_Status,pct=1) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Approve'
        )
      )
    #fisher and chisq test
    tab__(dt,~Self_Employed+Loan_Status) %>% fisher.test
    tab__(dt,~Self_Employed+Loan_Status) %>% chisq.test
    
    #p-value is larger than 0.05

#Married vs Loan Status
    tab(dt,~Married+Loan_Status)
    
    tab__(dt,~Married+Loan_Status) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Loan Status'
        )
      )
    
    # percent comparision
    tab__(dt,~Married+Loan_Status,pct=1) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Approve'
        )
      )
    #fisher and chisq test
    tab__(dt,~Married+Loan_Status) %>% fisher.test
    tab__(dt,~Married+Loan_Status) %>% chisq.test
    #p-value is smaller than 0.05
    
    #Property_Area vs Loan Status
    tab(dt,~Property_Area+Loan_Status)
    
    tab__(dt,~Property_Area+Loan_Status) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Loan Status'
        )
      )
    
    # percent comparision
    tab__(dt,~Property_Area+Loan_Status,pct=1) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Approve'
        )
      )
    #fisher and chisq test
    tab__(dt,~Property_Area+Loan_Status) %>% fisher.test
    tab__(dt,~Property_Area+Loan_Status) %>% chisq.test
    #p-value is smaller than 0.05 
    
    #Credit_History vs Loan Status
    tab(dt,~Credit_History+Loan_Status)
    
    tab__(dt,~Credit_History+Loan_Status) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Loan Status'
        )
      )
    
    # percent comparision
    tab__(dt,~Credit_History+Loan_Status,pct=1) %>%
      barchart(
        horizontal = FALSE,
        ylab = 'Number of Loan Status',
        auto.key = list(
          space = 'right',
          reverse.rows = T,
          title = 'Approve'
        )
      )
    #fisher and chisq test
    tab__(dt,~Credit_History+Loan_Status) %>% fisher.test
    tab__(dt,~Credit_History+Loan_Status) %>% chisq.test

    #Quasi-Complete Separation
    
    
    #Loan status vs ApplicantIncome
    
    status<-as.character(dt$Loan_Status) #turn ch to factor
    facstatus<-factor(status)
    spineplot(Loan_Status~ApplicantIncome,dt1)
    
    spineplot(facstatus~CoapplicantIncome,dt)
  #******************************************************************
#determine importance factor by randomForest
    #encoding loan status
    dt$Loan_Status <- ifelse(dt$Loan_Status == "Y",1,0)
    
    
    library(randomForest)
    require(caTools)
    library(MASS)
fit<-glm(Loan_Status~.,family=binomial,data=dt1)
fit2<-glm(Loan_Status~':',family=binomial,data=dt1)
dat$Credit_score <- ifelse(dat$Credit_score == "Satisfactory",1,0)
step.model <- stepAIC(fit, direction = "backward",trace = FALSE)
fit<-glm(Loan_Status~Education,family=binomial,data=dt1)
fit1<-glm(Loan_Status~Credit_History,family=binomial,data=dt1)

stepAIC(fit)



ft1<-glm(Loan_Status~Married,family=binomial,data=dt1) #****
ft2<-glm(Loan_Status~Education,family=binomial,data=dt1)   #**
ft3<-glm(Loan_Status~LoanAmount,family=binomial,data=dt1)
ft4<-glm(Loan_Status~factor(Credit_History),family=binomial,data=dt1)
ft5<-glm(Loan_Status~factor(Property_Area),family=binomial,data=dt1) #****
anova(ft1)
anova(ft2)
anova(ft3)
anova(ft4)
anova(ft5)

#interaction between Credit_History vs Property_Area
fs1<-glm(Loan_Status~Property_Area,family=binomial,data=dt1)
summary(fs1)
anova(fs1)
fs2<-glm(Loan_Status~Property_Area+LoanAmount,family=binomial,data=dt1)
summary(fs2)
anova(fs2)
anova(fs1,fs2,test='LRT')
#there is no significantly differenct between two model.

#add the third variable
fs3<-glm(Loan_Status~Property_Area+LoanAmount+Education,family=binomial,data=dt1)
anova(fs2,fs3,test='LRT')
 #-> there is significantly difference between two model

# add education 
fs4<-glm(Loan_Status~Property_Area+Education,family=binomial,data=dt1)
anova(fs2,fs4,test='LRT')

#there is no significant differnect between two model.

fs5<-glm(Loan_Status~Property_Area+Education+Married+LoanAmount,family=binomial,data=dt1)
anova(fs4,fs5,test='LRT')
       #there is the significat difference between two model
fs6<-glm(Loan_Status~Credit_History+Property_Area*Married,family=binomial,data=dt1)
summary(fs6)
#modeling stepwise
fullmod<-glm(Loan_Status~.,family=binomial,data=dt1)
summary(fullmod)
nothing<-glm(Loan_Status~1,family = binomial,data=dt1)
summary(nothing)

remod<-glm(Loan_Status~Credit_History+Property_Area+Education,family=binomial,data=dt1)
summary(remod)

backwards<-step(fullmod)
formula(backwards)

summary(backward)
forwards <- step(nothing,scope=list(lower=formula(nothing),upper=formula(fullmod)), direction="forward")
bothway <- step(nothing,scope=list(lower=formula(nothing),upper=formula(fullmod)), direction="both")
formula(remod)
formula(backwards)
formula(forwards)
formula(bothway)

#an additive model.
nmod<-glm(Loan_Status~ Credit_History + Property_Area + Married + LoanAmount + 
            Education,family=binomial,data=dt1)
summary(nmod)
anova(forwards,backwards,test="LRT")

#new model
nmod1<-glm(Loan_Status~ Credit_History + Property_Area + Married + LoanAmount + 
            Education,family=binomial,data=dt1)
summary(nmod1)


#interaction test*****************************************************
#a test of parallelism i

#Exam the interaction between Credit_history and property area
install.packages("jtools")
install.packages("interactions")
library(interactions)
libarary(jtools)
inta<-glm(Loan_Status~ Credit_History+Married+LoanAmount+Education+Property_Area,family=binomial,data=dt1)
summary(inta)

interact_plot(inta, pred = LoanAmount, modx = Married)