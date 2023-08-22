#set wd
#setwd("C:/Users/f1extm00/Dropbox")
#setwd("C:/Users/f1ixi02/Dropbox (ATL-FRB)")
#setwd("C:/Users/ellie/Dropbox")

#load directories
#source(paste0(getwd(),"/WorkForceDevProj/directories.R"),local=TRUE)

#load libraries
#source(paste0(getwd(), dir.benefitsexpenses,"/programs/libraries.R"), local=TRUE) # Load required packages

# load expense data, benefit parameters, and eligible tables of SMI,FPL / crosswalks
load(paste0(getwd(), dir.benefitsexpenses,"/Database/tables.rdata"))
load(paste0(getwd(), dir.benefitsexpenses,"/Database/expenses.rdata"))
load(paste0(getwd(), dir.benefitsexpenses,"/Database/benefit.parameters.rdata"))
load(paste0(getwd(), dir.benefitsexpenses,"/Database/parameters.defaults.rdata"))

#---------------------------------------------------------------------------------------------------
#function for extracting copay from TFA & recalculating childcare expenses using UW & value of CCDF
#---------------------------------------------------------------------------------------------------
function.recalculateCCDFvalue<-function(data, stateNamevar, ageChild1var, ageChild2var,numkidsunder13var,value.CCDF_TFAvar, currentyear, childcareexpenseTFAvar){
  
#rename vars
  data<- data %>% 
    rename("stateName"=`stateNamevar`, "ageChild1"=`ageChild1var`, "ageChild2"=`ageChild2var`, "numkidsunder13"=`numkidsunder13var`, "value_CCDF_TFA" =`value.CCDF_TFAvar`,"childcareexpenseTFA"=`childcareexpenseTFAvar`)
  
# Merge on TFA Childcare expenses (Child Aware data) to recover the CCDF copay
childawaredata<-read.xlsx(paste0(getwd(),dir.benefitsexpenses,"/Data.childAware/Childcare_clean_2017.xlsx")
                          , sheet="Childcare", colNames = TRUE, startRow = 1) %>% 
                rename("stateName"="State") 

data<-data %>%   
  left_join(childawaredata, by="stateName") %>% 
  #note here the 5+year old data only includes the cost of childcare during the school year (to match TFA calcs)
  mutate(childcare.exp.childaware.child1=case_when(ageChild1<1 ~`<1.years.old`, 
                                                   ageChild1 %in% c(1:3) ~ `1-3.years.old`,
                                                   ageChild1==4 ~ `4.years.old`, 
                                                   ageChild1 %in% c(5:12) ~`5+.years.old`,
                                                   TRUE ~ 0)) %>% 
  mutate(childcare.exp.childaware.child2=case_when(ageChild2<1 ~`<1.years.old`,
                                                   ageChild2 %in% c(1:3) ~ `1-3.years.old`,
                                                   ageChild2==4 ~ `4.years.old`, 
                                                   ageChild2 %in% c(5:12) ~`5+.years.old`,
                                                   TRUE ~ 0)) %>% 
  #ET: looked at the column "ChildcareExpense" which i think is from TFA and it looks like this is the difference (at least when current yr = 2019)
  mutate(ChildcareExpenseGuess.tfa=round((childcare.exp.childaware.child1+childcare.exp.childaware.child2)*1.01))
  
# Recalculate the total CCDF copay
data$copay.tfa<-data$ChildcareExpenseGuess.tfa-data$value_CCDF_TFA

# Replace NAs if family is ineligible (copay=full amount) or has no childcare expenses
#data$copay.tfa[data$ChildcareExpenseGuess.tfa>=0]<-NA
data$copay.tfa[data$ChildcareExpenseGuess.tfa>=0 & data$value_CCDF_TFA==0]<-NA

#copay is the same for all children b/c we can't seperate them in TFA
data$copay.tfa.child1[data$ageChild1 %in% c(0:12)]<-data$copay.tfa[data$ageChild1 %in% c(0:12)]/data$numkidsunder13[data$ageChild1 %in% c(0:12)]
data$copay.tfa.child2[data$ageChild2 %in% c(0:12)]<-data$copay.tfa[data$ageChild2 %in% c(0:12)]/data$numkidsunder13[data$ageChild2 %in% c(0:12)]

#merge on new childcare expenses
#set to schoolagesummercare=parttime to match TFA
data$childcare.exp.child1<-function.childcareExp.UW(data=data, statefipsvar="stateFIPS", countyortownnamevar="countyortownName", ageofPersonvar="ageChild1", currentyr=currentyear, schoolagesummercare="parttime")
data$childcare.exp.child1<-ifelse(is.na(data$childcare.exp.child1),function.childcareExp.childaware(data=data
                                                                           , statefipsvar = "stateFIPS"
                                                                           , ageofPersonvar ="ageChild1"
                                                                           , currentyr=2019),data$childcare.exp.child1)

data$childcare.exp.child2<-function.childcareExp.UW(data=data, statefipsvar="stateFIPS", countyortownnamevar="countyortownName", ageofPersonvar="ageChild2", currentyr=currentyear, schoolagesummercare="parttime")
data$childcare.exp.child2<-ifelse(is.na(data$childcare.exp.child2),function.childcareExp.childaware(data=data
                                                                                                    , statefipsvar = "stateFIPS"
                                                                                                    , ageofPersonvar = "ageChild2"
                                                                                                    , currentyr=2019),data$childcare.exp.child2)



data$ChildcareExpense<-data$childcare.exp.child1 + data$childcare.exp.child2


# CCDF Child 1 & Child 2 separately
# Note: only if copay is 0 then CCDF is full value of childcare costs
data$value.CCDFchild1[!is.na(data$copay.tfa.child1)]<-data$childcare.exp.child1[!is.na(data$copay.tfa.child1)]-data$copay.tfa.child1[!is.na(data$copay.tfa.child1)]
data$value.CCDFchild1[is.na(data$copay.tfa.child1)]<-0

data$value.CCDFchild2[!is.na(data$copay.tfa.child2)]<-data$childcare.exp.child2[!is.na(data$copay.tfa.child2)]-data$copay.tfa.child2[!is.na(data$copay.tfa.child2)]
data$value.CCDFchild2[is.na(data$copay.tfa.child2)]<-0

CCDFValues<-data %>% 
  select(childcareexpenseTFA,ChildcareExpense,ChildcareExpenseGuess.tfa,childcare.exp.child1,childcare.exp.child2,value.CCDFchild1,value.CCDFchild2)
return(CCDFValues)
}
#View(data[data$FamilyType=="With_two_children", c("ChildcareExpense","ChildcareExpenseGuess.tfa", "Childcare_")])  
