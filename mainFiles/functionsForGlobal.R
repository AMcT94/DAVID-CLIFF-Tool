#This file contains all Dashboards functions

# Austin, could you delete functions that are not necessary ?

#NOTE: the dashboard does not have a createData function. Instead it has the same code (almost) twice time.series & cross.section


getPeriodValue <- function(x) { 
  
  x <- as.data.frame(x)
  
  # Generate additional zero rows if no basis for comparison
  if(nrow(x)==1){
    x<-rbind(0,x)
    x$CareerPath<-as.character(x$CareerPath)
    x$CareerPath[1]<-"Near-Minimum Wage Job"  # Artificial "Concessions" career path for comparison
  }
  
  
  result <- c()
  
  for(i in 2:nrow(x)){
    result[i] <- x[i,"NrPerDiscPeriod"] - x[i - 1, "NrPerDiscPeriod"]
  }
  
  returnValue <- data.frame(result, x$CareerPath, x$Period)
  
  return(returnValue)
} #end getPeriodValue


# Function to calculate the changes in net resources for each career step 
getPeriodValueTax <- function(x) { 
  
  x <- as.data.frame(x)
  
  # Generate additional zero rows if no basis for comparison
  if(nrow(x)==1){
    x<-rbind(0,x)
    x$CareerPath<-as.character(x$CareerPath)
    x$CareerPath[1]<-"Near-Minimum Wage Job"  # Artificial "Concessions" career path for comparison
  }
  
  
  result <- c()
  
  for(i in 2:nrow(x)){
    result[i] <- x[i,"NtPerDiscPeriod"] - x[i-1,"NtPerDiscPeriod"]
  }
  
  returnValue <- data.frame(result, x$CareerPath, x$Period)
  
  return(returnValue)
} #end getPeriodValueTax



###########################
# FUNCTIONS TO PRODUCE DATA
###########################

time.series <- function(benefit1, benefit2, city.name, career_path, occupation_1, occupation_2, numadults, numkids, careerpathIDs, aa1, aa2, aa3, aa4, aa5, aa6, ac1, ac2, ac3, ac4, ac5, ac6, spouse_income, third_adult_income, fourth_adult_income, fifth_adult_income, sixth_adult_income, income.otherfamily, schooling_1, schooling_2, edu_req_1, edu_req_2, manual, married, disability1, disability2, disability3, disability4, disability5, disability6, disability7, disability8, disability9, disability10,  disability11,  disability12, ssdiPIA1, ssdiPIA2, ssdiPIA3, ssdiPIA4, ssdiPIA5, ssdiPIA6,  blind1, blind2, blind3, blind4, blind5, blind6, disab.work.exp, prev_ssi,fam_disab){

  data<-expand_grid(
    locations = city.name # Make sure that format is County, ST
    , careerpathID = careerpathIDs # How many career paths do you want to analyze (careerpathID=1 is a baseline - Concessions)
    , income = 0 # initialize income to override later
    , Year = years
    , ruleYear = ruleYear
    , value.medicaid.adult = 0
    , value.medicaid.child = 0
    , ownorrent = 1
    , income.child_support =0
    , income.gift = 0
    , spouse_income = spouse_income * 12
    , third_adult_income = third_adult_income * 12
    , fourth_adult_income = fourth_adult_income * 12
    , fifth_adult_income = fifth_adult_income * 12
    , sixth_adult_income = sixth_adult_income * 12
    , income.otherfamily = income.otherfamily * 12
    , married = married
    , disability1=disability1
    ,disability2=disability2
    ,disability3=disability3
    ,disability4=disability4
    ,disability5=disability5
    ,disability6=disability6
    ,disability7=disability7
    ,disability8=disability8
    ,disability9=disability9
    ,disability10=disability10
    ,disability11=disability11
    ,disability12=disability12
    ,disab.work.exp=disab.work.exp
    ,prev_ssi=prev_ssi
    ,ssdiPIA1=ssdiPIA1
    ,ssdiPIA2=ssdiPIA2
    ,ssdiPIA3=ssdiPIA3
    ,ssdiPIA4=ssdiPIA4
    ,ssdiPIA5=ssdiPIA5
    ,ssdiPIA6=ssdiPIA6
    ,blind1=blind1
    ,blind2=blind2
    ,blind3=blind3
    ,blind4=blind4
    ,blind5=blind5
    ,blind6=blind6
  )
  
  ruleYear <- data$ruleYear
  data$Year<-as.numeric(data$Year)
  minYear <- min(data$Year)
  
  # Specify switches for Benefits profile
  if(benefit1=="All programs"){
    APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
    APPLY_HEADSTART<-TRUE
    APPLY_CCDF<-TRUE
    APPLY_PREK<-TRUE
    APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
    APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
    APPLY_MEDICAID_ADULT<-TRUE
    APPLY_MEDICAID_CHILD<-TRUE
    APPLY_ACA<-TRUE
    APPLY_SECTION8<-TRUE
    APPLY_SNAP<-TRUE
    APPLY_SLP<-TRUE 
    APPLY_WIC<-TRUE
    APPLY_EITC<-TRUE
    APPLY_CTC<-TRUE
    APPLY_CDCTC<-TRUE
    APPLY_TANF<-TRUE
    APPLY_SSDI<-TRUE
    APPLY_SSI<-TRUE
    APPLY_RAP<-FALSE
    APPLY_FATES<-FALSE
    
    
  }else if(benefit1=="No programs"){
    APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
    APPLY_HEADSTART<-FALSE
    APPLY_CCDF<-FALSE
    APPLY_PREK<-FALSE
    APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
    APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
    APPLY_MEDICAID_ADULT<-FALSE
    APPLY_MEDICAID_CHILD<-FALSE
    APPLY_ACA<-FALSE
    APPLY_SECTION8<-FALSE
    APPLY_SNAP<-FALSE
    APPLY_SLP<-FALSE 
    APPLY_WIC<-FALSE
    APPLY_EITC<-FALSE
    APPLY_CTC<-FALSE
    APPLY_CDCTC<-FALSE
    APPLY_TANF<-FALSE
    APPLY_SSDI<-FALSE
    APPLY_SSI<-FALSE
    APPLY_RAP<-FALSE
    APPLY_FATES<-FALSE
    
  }else if(benefit1=="All programs except Section 8 and CCDF"){
    APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
    APPLY_HEADSTART<-TRUE
    headstart_ftorpt <-"PT" #use the same for both headstart & earlyheadstart 
    APPLY_CCDF<-FALSE
    APPLY_PREK<-TRUE
    APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
    APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
    APPLY_MEDICAID_ADULT<-TRUE
    APPLY_MEDICAID_CHILD<-TRUE
    APPLY_ACA<-TRUE
    APPLY_SECTION8<-FALSE
    APPLY_SNAP<-TRUE
    APPLY_SLP<-TRUE 
    APPLY_WIC<-TRUE
    APPLY_EITC<-TRUE
    APPLY_CTC<-TRUE
    APPLY_CDCTC<-TRUE
    APPLY_TANF<-TRUE
    APPLY_SSDI<-FALSE
    APPLY_SSI<-FALSE
    APPLY_RAP<-FALSE
    APPLY_FATES<-FALSE
    
  }else if(benefit1=="Select a custom list"){
    APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
    APPLY_HEADSTART<-FALSE
    APPLY_CCDF<-FALSE
    APPLY_PREK<-FALSE
    APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
    APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
    APPLY_MEDICAID_ADULT<-FALSE
    APPLY_MEDICAID_CHILD<-FALSE
    APPLY_ACA<-FALSE
    APPLY_SECTION8<-FALSE
    APPLY_SNAP<-FALSE
    APPLY_SLP<-FALSE 
    APPLY_WIC<-FALSE
    APPLY_EITC<-FALSE
    APPLY_CTC<-FALSE
    APPLY_CDCTC<-FALSE
    APPLY_TANF<-FALSE
    APPLY_SSDI<-FALSE
    APPLY_SSI<-FALSE
    APPLY_RAP<-FALSE
    APPLY_FATES<-FALSE
    
    if("Supplemental Nutrition Assistance Program (SNAP)" %in% benefit2) {APPLY_SNAP<-TRUE}#else{APPLY_SNAP<-FALSE}
    if("Free or Reduced Price School Meals" %in% benefit2) {APPLY_SLP<-TRUE}
    if("Earned Income Tax Credit (EITC)" %in% benefit2) {APPLY_EITC<-TRUE}
    if("Child Tax Credit (CTC)" %in% benefit2) {APPLY_CTC<-TRUE}
    if("Child and Dependent Care Tax Credit (CDCTC)" %in% benefit2) {APPLY_CDCTC<-TRUE}
    if("Head Start/Early Head Start" %in% benefit2) {APPLY_HEADSTART<-TRUE}
    if("Section 8 Housing Choice Voucher" %in% benefit2) {APPLY_SECTION8<-TRUE}
    if("Child Care Subsidy (CCDF)" %in% benefit2) {APPLY_CCDF<-TRUE}
    if("State-Funded Pre-Kindergarten" %in% benefit2) {APPLY_PREK<-TRUE}
    if("Medicaid for Adults" %in% benefit2) {APPLY_MEDICAID_ADULT<-TRUE}
    if("Medicaid for Children/CHIP"  %in% benefit2) {APPLY_MEDICAID_CHILD<-TRUE}
    if("Health Insurance Marketplace Subsidy" %in% benefit2) {APPLY_ACA<-TRUE}
    if("Women, Infants and Children Nutrition Program (WIC)" %in% benefit2) {APPLY_WIC<-TRUE}
    if("Home Energy Assistance" %in% benefit2) {APPLY_LIHEAP<-TRUE}
    if("Temporary Assistance for Needy Families (TANF)" %in% benefit2) {APPLY_TANF<-TRUE}
    if ("RAP" %in% benefit2) {APPLY_RAP <- TRUE}
    if ("FATES" %in% benefit2) {APPLY_FATES <- TRUE}
    if ("Social Security Disability Insurance (SSDI)" %in% benefit2) {APPLY_SSDI <- TRUE}
    if ("Supplemental Security Income (SSI)" %in% benefit2) {APPLY_SSI <- TRUE}
    
    #change switches for continuous eligiblity if specified by user. Default is use cont elig rules.
    
    #insert switches
    
  }
  ##################################################################################

  data<-data %>% 
  #-----------------------------
  # 1. Demographics
  #-----------------------------
  # Initialize age of each member of the household
  
  mutate(agePerson1=NA_real_  
         ,agePerson2=NA_real_
         ,agePerson3=NA_real_ 
         ,agePerson4=NA_real_
         ,agePerson5=NA_real_
         ,agePerson6=NA_real_
         ,agePerson7=NA_real_
         ,agePerson8=NA_real_
         ,agePerson9=NA_real_
         ,agePerson10=NA_real_
         ,agePerson11=NA_real_
         ,agePerson12=NA_real_) %>% 
    
    # Tax Filing Status:
    # 1 - single
    # 2 - married filing jointly (make a note in Dashboard)
    # 3 - Heads of Household (for later)
    # 4 - Married Filing Separately (for later)

    mutate(FilingStatus=case_when(married==0 & numkids==0 ~ 1 # Single 
                                  ,married==0 & numkids>0 ~ 3 # Head of Household
                                  ,married==1 ~ 2 # Married Filing Jointly
                                  ,TRUE ~ 1)) %>%    

  #-----------------------------
  # 2. Finances
  #-----------------------------
  mutate(empl_healthcare=0 # initialize and override later
         ,income.investment=0
         ,ownorrent="rent"
         ,assets.cash=0
         ,assets.car1=0
         ,income_ind=income) %>% 
    
    
  #------------------------------------------------------------------------
  # 3. Family types settings (Manually set up start ages of family members)
  #-----------------------------------------------------------------------
  
  # Person 1 -6  - adults
  # Person 7-12 - children
  mutate(agePerson1=case_when(numadults>=1  ~ as.numeric(aa1)
                              ,TRUE ~ agePerson1),
         
         agePerson2=case_when(numadults>=2 ~ as.numeric(aa2)
                              ,TRUE ~ agePerson2),
         
         agePerson3=case_when(numadults>=3 ~ as.numeric(aa3)
                              ,TRUE ~ agePerson3),
         
         agePerson4=case_when(numadults>=4 ~ as.numeric(aa4)
                              ,TRUE ~ agePerson4),
         
         agePerson5=case_when(numadults>=5 ~ as.numeric(aa5)
                              ,TRUE ~ agePerson5),
         
         agePerson6=case_when(numadults>=6 ~ as.numeric(aa6)
                              ,TRUE ~ agePerson6),
         
         agePerson7=case_when(numkids>=1 ~ as.numeric(ac1)
                              ,TRUE ~ agePerson7),
         
         agePerson8=case_when(numkids>=2 ~ as.numeric(ac2)
                              ,TRUE ~ agePerson8),
         
         agePerson9=case_when(numkids>=3 ~ as.numeric(ac3)
                              ,TRUE ~ agePerson9),
         
         agePerson10=case_when(numkids>=4 ~ as.numeric(ac4)
                              ,TRUE ~ agePerson10),
         
         agePerson11=case_when(numkids>=5 ~ as.numeric(ac5)
                               ,TRUE ~ agePerson11),
         
         agePerson12=case_when(numkids>=6 ~ as.numeric(ac6)
                               ,TRUE ~ agePerson12),
  )
  
  
  
  data$negative <- 0
  
  if(!is.na(ac1) & ac1 < 0){
    ac1 <- NA
    data$negative <- 1
  } 
  
  if(!is.na(ac2) & ac2 < 0){
    ac2 <- NA
    data$negative <- 1
  }
  
  if(!is.na(ac3) & ac3 < 0){
    ac3 <- NA
    data$negative <- 1
  }
  
  if(!is.na(ac4) & ac4 < 0){
    ac4 <- NA
    data$negative <- 1
  }
    
  if(!is.na(ac5) & ac5 < 0){
      ac5 <- NA
      data$negative <- 1
  }
         
  if(!is.na(ac6) & ac6 < 0){
        ac6 <- NA
        data$negative <- 1
  }
  
  
  
  
  if(!is.na(aa1) & aa1 < 0){
    aa1 <- NA
    data$negative <- 1
  } 
  
  if(!is.na(aa2) & aa2 < 0){
    aa2 <- NA
    data$negative <- 1
  }
  
  if(!is.na(aa3) & aa3 < 0){
    aa3 <- NA
    data$negative <- 1
  }
  
  if(!is.na(aa4) & aa4 < 0){
    aa4 <- NA
    data$negative <- 1
  }
  
  if(!is.na(aa5) & aa5 < 0){
    aa5 <- NA
    data$negative <- 1
  }
  
  if(!is.na(aa6) & aa6 < 0){
    aa6 <- NA
    data$negative <- 1
  }
  

  data$missing <- 0
  
  if((numkids == 1) & (is.na(ac1) | ac1 > 18)){
    data$missing <- 1
    data$agePerson7 <- NA
    data$agePerson8 <- NA
    data$agePerson9 <- NA
    data$agePerson10 <- NA
    data$agePerson11 <- NA
    data$agePerson12 <- NA
     }
  
  if((numkids == 2) & (is.na(ac1) | is.na(ac2) | ac1 > 18 | ac2 > 18)){
    data$missing <- 1 
    data$agePerson7 <- NA
    data$agePerson8 <- NA
    data$agePerson9 <- NA
    data$agePerson10 <- NA
    data$agePerson11 <- NA
    data$agePerson12 <- NA
  }
  
  if((numkids == 3) & (is.na(ac1) | is.na(ac2) | is.na(ac3) | ac1 > 18 | ac2 > 18 | ac3 > 18)){
    data$missing <- 1
    data$agePerson7 <- NA
    data$agePerson8 <- NA
    data$agePerson9 <- NA
    data$agePerson10 <- NA
    data$agePerson11 <- NA
    data$agePerson12 <- NA
  }
  
  if((numkids == 4) & (is.na(ac1) | is.na(ac2) | is.na(ac3) |  is.na(ac4) | ac1 > 18 | ac2 > 18 | ac3 > 18 | ac4 > 18)){
    data$missing <- 1
    data$agePerson7 <- NA
    data$agePerson8 <- NA
    data$agePerson9 <- NA
    data$agePerson10 <- NA
    data$agePerson11 <- NA
    data$agePerson12 <- NA
  }
  
  if((numkids == 5) & (is.na(ac1) | is.na(ac2) | is.na(ac3) |  is.na(ac4) |  is.na(ac5)| ac1 > 18 | ac2 > 18 | ac3 > 18 | ac4 > 18 | ac5 > 18)){
    data$missing <- 1
    data$agePerson7 <- NA
    data$agePerson8 <- NA
    data$agePerson9 <- NA
    data$agePerson10 <- NA
    data$agePerson11 <- NA
    data$agePerson12 <- NA
  }
  
  if((numkids == 6) & (is.na(ac1) | is.na(ac2) | is.na(ac3) |  is.na(ac4) |  is.na(ac5)| is.na(ac6)| ac1 > 18 | ac2 > 18 | ac3 > 18 | ac4 > 18 | ac5 > 18 | ac6 > 18)){
    data$missing <- 1
    data$agePerson7 <- NA
    data$agePerson8 <- NA
    data$agePerson9 <- NA
    data$agePerson10 <- NA
    data$agePerson11 <- NA
    data$agePerson12 <- NA
  }
  
  
  ############################
  
  
  if((numadults == 1) & (is.na(aa1) | aa1 < 18)){
    data$missing <- 1
    data$agePerson1 <- NA
    data$agePerson2 <- NA
    data$agePerson3 <- NA
    data$agePerson4 <- NA
    data$agePerson5 <- NA
    data$agePerson6 <- NA
  }
  
  if((numadults == 2) & (is.na(aa1) | is.na(aa2) | aa1 < 18 | aa2 < 18)){
    data$missing <- 1
    data$agePerson1 <- NA
    data$agePerson2 <- NA
    data$agePerson3 <- NA
    data$agePerson4 <- NA
    data$agePerson5 <- NA
    data$agePerson6 <- NA
  }
  
  if((numadults == 3) & (is.na(aa1) | is.na(aa2) | is.na(aa3) | aa1 < 18 | aa2 < 18 | aa3 < 18)){
    data$missing <- 1
    data$agePerson1 <- NA
    data$agePerson2 <- NA
    data$agePerson3 <- NA
    data$agePerson4 <- NA
    data$agePerson5 <- NA
    data$agePerson6 <- NA
  }
  
  if((numadults == 4) & (is.na(aa1) | is.na(aa2) | is.na(aa3) |  is.na(aa4) | aa1 < 18 | aa2 < 18 | aa3 < 18 | aa4 < 18)){
    data$missing <- 1
    data$agePerson1 <- NA
    data$agePerson2 <- NA
    data$agePerson3 <- NA
    data$agePerson4 <- NA
    data$agePerson5 <- NA
    data$agePerson6 <- NA
  }
  
  if((numadults == 5) & (is.na(aa1) | is.na(aa2) | is.na(aa3) |  is.na(aa4) |  is.na(aa5)| aa1 < 18 | aa2 < 18 | aa3 < 18 | aa4 < 18 | aa5 < 18)){
    data$missing <- 1
    data$agePerson1 <- NA
    data$agePerson2 <- NA
    data$agePerson3 <- NA
    data$agePerson4 <- NA
    data$agePerson5 <- NA
    data$agePerson6 <- NA
  }
  
  if((numadults == 6) & (is.na(aa1) | is.na(aa2) | is.na(aa3) |  is.na(aa4) |  is.na(aa5)| is.na(aa6)| aa1 < 18 | aa2 < 18 | aa3 < 18 | aa4 < 18 | aa5 < 18 | aa6 < 18)){
    data$missing <- 1
    data$agePerson1 <- NA
    data$agePerson2 <- NA
    data$agePerson3 <- NA
    data$agePerson4 <- NA
    data$agePerson5 <- NA
    data$agePerson6 <- NA
  }
  
  
  if(fam_disab == "No" & (is.na(income.otherfamily) | income.otherfamily < 0)){
    data$missing <- 1
    data$income.otherfamily <- 0
    income.otherfamily <- 0
  }
  
  if(fam_disab == "Yes" & (is.na(spouse_income) | spouse_income < 0)){
    data$missing <- 1
    data$spouse_income  <- 0
  }
  
  if(fam_disab == "Yes" & (is.na(third_adult_income) | third_adult_income < 0)){
    data$missing <- 1
    data$third_adult_income <- 0
  }
  
  if(fam_disab == "Yes" & (is.na(fourth_adult_income) | fourth_adult_income < 0)){
    data$missing <- 1
    data$fourth_adult_income  <- 0
    }
  
  if(fam_disab == "Yes" & (is.na(fifth_adult_income) | fifth_adult_income < 0)){
    data$missing <- 1
    data$fifth_adult_income <- 0
   }
  
  if(fam_disab == "Yes" & (is.na(sixth_adult_income) | sixth_adult_income < 0)){
    data$missing <- 1
    data$sixth_adult_income  <- 0
  }
  
  if(1 %in% data$missing){
    data$agePerson1 <- 25
  }
  
  data$Year<-as.numeric(data$Year)
  
  # Adjust the age depending on year and starting age
  data<-data %>% 
    mutate(agePerson1 = agePerson1+(Year-minYear)
           ,agePerson2 = agePerson2+(Year-minYear)
           ,agePerson3 = agePerson3+(Year-minYear)
           ,agePerson4 = agePerson4+(Year-minYear)
           ,agePerson5 = agePerson5+(Year-minYear)
           ,agePerson6 = agePerson6+(Year-minYear)
           ,agePerson7 = agePerson7+(Year-minYear)
           ,agePerson8 = agePerson8+(Year-minYear)
           ,agePerson9 = agePerson9+(Year-minYear)
           ,agePerson10 = agePerson10+(Year-minYear)
           ,agePerson11= agePerson11+(Year-minYear)
           ,agePerson12 = agePerson12+(Year-minYear)
           ) %>% 
    
    # Simulate children leaving the house
    mutate(agePerson7=case_when(agePerson7>=child.leaves.house ~ NA_real_, TRUE ~ agePerson7)
           ,agePerson8=case_when(agePerson8>=child.leaves.house ~ NA_real_, TRUE ~ agePerson8)
           ,agePerson9=case_when(agePerson9>=child.leaves.house ~ NA_real_, TRUE ~ agePerson9)
           ,agePerson10=case_when(agePerson10>=child.leaves.house ~ NA_real_, TRUE ~ agePerson10)
           ,agePerson11=case_when(agePerson11>=child.leaves.house ~ NA_real_, TRUE ~ agePerson11)
           ,agePerson12=case_when(agePerson12>=child.leaves.house ~ NA_real_, TRUE ~ agePerson12)
           ) %>% 
    
    mutate(year.index = Year-minYear+1)
  
  
  data <- data[data$agePerson1 <= 64,]
  
  
  # Map state Abbrev to StateFIPS and county of choice to MSA for the wage growth projections
  data<-data %>%
    separate(locations, c("countyortownName","stateAbbrev"), sep=", ") %>%
    left_join(table.statemap, by="stateAbbrev") %>%
    left_join(table.msamap, by=c("stateAbbrev", "countyortownName"))
  
  data<-as.data.frame(data)
  
  d2 <<- data
  #-------------------------------------------
  # Step 2: Project wages
  #-------------------------------------------
  if(manual != "manual_no"){
    occupation.choice_2 <- occupation_2
  }else{
        occupation.choice_2 <- "empty"
        }
  
  #data$occ_title[data$careerpathID == 1] <- occupation.baseline
  occupation.baseline<-"Cashiers"
  occupation.choice <- occupation_1
  
  data$CareerPath <- NA
  if(manual == "manual_no"){
   data$CareerPath[data$careerpathID==1]<-"Near-Minimum Wage Job" 
   CareerPaths <- c(occupation.baseline, occupation.choice)
  }else{
    data$CareerPath[data$careerpathID==1]<-career_path[1]
    CareerPaths <- c(occupation.choice_2, occupation.choice)
  }
  data$CareerPath[data$careerpathID==2]<-career_path[2]
 
  if(schooling_1 < 0 & !is.na(schooling_1)) {schooling_1 <- NA}  
  
  if(schooling_2 < 0 & !is.na(schooling_2)) {schooling_2 <- NA}  
  
  data_full<-NULL
  data_full_2<-NULL
  
  if(manual == "manual_no" & edu_req_1 == "yes_typical_1"){
    # Need to make sure that occupation is full BLS name
    for(careerpathID in careerpathIDs){
      
      data.temp<-constructCareerPath.Dashboard(data[data$careerpathID==careerpathID,]
                                               , baselineOccupation = occupation.baseline # Baseline occupation (concession)
                                               , baselineOccupation.empl_healthcare = 1 # No healthcare for baseline occupation
                                               , educationDuration = NA # If set to NA then program automatically maps years of schooling for the choice occuation
                                               , hoursPT = 20 # how many hours to work while in school (Elias: need to automate Full-time/part-time schooling)
                                               , choiceOccupation = CareerPaths[careerpathID] 
                                               , choiceOccupation.empl_healthcare = 1) # Does it provide healthcare (typically yes) 
      
      data_full<-rbind(data.temp, data_full)
      
   }
  }else if(manual == "manual_no" & edu_req_1 == "yes_custom_1"){
    
    for(careerpathID in careerpathIDs){
      
      data.temp<-constructCareerPath.Dashboard(data[data$careerpathID==careerpathID,]
                                               , baselineOccupation = occupation.baseline # Baseline occupation (concession)
                                               , baselineOccupation.empl_healthcare = 1 # No healthcare for baseline occupation
                                               , educationDuration = schooling_1 # If set to NA then program automatically maps years of schooling for the choice occuation
                                               , hoursPT = 20 # how many hours to work while in school (Elias: need to automate Full-time/part-time schooling)
                                               , choiceOccupation = CareerPaths[careerpathID] 
                                               , choiceOccupation.empl_healthcare = 1) # Does it provide healthcare (typically yes)
         data_full<-rbind(data.temp, data_full)
   }
    
    
    
  }else if(manual == "manual_no" & edu_req_1 == "no_1"){
    
    for(careerpathID in careerpathIDs){
      
      data.temp<-constructCareerPath.Dashboard(data[data$careerpathID==careerpathID,]
                                               , baselineOccupation = occupation.baseline # Baseline occupation (concession)
                                               , baselineOccupation.empl_healthcare = 1 # No healthcare for baseline occupation
                                               , educationDuration = 0 # If set to NA then program automatically maps years of schooling for the choice occuation
                                               , hoursPT = 40 # how many hours to work while in school (Elias: need to automate Full-time/part-time schooling)
                                               , choiceOccupation = CareerPaths[careerpathID] 
                                               , choiceOccupation.empl_healthcare = 1) # Does it provide healthcare (typically yes)
      
      data_full<-rbind(data.temp, data_full)
    }
  }
  
  if(manual == "manual_yes"){
    if(edu_req_2 == "yes_typical_2"){
      # Need to make sure that occupation is full BLS name
       
      data.temp<-constructCareerPath.Dashboard(data[data$careerpathID==1,]
                                               , baselineOccupation = occupation.baseline # Baseline occupation (concession)
                                               , baselineOccupation.empl_healthcare = 1 # No healthcare for baseline occupation
                                               , educationDuration = NA # If set to NA then program automatically maps years of schooling for the choice occuation
                                               , hoursPT = 20 # how many hours to work while in school (Elias: need to automate Full-time/part-time schooling)
                                               , choiceOccupation = CareerPaths[1] 
                                               , choiceOccupation.empl_healthcare = 1) # Does it provide healthcare (typically yes) 
      
      data_full_2<-rbind(data.temp, data_full_2)
      
     }else if(edu_req_2 == "yes_custom_2"){
      
       
      data.temp<-constructCareerPath.Dashboard(data[data$careerpathID==1,]
                                               , baselineOccupation = occupation.baseline # Baseline occupation (concession)
                                               , baselineOccupation.empl_healthcare = 1 # No healthcare for baseline occupation
                                               , educationDuration = schooling_2 # If set to NA then program automatically maps years of schooling for the choice occuation
                                               , hoursPT = 20 # how many hours to work while in school (Elias: need to automate Full-time/part-time schooling)
                                               , choiceOccupation = CareerPaths[1] 
                                               , choiceOccupation.empl_healthcare = 1) # Does it provide healthcare (typically yes)
      data_full_2<-rbind(data.temp, data_full_2)
      
      
      
    }else if(edu_req_2 == "no_2"){
 
      data.temp<-constructCareerPath.Dashboard(data[data$careerpathID==1,]
                                               , baselineOccupation = occupation.baseline # Baseline occupation (concession)
                                               , baselineOccupation.empl_healthcare = 1 # No healthcare for baseline occupation
                                               , educationDuration = 0 # If set to NA then program automatically maps years of schooling for the choice occuation
                                               , hoursPT = 40 # how many hours to work while in school (Elias: need to automate Full-time/part-time schooling)
                                               , choiceOccupation = CareerPaths[1] 
                                               , choiceOccupation.empl_healthcare = 1) # Does it provide healthcare (typically yes)
      
      data_full_2<-rbind(data.temp, data_full_2)
    }
    
    if(edu_req_1 == "yes_typical_1"){
    # Need to make sure that occupation is full BLS name

    data.temp<-constructCareerPath.Dashboard(data[data$careerpathID==2,]
                                             , baselineOccupation = occupation.baseline # Baseline occupation (concession)
                                             , baselineOccupation.empl_healthcare = 1 # No healthcare for baseline occupation
                                             , educationDuration = NA # If set to NA then program automatically maps years of schooling for the choice occuation
                                             , hoursPT = 20 # how many hours to work while in school (Elias: need to automate Full-time/part-time schooling)
                                             , choiceOccupation = CareerPaths[2] 
                                             , choiceOccupation.empl_healthcare = 1) # Does it provide healthcare (typically yes) 
    
    data_full<-rbind(data.temp, data_full)
    
   }else if(edu_req_1 == "yes_custom_1"){
    
    
    data.temp<-constructCareerPath.Dashboard(data[data$careerpathID==2,]
                                             , baselineOccupation = occupation.baseline # Baseline occupation (concession)
                                             , baselineOccupation.empl_healthcare = 1 # No healthcare for baseline occupation
                                             , educationDuration = schooling_1 # If set to NA then program automatically maps years of schooling for the choice occuation
                                             , hoursPT = 20 # how many hours to work while in school (Elias: need to automate Full-time/part-time schooling)
                                             , choiceOccupation = CareerPaths[2] 
                                             , choiceOccupation.empl_healthcare = 1) # Does it provide healthcare (typically yes)
    data_full<-rbind(data.temp, data_full)
     
    
    
  }else if(edu_req_1 == "no_1"){
    
     
    data.temp<-constructCareerPath.Dashboard(data[data$careerpathID==2,]
                                             , baselineOccupation = occupation.baseline # Baseline occupation (concession)
                                             , baselineOccupation.empl_healthcare = 1 # No healthcare for baseline occupation
                                             , educationDuration = 0 # If set to NA then program automatically maps years of schooling for the choice occuation
                                             , hoursPT = 40 # how many hours to work while in school (Elias: need to automate Full-time/part-time schooling)
                                             , choiceOccupation = CareerPaths[2] 
                                             , choiceOccupation.empl_healthcare = 1) # Does it provide healthcare (typically yes)
    
    data_full<-rbind(data.temp, data_full)
  }
    } #end manual=yes
  
  year_diff <- max(data$Year) - min(data$Year) + 1
  
  data <- rbind(data_full, data_full_2)
  minwage <- seq(1,year_diff,1)
  data$experience[data$CareerPath == "Near-Minimum Wage Job"] <- seq(1,year_diff,1)
  data$hours[data$CareerPath == "Near-Minimum Wage Job"] <- 40
  
  rm(data.temp)
  rm(data_full)
  # Later rename occupations back
  
  # Apply wage growth projection
  data<-function.projectWages(data)  # Wage growth projection
  
  if(is.na(sum(data$intercept))){
    data <- data[data$careerpathID != 2,]
    }
  
  
  #create lag of income for tax credits calculations
  data<-as.data.table(data)
  data_1 <- data[data$careerpathID == 1,]
  data_2 <- data[data$careerpathID == 2,]
  data_1[, income_tm12:=c(income[1], income[-.N]), by=countyortownName]
  data_2[, income_tm12:=c(income[1], income[-.N]), by=countyortownName]
  data <- rbind(data_2, data_1)
  data<-as.data.frame(data)
  
  data$individual_income_BT <- data$income
  data$income_ind <- data$income
  
  data$income <- data$income + data$spouse_income + data$third_adult_income + data$fourth_adult_income + data$fifth_adult_income + data$sixth_adult_income + data$income.otherfamily
  
  data$income <- data$income 
  

# d3 <<- data
# zyx <<- 590
 # remove the following from the data and add them back in with InitialTransformations
  data<-data %>% select(-c("MSA","stateName","stateFIPS"))
  
 # Need InitialTransformations to get incomes for disability programs
 

  data <- function.InitialTransformations(data)

  #override incomes for each family member if there is disability in the home
    data$income1 <- data$individual_income_BT
    data$income2[fam_disab == "Yes" & !is.na(data$agePerson2) & data$agePerson2>=19] <- data$spouse_income[fam_disab == "Yes" & !is.na(data$agePerson2) & data$agePerson2>=19]
    data$income3[fam_disab == "Yes" & !is.na(data$agePerson3) & data$agePerson3>=19] <- data$third_adult_income[fam_disab == "Yes" & !is.na(data$agePerson3) & data$agePerson3>=19]
    data$income4[fam_disab == "Yes" & !is.na(data$agePerson4) & data$agePerson4>=19] <- data$fourth_adult_income[fam_disab == "Yes" & !is.na(data$agePerson4) & data$agePerson4>=19]
    data$income5[fam_disab == "Yes" & !is.na(data$agePerson5) & data$agePerson5>=19] <- data$fifth_adult_income[fam_disab == "Yes" & !is.na(data$agePerson5) & data$agePerson5>=19]
    data$income6[fam_disab == "Yes" & !is.na(data$agePerson6) & data$agePerson6>=19] <- data$sixth_adult_income[fam_disab == "Yes" & !is.na(data$agePerson6) & data$agePerson6>=19]

  zyx <<- 599
  
  #-------------------------------------------
  # Step 3: Apply Benefits Calculator
  #-------------------------------------------
  
  data_pre <<- data

  # Apply Expenses
  data<-BenefitsCalculator.ALICEExpenses(data)
  data<-BenefitsCalculator.OtherBenefits(data, APPLY_TANF, APPLY_SSDI, APPLY_SSI) 
  #note!! initital vs cont elig rules will only work for datset with ONE family in it.... b/c min function is not family specific!!
  data<-BenefitsCalculator.Childcare(data, APPLY_CHILDCARE, APPLY_HEADSTART, APPLY_PREK, APPLY_CCDF,APPLY_FATES, contelig.ccdf = `contelig.ccdf`,contelig.headstart = `contelig.headstart`, contelig.earlyheadstart = `contelig.earlyheadstart`) #option to add APPLY_FATES
  data<-BenefitsCalculator.Healthcare(data, APPLY_HEALTHCARE, APPLY_MEDICAID_ADULT, APPLY_MEDICAID_CHILD, APPLY_ACA)
  data<-BenefitsCalculator.FoodandHousing(data, APPLY_SECTION8, APPLY_LIHEAP, APPLY_SNAP, APPLY_SLP, APPLY_WIC,APPLY_RAP) # OPTION TO END WITH APPLY_RAP
  data<-BenefitsCalculator.TaxesandTaxCredits(data, APPLY_EITC, APPLY_CTC, APPLY_CDCTC)

  data$ATI <- data$AfterTaxIncome
  
  data_end <<- data
  
 # data_post <<- data
  zyx <<- 606
  ######################################################
  # Generate Additional Variables
  ######################################################
  data<-function.createVars(data)
  
  zyx <<- 607
  data$ProgramLoss<-0
  
 # data_time_series <<- data
  
    #data <- program.loss(data, ac1, ac2, ac3, family, careerpathIDs)
  #data$ProgramLoss[data$agePerson1 == 25] <- 0
  
  return(data)
  
} #end of time.series function



cross.section <- function(benefit1, benefit2, city.name, career_path, occupation_1, numadults, numkids, careerpathIDs, aa1, aa2, aa3, aa4, aa5, aa6, ac1, ac2, ac3, ac4, ac5, ac6, married, disability1, disability2, disability3, disability4, disability5, disability6, disability7, disability8, disability9, disability10, disability11, disability12, ssdiPIA1, ssdiPIA2, ssdiPIA3, ssdiPIA4, ssdiPIA5, ssdiPIA6, blind1, blind2, blind3, blind4, blind5, blind6, disab.work.exp, prev_ssi){
  
  
  csdata<-expand_grid(
     locations = city.name
    , careerpathID = careerpathIDs # How many career paths do you want to analyze (careerpathID=1 is a baseline - Concessions)
    , income = income
    , Year = ruleYear
    , ruleYear = ruleYear
    , value.medicaid.adult = 0 #ET- 9/1/21 .. cam this be deleted since its caluclated later?
    , value.medicaid.child = 0
    , income.child_support =0
    , income.gift =0
    , income.otherfamily = income.otherfamily
    , married = married
    , disability1=disability1
    ,disability2=disability2
    ,disability3=disability3
    ,disability4=disability4
    ,disability5=disability5
    ,disability6=disability6
    ,disability7=disability7
    ,disability8=disability8
    ,disability9=disability9
    ,disability10=disability10
    ,disability11=disability11
    ,disability12=disability12
    ,disab.work.exp=disab.work.exp
    ,prev_ssi=prev_ssi
    ,ssdiPIA1=ssdiPIA1
    ,ssdiPIA2=ssdiPIA2
    ,ssdiPIA3=ssdiPIA3
    ,ssdiPIA4=ssdiPIA4
    ,ssdiPIA5=ssdiPIA5
    ,ssdiPIA6=ssdiPIA6
    ,blind1=blind1
    ,blind2=blind2
    ,blind3=blind3
    ,blind4=blind4
    ,blind5=blind5
    ,blind6=blind6
  ) 

  ruleYear <- csdata$ruleYear
  
  csdata$Year<-as.numeric(csdata$Year)
  
  
  # Specify switches for Benefits profile
  if(benefit1=="All programs"){
    APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
    APPLY_HEADSTART<-TRUE
    APPLY_CCDF<-TRUE
    APPLY_PREK<-TRUE
    APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
    APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
    APPLY_MEDICAID_ADULT<-TRUE
    APPLY_MEDICAID_CHILD<-TRUE
    APPLY_ACA<-TRUE
    APPLY_SECTION8<-TRUE
    APPLY_SNAP<-TRUE
    APPLY_SLP<-TRUE 
    APPLY_WIC<-TRUE
    APPLY_EITC<-TRUE
    APPLY_CTC<-TRUE
    APPLY_CDCTC<-TRUE
    APPLY_TANF<-TRUE
    APPLY_SSDI<-TRUE
    APPLY_SSI<-TRUE
    APPLY_RAP<-FALSE
    APPLY_FATES<-FALSE
    
    
  }else if(benefit1=="No programs"){
    APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
    APPLY_HEADSTART<-FALSE
    APPLY_CCDF<-FALSE
    APPLY_PREK<-FALSE
    APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
    APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
    APPLY_MEDICAID_ADULT<-FALSE
    APPLY_MEDICAID_CHILD<-FALSE
    APPLY_ACA<-FALSE
    APPLY_SECTION8<-FALSE
    APPLY_SNAP<-FALSE
    APPLY_SLP<-FALSE 
    APPLY_WIC<-FALSE
    APPLY_EITC<-FALSE
    APPLY_CTC<-FALSE
    APPLY_CDCTC<-FALSE
    APPLY_TANF<-FALSE
    APPLY_SSDI<-FALSE
    APPLY_SSI<-FALSE
    APPLY_RAP<-FALSE
    APPLY_FATES<-FALSE
    
  }else if(benefit1=="All programs except Section 8 and CCDF"){
    APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
    APPLY_HEADSTART<-TRUE
    contelig.headstart <- TRUE
    contelig.earlyheadstart <- TRUE
    APPLY_CCDF<-FALSE
    contelig.ccdf <- TRUE
    APPLY_PREK<-TRUE
    APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
    APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
    APPLY_MEDICAID_ADULT<-TRUE
    APPLY_MEDICAID_CHILD<-TRUE
    APPLY_ACA<-TRUE
    APPLY_SECTION8<-FALSE
    APPLY_SNAP<-TRUE
    APPLY_SLP<-TRUE 
    APPLY_WIC<-TRUE
    APPLY_EITC<-TRUE
    APPLY_CTC<-TRUE
    APPLY_CDCTC<-TRUE
    APPLY_TANF<-TRUE
    APPLY_SSDI<-TRUE
    APPLY_SSI<-TRUE
    APPLY_RAP<-FALSE
    APPLY_FATES<-FALSE
    
  }else if(benefit1=="Select a custom list"){
    APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
    APPLY_HEADSTART<-FALSE
    contelig.headstart <- TRUE
    contelig.earlyheadstart <- TRUE
    APPLY_CCDF<-FALSE
    contelig.ccdf <- TRUE
    APPLY_PREK<-FALSE
    APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
    APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
    APPLY_MEDICAID_ADULT<-FALSE
    APPLY_MEDICAID_CHILD<-FALSE
    APPLY_ACA<-FALSE
    APPLY_SECTION8<-FALSE
    APPLY_SNAP<-FALSE
    APPLY_SLP<-FALSE 
    APPLY_WIC<-FALSE
    APPLY_EITC<-FALSE
    APPLY_CTC<-FALSE
    APPLY_CDCTC<-FALSE
    APPLY_TANF<-FALSE
    APPLY_SSDI<-FALSE
    APPLY_SSI<-FALSE
    APPLY_RAP<-FALSE
    APPLY_FATES<-FALSE
    
    if("Supplemental Nutrition Assistance Program (SNAP)" %in% benefit2) {APPLY_SNAP<-TRUE}else{APPLY_SNAP<-FALSE}
    if("Free or Reduced Price School Meals" %in% benefit2) {APPLY_SLP<-TRUE}
    if("Earned Income Tax Credit (EITC)" %in% benefit2) {APPLY_EITC<-TRUE}
    if("Child Tax Credit (CTC)" %in% benefit2) {APPLY_CTC<-TRUE}
    if("Child and Dependent Care Tax Credit (CDCTC)" %in% benefit2) {APPLY_CDCTC<-TRUE}
    if("Head Start/Early Head Start" %in% benefit2) {APPLY_HEADSTART<-TRUE}
    if("Section 8 Housing Choice Voucher" %in% benefit2) {APPLY_SECTION8<-TRUE}
    if("Child Care Subsidy (CCDF)" %in% benefit2) {APPLY_CCDF<-TRUE}
    if("State-Funded Pre-Kindergarten" %in% benefit2) {APPLY_PREK<-TRUE}
    if("Medicaid for Adults" %in% benefit2) {APPLY_MEDICAID_ADULT<-TRUE}
    if("Medicaid for Children/CHIP"  %in% benefit2) {APPLY_MEDICAID_CHILD<-TRUE}
    if("Health Insurance Marketplace Subsidy" %in% benefit2) {APPLY_ACA<-TRUE}
    if("Women, Infants and Children Nutrition Program (WIC)" %in% benefit2) {APPLY_WIC<-TRUE}
    if("Home Energy Assistance" %in% benefit2) {APPLY_LIHEAP<-TRUE}
    if("Temporary Assistance for Needy Families (TANF)" %in% benefit2) {APPLY_TANF<-TRUE}
    if ("RAP" %in% benefit2) {APPLY_RAP <- TRUE}
    if ("FATES" %in% benefit2) {APPLY_FATES <- TRUE}
    if ("Social Security Disability Insurance (SSDI)" %in% benefit2) {APPLY_SSDI <- TRUE}
    if ("Supplemental Security Income (SSI)" %in% benefit2) {APPLY_SSI <- TRUE}
    
    
    #change switches for continuous eligiblity if specified by user. Default is use cont elig rules.
    
    #insert switches
    
  }
 
    #-----------------------------
  # 1. Demographics
  #-----------------------------
  # Initialize age of each member of the household
  
  
  
 csdata<-csdata %>% 
    #-----------------------------
  # 1. Demographics
  #-----------------------------
  # Initialize age of each member of the household
  
  mutate(agePerson1=NA_real_  
         ,agePerson2=NA_real_
         ,agePerson3=NA_real_ 
         ,agePerson4=NA_real_
         ,agePerson5=NA_real_
         ,agePerson6=NA_real_
         ,agePerson7=NA_real_
         ,agePerson8=NA_real_
         ,agePerson9=NA_real_
         ,agePerson10=NA_real_
         ,agePerson11=NA_real_
         ,agePerson12=NA_real_
         ) %>% 
    
    # Tax Filing Status:
    # 1 - single
    # 2 - married filing jointly (make a note in Dashboard)
    # 3 - Heads of Household (for later)
    # 4 - Married Filing Separately (for later)
    
    mutate(FilingStatus=case_when(married==0 & numkids==0 ~ 1 # Single 
                                  ,married==0 & numkids>0 ~ 3 # Head of Household
                                  ,married==1 ~ 2 # Married Filing Jointly
                                  ,TRUE ~ 1)) %>%     
    
    #-----------------------------
  # 2. Finances
  #-----------------------------
  mutate(empl_healthcare=0 # initialize and override later
         ,income.investment=0
         ,ownorrent="rent"
         ,assets.cash=0
         ,assets.car1=0
         ,income_ind=income) %>% 
    
    
    #------------------------------------------------------------------------
  # 3. Family types settings (Manually set up start ages of family members)
  #-----------------------------------------------------------------------
  
  # Person 1 -6  - adults
  # Person 7-12 - children
  mutate(agePerson1=case_when(numadults>=1  ~ as.numeric(aa1)
                              ,TRUE ~ agePerson1),
         
         agePerson2=case_when(numadults>=2 ~ as.numeric(aa2)
                              ,TRUE ~ agePerson2),
         
         agePerson3=case_when(numadults>=3 ~ as.numeric(aa3)
                              ,TRUE ~ agePerson3),
         
         agePerson4=case_when(numadults>=4 ~ as.numeric(aa4)
                              ,TRUE ~ agePerson4),
         
         agePerson5=case_when(numadults>=5 ~ as.numeric(aa5)
                              ,TRUE ~ agePerson5),
         
         agePerson6=case_when(numadults>=6 ~ as.numeric(aa6)
                              ,TRUE ~ agePerson6),
         
         agePerson7=case_when(numkids>=1 ~ as.numeric(ac1)
                              ,TRUE ~ agePerson7),
         
         agePerson8=case_when(numkids>=2 ~ as.numeric(ac2)
                              ,TRUE ~ agePerson8),
         
         agePerson9=case_when(numkids>=3 ~ as.numeric(ac3)
                              ,TRUE ~ agePerson9),
         
         agePerson10=case_when(numkids>=4 ~ as.numeric(ac4)
                               ,TRUE ~ agePerson10),
         
         agePerson11=case_when(numkids>=5 ~ as.numeric(ac5)
                               ,TRUE ~ agePerson11),
         
         agePerson12=case_when(numkids>=6 ~ as.numeric(ac6)
                               ,TRUE ~ agePerson12),
  )
 
  
  
  csdata$negative <- 0
  
  if(!is.na(ac1) & ac1 < 0){
    ac1 <- NA
    csdata$negative <- 1
  } 
  
  if(!is.na(ac2) & ac2 < 0){
    ac2 <- NA
    csdata$negative <- 1
  }
  
  if(!is.na(ac3) & ac3 < 0){
    ac3 <- NA
    csdata$negative <- 1
  }
  
  if(!is.na(ac4) & ac4 < 0){
    ac4 <- NA
    csdata$negative <- 1
  }
  
  if(!is.na(ac5) & ac5 < 0){
    ac5 <- NA
    csdata$negative <- 1
  }
  
  if(!is.na(ac6) & ac6 < 0){
    ac6 <- NA
    csdata$negative <- 1
  }
  
  
  
  
  if(!is.na(aa1) & aa1 < 0){
    aa1 <- NA
    csdata$negative <- 1
  } 
  
  if(!is.na(aa2) & aa2 < 0){
    aa2 <- NA
    csdata$negative <- 1
  }
  
  if(!is.na(aa3) & aa3 < 0){
    aa3 <- NA
    csdata$negative <- 1
  }
  
  if(!is.na(aa4) & aa4 < 0){
    aa4 <- NA
    csdata$negative <- 1
  }
  
  if(!is.na(aa5) & aa5 < 0){
    aa5 <- NA
    csdata$negative <- 1
  }
  
  if(!is.na(aa6) & aa6 < 0){
    aa6 <- NA
    csdata$negative <- 1
  }
  
  
  
  csdata$missing <- 0
  
  if((numkids == 1) & (is.na(ac1) | ac1 > 18)){
    csdata$missing <- 1
    csdata$agePerson7 <- NA
    csdata$agePerson8 <- NA
    csdata$agePerson9 <- NA
    csdata$agePerson10 <- NA
    csdata$agePerson11 <- NA
    csdata$agePerson12 <- NA
  }
  
  if((numkids == 2) & (is.na(ac1) | is.na(ac2) | ac1 > 18 | ac2 > 18)){
    csdata$missing <- 1 
    csdata$agePerson7 <- NA
    csdata$agePerson8 <- NA
    csdata$agePerson9 <- NA
    csdata$agePerson10 <- NA
    csdata$agePerson11 <- NA
    csdata$agePerson12 <- NA
  }
  
  if((numkids == 3) & (is.na(ac1) | is.na(ac2) | is.na(ac3) | ac1 > 18 | ac2 > 18 | ac3 > 18)){
    csdata$missing <- 1
    csdata$agePerson7 <- NA
    csdata$agePerson8 <- NA
    csdata$agePerson9 <- NA
    csdata$agePerson10 <- NA
    csdata$agePerson11 <- NA
    csdata$agePerson12 <- NA
  }
  
  if((numkids == 4) & (is.na(ac1) | is.na(ac2) | is.na(ac3) |  is.na(ac4) | ac1 > 18 | ac2 > 18 | ac3 > 18 | ac4 > 18)){
    csdata$missing <- 1
    csdata$agePerson7 <- NA
    csdata$agePerson8 <- NA
    csdata$agePerson9 <- NA
    csdata$agePerson10 <- NA
    csdata$agePerson11 <- NA
    csdata$agePerson12 <- NA
  }
  
  if((numkids == 5) & (is.na(ac1) | is.na(ac2) | is.na(ac3) |  is.na(ac4) |  is.na(ac5)| ac1 > 18 | ac2 > 18 | ac3 > 18 | ac4 > 18 | ac5 > 18)){
    csdata$missing <- 1
    csdata$agePerson7 <- NA
    csdata$agePerson8 <- NA
    csdata$agePerson9 <- NA
    csdata$agePerson10 <- NA
    csdata$agePerson11 <- NA
    csdata$agePerson12 <- NA
  }
  
  if((numkids == 6) & (is.na(ac1) | is.na(ac2) | is.na(ac3) |  is.na(ac4) |  is.na(ac5)| is.na(ac6)| ac1 > 18 | ac2 > 18 | ac3 > 18 | ac4 > 18 | ac5 > 18 | ac6 > 18)){
    csdata$missing <- 1
    csdata$agePerson7 <- NA
    csdata$agePerson8 <- NA
    csdata$agePerson9 <- NA
    csdata$agePerson10 <- NA
    csdata$agePerson11 <- NA
    csdata$agePerson12 <- NA
  }
  
  
  ############################
  
  
  if((numadults == 1) & (is.na(aa1) | aa1 < 18)){
    csdata$missing <- 1
    csdata$agePerson1 <- NA
    csdata$agePerson2 <- NA
    csdata$agePerson3 <- NA
    csdata$agePerson4 <- NA
    csdata$agePerson5 <- NA
    csdata$agePerson6 <- NA
  }
  
  if((numadults == 2) & (is.na(aa1) | is.na(aa2) | aa1 < 18 | aa2 < 18)){
    csdata$missing <- 1
    csdata$agePerson1 <- NA
    csdata$agePerson2 <- NA
    csdata$agePerson3 <- NA
    csdata$agePerson4 <- NA
    csdata$agePerson5 <- NA
    csdata$agePerson6 <- NA
  }
  
  if((numadults == 3) & (is.na(aa1) | is.na(aa2) | is.na(aa3) | aa1 < 18 | aa2 < 18 | aa3 < 18)){
    csdata$missing <- 1
    csdata$agePerson1 <- NA
    csdata$agePerson2 <- NA
    csdata$agePerson3 <- NA
    csdata$agePerson4 <- NA
    csdata$agePerson5 <- NA
    csdata$agePerson6 <- NA
  }
  
  if((numadults == 4) & (is.na(aa1) | is.na(aa2) | is.na(aa3) |  is.na(aa4) | aa1 < 18 | aa2 < 18 | aa3 < 18 | aa4 < 18)){
    csdata$missing <- 1
    csdata$agePerson1 <- NA
    csdata$agePerson2 <- NA
    csdata$agePerson3 <- NA
    csdata$agePerson4 <- NA
    csdata$agePerson5 <- NA
    csdata$agePerson6 <- NA
  }
  
  if((numadults == 5) & (is.na(aa1) | is.na(aa2) | is.na(aa3) |  is.na(aa4) |  is.na(aa5)| aa1 < 18 | aa2 < 18 | aa3 < 18 | aa4 < 18 | aa5 < 18)){
    csdata$missing <- 1
    csdata$agePerson1 <- NA
    csdata$agePerson2 <- NA
    csdata$agePerson3 <- NA
    csdata$agePerson4 <- NA
    csdata$agePerson5 <- NA
    csdata$agePerson6 <- NA
  }
  
  if((numadults == 6) & (is.na(aa1) | is.na(aa2) | is.na(aa3) |  is.na(aa4) |  is.na(aa5)| is.na(aa6)| aa1 < 18 | aa2 < 18 | aa3 < 18 | aa4 < 18 | aa5 < 18 | aa6 < 18)){
    csdata$missing <- 1
    csdata$agePerson1 <- NA
    csdata$agePerson2 <- NA
    csdata$agePerson3 <- NA
    csdata$agePerson4 <- NA
    csdata$agePerson5 <- NA
    csdata$agePerson6 <- NA
  }
  
  if(is.na(income.otherfamily)){
    csdata$missing <- 1
    csdata$income.otherfamily <- 0
    income.otherfamily <- 0
  }
  
  if(1 %in% csdata$missing){
    data$agePerson1 <- 25
  }
  
  csdata$Year<-as.numeric(csdata$Year)
  
  # Map state Abbrev to StateFIPS and county of choice to MSA for the wage growth projections
  # this is done initial transformations
  csdata<-csdata %>%
    separate(locations, c("countyortownName","stateAbbrev"), sep=", ") #%>%

  csdata<-as.data.frame(csdata)
  csdata<-function.InitialTransformations(csdata)
  

  occupation.baseline<-"Cashiers"
  
  occupation.choice <- occupation_1
  
  CareerPaths <- c(occupation.baseline, occupation.choice)
  
  csdata$CareerPath <- NA
  for(careerpathID in careerpathIDs){
    csdata$CareerPath[csdata$careerpathID==careerpathID]<-career_path[careerpathID]
  }
  csdata$CareerPath <- as.factor(csdata$CareerPath)
  
  csdata$empl_healthcare <- 0
  csdata$year.index <- 1
  #csdata$BenefitsProfile <- benefits

  # Apply Expenses
  csdata<-BenefitsCalculator.ALICEExpenses(csdata)
  
  csdata$income_tm12<-csdata$income # create lag of income here, because I have no fucking idea where else income is created
  
  # if(csdata$income.otherfamily == 0){
  #   csdata$income1 <- income
  #   csdata$income2 <- 0
  # }else{
  # csdata$income1 <- csdata$income/2
  # csdata$income2 <- csdata$income/2
  # }
  
 
  csdata<-BenefitsCalculator.OtherBenefits(csdata, APPLY_TANF, APPLY_SSDI, APPLY_SSI) 
  #note!! initital vs cont elig rules will only work for datset with ONE family in it.... b/c min function is not family specific!!
  csdata<-BenefitsCalculator.Childcare(csdata, APPLY_CHILDCARE, APPLY_HEADSTART, APPLY_PREK, APPLY_CCDF,APPLY_FATES, contelig.ccdf = `contelig.ccdf`,contelig.headstart = `contelig.headstart`, contelig.earlyheadstart = `contelig.earlyheadstart`) #option to add APPLY_FATES
  csdata$value.CCDF[numkids == 0] <- 0
   csdata<-BenefitsCalculator.Healthcare(csdata, APPLY_HEALTHCARE, APPLY_MEDICAID_ADULT, APPLY_MEDICAID_CHILD, APPLY_ACA)
  csdata<-BenefitsCalculator.FoodandHousing(csdata, APPLY_SECTION8, APPLY_LIHEAP, APPLY_SNAP, APPLY_SLP, APPLY_WIC,APPLY_RAP) # OPTION TO END WITH APPLY_RAP
  csdata<-BenefitsCalculator.TaxesandTaxCredits(csdata, APPLY_EITC, APPLY_CTC, APPLY_CDCTC)
  
  csdata<-function.createVars(csdata)
  
  csdata$ProgramLoss<-0
  csdata$ProgramLoss.FATES<-0
  csdata$FATES_years<-0
  csdata$FATES_count<-0
  
  
  children <- c(ac1, ac2, ac3, ac4, ac5, ac6)
  
  if(numkids > 0 & !is.na(numkids)){
    min_ac <- min(children[!is.na(children)])
    if(min_ac == Inf){
      min_ac <- NA
    }
  }else{
    min_ac <- NA
  }
  
  if(is.na(ac1) & is.na(ac2) & is.na(ac3) & is.na(ac4) & is.na(ac5) & is.na(ac6)){
    min_ac <- NA
  }
  
  
  # csdata$ACALoss <- 0
  csdata$CHIPLoss <- 0
  csdata$MedicaidLoss <- 0
  csdata$SNAPLoss <- 0
  csdata$CCDFLoss <- 0
  csdata$HeadStartLoss <- 0
  csdata$SchoolMealsLoss <- 0
  csdata$CDCTCLoss <- 0
  csdata$EITCLoss <- 0
  csdata$CTCLoss <- 0
  csdata$S8Loss <- 0
  csdata$TANFLoss <- 0
  csdata$ProgramLoss2 <- 0
  csdata$FATESLoss <- 0
  csdata$SSILoss <- 0
  csdata$SSDILoss <- 0
  
  
  if(sum(csdata$value.aca)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.aca[i-1]>0 & csdata$value.aca[i]==0){
        csdata$ProgramLoss[i]<-1
        
      }
    }
  }
  
  x <- unique(csdata$income[csdata$ProgramLoss == 1])
  csdata$ACA1 <- x[1]
  csdata$ACA2 <- x[2]
  csdata$ACA3 <- x[3]
  csdata$ACA4 <- x[4]
  csdata$ACA5 <- x[5]
  csdata$ACA6 <- x[6]
  
  
  if(sum(csdata$value.medicaid.child)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.medicaid.child[i-1]>0 & csdata$value.medicaid.child[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$CHIPLoss <- csdata$income[i]
        
      }
    }
  }
  
  if(sum(csdata$value.medicaid.adult)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.medicaid.adult[i-1]>0 & csdata$value.medicaid.adult[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$MedicaidLoss <- csdata$income[i]
        
      }
    }
  }
  
  
  if(sum(csdata$value.snap)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.snap[i-1]>0 & csdata$value.snap[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$SNAPLoss <- csdata$income[i]
      }
    }
  }
  
  if(sum(csdata$value.CCDF)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.CCDF[i-1]>0 & csdata$value.CCDF[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$CCDFLoss <- csdata$income[i]
      }
    }
  }
  
  if(sum(csdata$value.HeadStart)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.HeadStart[i-1]>0 & csdata$value.HeadStart[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$HeadStartLoss <- csdata$income[i]
      }
    }
  }
  
  if(sum(csdata$value.schoolmeals)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.schoolmeals[i-1]>0 & csdata$value.schoolmeals[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$SchoolMealsLoss <- csdata$income[i]
      }
    }
  }
  
  if(sum(csdata$value.cdctc)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.cdctc[i-1]>0 & csdata$value.cdctc[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$CDCTCLoss <- csdata$income[i]
      }
    }
  }
  
  if(sum(csdata$value.ctc)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.ctc[i-1]>0 & csdata$value.ctc[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$CTCLoss <- csdata$income[i]
      }
    }
  }
  
  
  if(sum(csdata$value.section8)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      
      
      if(csdata$value.section8[i-1]>0 & csdata$value.section8[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$ProgramLoss2[i] <- 1
        # csdata$S8Loss <- csdata$income[i]
      }
    }
  }
  
  if(sum(csdata$value.FATES)>0 & FATES==TRUE){
    csdata$ProgramLoss.FATES<-csdata$ProgramLoss
    csdata$ProgramLoss2.FATES<-csdata$ProgramLoss2
    
    for(i in 2:length(csdata$ProgramLoss.FATES)){
      
      
      if(csdata$value.FATES[i-1]>0 & csdata$value.FATES[i]==0){
        csdata$ProgramLoss.FATES[i]<-1
        csdata$ProgramLoss2.FATES[i] <- 1
        # csdata$S8Loss <- csdata$income[i]
      }
    }
  }
  
  
  y <- unique(csdata$income[csdata$ProgramLoss2 == 1])
  csdata$S81 <- y[1]
  csdata$S82 <- y[2]
  csdata$S83 <- y[3]
  
  
  if(sum(csdata$value.eitc)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.eitc[i-1]>0 & csdata$value.eitc[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$EITCLoss <- csdata$income[i]
      }
    }
  }
  
  if(sum(csdata$value.tanf)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.tanf[i-1]>0 & csdata$value.tanf[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$TANFLoss <- csdata$income[i]
      }
    }
  }
  
  if(sum(csdata$value.ssi)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.ssi[i-1]>0 & csdata$value.ssi[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$SSILoss <- csdata$income[i]
      }
    }
  }
  
  if(sum(csdata$value.ssdi)>0){
    for(i in 2:length(csdata$ProgramLoss)){
      if(csdata$value.ssdi[i-1]>0 & csdata$value.ssdi[i]==0){
        csdata$ProgramLoss[i]<-1
        csdata$SSDILoss <- csdata$income[i]
      }
    }
  }
  
  
  csdata$ProgramLoss[csdata$income == 1000] <- 0
  
  csdata <- csdata[csdata$CareerPath == occupation.choice,]
  
  return(csdata)
  
} #end cross.section function


###############################
# NECESSARY FUNCTIONS FOR CHARTS
###############################

net.res.values <- function(d,c,ts1,te1,ts2,te2,ts3,te3,b1,b2,schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype){
  
  validate(
    need(!is.na(d), "Please complete all fields and hit the Calculate Results button to continue.")
  )
  
  validate(
    need(num_adults > 0 & !is.na(num_adults), "Tool does not support family profiles without adults. Please provide information about adults in the family.")
  )
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  
  validate(
    need(error_kids == 0 & error_adults == 0, "The number of children and/or adults is missing. Please specify valid values.")
  )
  

  
  validate(
    need(nrow(d[d$CareerPath != "Near-Minimum Wage Job",])>0, "There are not enough people with this job in this county to accurately estimate wages. Please switch to a different location or select an alternative career.")
  )
  validate(
    need(sum(d$missing)==0 & sum(d$negative)==0, "Age of an adult, child(ren), and/ or spouse or other family member income are negative, not specified, or incorrectly filled. Please specify valid values.")
  )
  
  if(manual == "manual_no"){
  validate(
    need(!is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
  )
  }
  
  if(manual == "manual_yes"){
    validate(
      need(!is.na(schooling_2) & schooling_2 >= 0 & !is.na(schooling_years_2) & schooling_years_2 >= 0 & schooling_years_2 <= 9 & schooling_months_2 >= 0 & schooling_months_2 <= 11 & !is.na(schooling_months_2 & !is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1)), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    ) 
  }
  
  if(b1 != "No programs" & sum(d$ProgramLoss)>0){
    #   plotdata = d %>% filter(ProgramLoss == 1 & CareerPath != "Near-Minimum Wage Job")
    d <- replace(d, d==0, NA)
  }else{
    # plotdata = d %>% filter(ProgramLoss == 0)
    d <- d
  }
  
  # plotdata <- data.table(plotdata, colr = c("#56B4E9"))
  
  x <- 5000
  
  maxincome <- max(d$income)
  if(maxincome > 99999){
    x <- 10000
  }else if(maxincome > 149999){
    x <- 15000
  }
  
  if(occupation_1 == occupation_2){
    d1 <- d[d$careerpathID == 1,]
    d2 <- d[d$careerpathID == 2,]
    exp_1 <- sum(d1$experience)
    exp_2 <- sum(d2$experience)
    rm(d1)
    rm(d2)
    if(exp_1 != exp_2){
    d$CareerPath <- as.character(d$CareerPath)
    d$CareerPath[d$careerpathID == 1] <- "Baseline Occupation"
    d$CareerPath <- as.factor(d$CareerPath)
    }else{
      d <- d[d$careerpathID == 2,]
    }
    
  }
  
  d$`Net Resources` <- ceiling(d$NetResources)
  d$`Career Path` <- d$CareerPath
  
  d$`Break-Even Line` <- 0
  
  if(plottype == "ggplotly"){
    
    
    net.res <- ggplot(d, aes(x=Year, group = `Career Path`, shape = `Career Path`)) +
      
      geom_line(aes(y=`Net Resources`, color = CareerPath), size=1.5) +
      
      geom_point(aes(y=`Net Resources`, color = CareerPath,
                                text = paste("<br>Career Path:", `Career Path`,
                                             "<br>Net Resources: $", format(`Net Resources`, digits = 1, big.mark = ",", scientific = FALSE))), size=4) +
      
    
    
   # net.res <- ggplot(d, aes(x=Year, y=`Net Resources`, group = `Career Path`, shape = `Career Path`))  +
  #   geom_line(aes(color = CareerPath), size=1.5) +
      
  #    geom_hline(yintercept = 0, lty =2, size = 1.5) +
    # Add dots with plotly text (otehrwise line breaks...)
      
      
      scale_x_continuous(limits=c(min(d$Year), max(d$Year)), 
                       breaks = seq(min(d$Year), max(d$Year), 1), expand = c(0.035, 0.035)) + 
    scale_y_continuous(label = comma, limits=c(min(0, round_any(min(d$NetResources), x, f = floor)), 
                                               round_any(max(d$NetResources), x, f = ceiling)), 
                       breaks = seq(min(0, round_any(min(d$NetResources), x, f = floor)),
                                    round_any(max(d$NetResources), x, f = ceiling), x)) +  
    ylab("$ per year") + xlab("Year") +
    theme(plot.title = element_text(hjust = .5, size = 20), 
          axis.text.x = element_text(angle = 90, vjust = .5), 
          axis.text = element_text(size = 17, colour = "black"), 
          axis.title = element_text(size = 16),
          panel.grid.major = element_line(colour = "gray79"),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"), 
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          axis.title.y = element_text(angle = 90, size=18,  vjust = 0.525)
    )
    
    net.res <- net.res + labs(title="", 
         caption=c,
         color = "") +
    ggtitle("Annual Net Financial Resources on Chosen Career Path") 
    
     net.res <- net.res +# geom_point(d, mapping=aes(x=Year, y=break_even, color = "Zero (\"Break-even line\")", shape = "Zero (\"Break-even line\")",
                        #        text = paste("<br>Zero (\"Break-even line\"): $", format(break_even, digits = 0, big.mark = ",", scientific = FALSE))), size=0) +
      geom_line(d, mapping=aes(x=Year, y=`Break-Even Line`, color = "Break-Even Line", shape = "Break-Even Line"), lty=2, size = 1.5) 
    # Add dots with plotly text (otehrwise line breaks...)

    
  
  }else{
    net.res <- ggplot(d, aes(x=Year, y=`Net Resources`, group = `Career Path`, shape = `Career Path`))  +
      geom_line(aes(color = CareerPath), size=1.5) +
      
      geom_hline(aes(yintercept = 0, color = "Break-Even Line"), linetype = "dashed", size = 1) +
      scale_x_continuous(limits=c(min(d$Year), max(d$Year)), 
                         breaks = seq(min(d$Year), max(d$Year), 1)) + 
      scale_y_continuous(label = comma, limits=c(min(0, round_any(min(d$NetResources), x, f = floor)), 
                                                 round_any(max(d$NetResources), x, f = ceiling)), 
                         breaks = seq(min(0, round_any(min(d$NetResources), x, f = floor)),
                                      round_any(max(d$NetResources), x, f = ceiling), x)) +  
      ylab("$ per year") + xlab("Year") +
      theme(plot.title = element_text(hjust = .5, size = 20), 
            axis.text.x = element_text(angle = 90, vjust = .5), 
            axis.text = element_text(size = 17, colour = "black"), 
            axis.title = element_text(size = 16),
            panel.grid.major = element_line(colour = "gray79"),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.key = element_rect(fill = "white"), 
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.position=c(.7,.1),
            axis.title.y = element_text(angle = 90, size=18,  vjust = 0.525)
      ) + geom_point(aes(color = CareerPath), size=5) +
      labs(title="", 
           caption=c,
           color = "") +
      ggtitle("Annual Net Financial Resources on Chosen Career Path") 
    
  }

  
  path_names <- unique(d$CareerPath)
  
  ##copy line_colors vector
  path_colors_vector <- line_colors
  
  ## add names to vector
  names(path_colors_vector) <- path_names
  
  ## add Self-Sufficiency target to vector
  path_colors_vector <- c(path_colors_vector, "#000000")
  
  names(path_colors_vector)[length(path_colors_vector)] <-  "Break-Even Line"
  
  net.res <- net.res + scale_color_manual(values = path_colors_vector)
  
  shapes <- c("Break-Even Line",shapes)
  
  net.res <- net.res + scale_shape_manual(values = shapes) + geom_hline(yintercept = 0, lty =2, size = 1.5)
  
  # plotdata$Year2 <- plotdata$Year
  d$PL <- d$ProgramLoss
  d$PL[is.na(d$ProgramLoss)] <- 0
  
  # if(b1 != "No programs" & sum(d$PL) > 0){
  #  net.res <-  net.res + geom_vline(data = plotdata,
  #                                    xintercept=plotdata$Year2[plotdata$ProgramLoss==1], color = plotdata$colr, size = 1)
  #                                    
  # }else{
  #   net.res <-  net.res
  # }
  
  ## add annotations to indicate schooling period in each career path
  
  net.res <- net.res + annotate("rect", xmin = ts1, xmax = te1, ymin = -Inf , ymax = Inf,
                                alpha = .2, fill = line_colors3[1])
  
  net.res <- net.res + annotate("rect", xmin = ts2, xmax = te2, ymin = -Inf , ymax = Inf,
                                alpha = .2, fill = line_colors3[2])
  
  net.res <- net.res + annotate("rect", xmin = ts3, xmax = te3, ymin = -Inf , ymax = Inf,
                                alpha = .2, fill = line_colors3[3])
  
  
  
  
  
  if (plottype=="ggplotly"){
    net.res <- ggplotly(net.res,tooltip = c("x","y", "group")) %>% 
      layout(legend = list(orientation = "h", x = 0, y = -0.4, title = list(text = ' '))) %>%  # GGPLOTLY doesn't know how to deal with legend location so have to do that
      layout(annotations = list(x = 0.85, y = -0.4, text = c, 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='center', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=15, color="black")))
    
    net.res <- net.res %>% config(displayModeBar = F)
  }
  
  return(print(net.res))
} #end of net.res.values function



######################
#Data return function#
######################

data_return <- function(d){
  return(data.table(d))
}


#########################
#Lifetime Net Resources##
#########################
net.res.life.values <- function(result.data.frame, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, c, plottype){
  
  validate(
    need(!is.na(result.data.frame), "Please complete all fields and hit the Calculate Results button to continue.")
  )
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  
  validate(
    need(num_adults > 0 & !is.na(num_adults), "Tool does not support family profiles without adults. Please provide information about adults in the family.")
  )
  
  
  
  validate(
    need(error_kids == 0 & error_adults == 0, "The number of children and/or adults is missing. Please specify valid values.")
  )
  
  validate(
    need(nrow(result.data.frame[result.data.frame$CareerPath != "Near-Minimum Wage Job"])>0, "There are not enough people with this job in this county to accurately estimate wages. Please switch to a different location or select an alternative career.")
  )
#  validate(
#    need(!is.na(schooling) & schooling >= 0 & !is.na(schooling_years) & schooling_years >= 0 & schooling_years <= 9 & schooling_months >= 0 & schooling_months <= 11 & !is.na(schooling_months), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
#  )
  
  if(manual == "manual_no"){
    validate(
      need(!is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    )
  }
  
  
  
  if(manual == "manual_yes"){
    validate(
      need(!is.na(schooling_2) & schooling_2 >= 0 & !is.na(schooling_years_2) & schooling_years_2 >= 0 & schooling_years_2 <= 9 & schooling_months_2 >= 0 & schooling_months_2 <= 11 & !is.na(schooling_months_2 & !is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1)), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    ) 
  }
  
  #minNRD <- pmin(0,round_any(min(result.data.frame$Total_Net_Resources), 5000, f=floor))
  #maxNRD <- round_any(max(result.data.frame$Total_Net_Resources),5000, f=ceiling)
  result.data.frame$`Total Net Resources` <- as.integer(result.data.frame$Total_Net_Resources)
  
 # result.data.frame$`Total Net Resources` <- as.factor(result.data.frame$`Total Net Resources`)

  result.data.frame$`Career Path` <- result.data.frame$CareerPath 
  
  net.res.life <- ggplot(result.data.frame) + geom_bar(aes(x = Period, y = `Total Net Resources`, fill = `Career Path`, label = `Total Net Resources`), stat = "identity", position = "dodge") 

  net.res.life <- net.res.life + scale_y_continuous(label = comma) +
  #, limits=c(minNRD, maxNRD)),breaks = seq(minNRD, maxNRD, by=round_any((maxNRD-minNRD)/7,5000,f=floor))) +
    scale_x_discrete("Time Period") +
    scale_fill_manual(values = line_colors[1])  +
    ylab("Dollars ($) per time-period") + xlab("Time Period") +
    theme(plot.title = element_text(hjust = .5, size = 20), 
          axis.text = element_text(size = 16, colour = "black"), 
          
          axis.title = element_text(size = 14),
          panel.grid.major = element_line(colour = "gray79"),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"), 
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          axis.title.y = element_text(angle = 90, size=18,  vjust = 0.525)) +
    labs(title="",
         caption="",
         fill = "") + 
    ggtitle("Financial Gains from Career Advancement")
  
  
  net.res.life <- net.res.life + geom_hline(yintercept = 0, lty = 1, size = 1)
  
  if (plottype=="ggplotly"){
    
  #  plot_ly(dat1, type = "bar", x=Score, y=Attributes, group=Item, orientation="h")
    
    #  net.res.life <- plot_ly(result.data.frame, type = "bar", x = Period, y = `Total Net Resources`, fill = `Career Path`) %>%
    
   net.res.life <- ggplotly(net.res.life,  tooltip = c("x", "label", "group")) %>% 
    layout(legend = list(orientation = "h", x = 0, y = -0.4)) %>%  # GGPLOTLY doesn't know how to deal with legend location so have to do that
    layout(annotations = list(x = 0.85, y = -0.4, text = c, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='center', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=15, color="black")))  %>% 
     config(displayModeBar = F)
  }
  return(print(net.res.life))
} #end of net.res.life function




after.tax.self.sufficiency.values <- function(d,c,ts1,te1,ts2,te2,ts3,te3,schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype){
  
  validate(
    need(!is.na(d), "Please complete all fields and hit the Calculate Results button to continue.")
  )
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  validate(
    need(num_adults > 0 & !is.na(num_adults), "Tool does not support family profiles without adults. Please provide information about adults in the family.")
  )
  
  validate(
    need(error_kids == 0 & error_adults == 0, "The number of children and/or adults is missing. Please specify valid values.")
  )
  
  
   validate(
    need(nrow(d[d$CareerPath != "Near-Minimum Wage Job",])>0, "There are not enough people with this job in this county to accurately estimate wages. Please switch to a different location or select an alternative career.")
  )
  validate(
    need(sum(d$missing)==0 & sum(d$negative)==0, "Age of an adult, child(ren), and/ or spouse or other family member income are negative, not specified, or incorrectly filled. Please specify valid values.")
  )
  
  if(manual == "manual_no"){
    validate(
      need(!is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    )
  }
  
  
  
  if(manual == "manual_yes"){
    validate(
      need(!is.na(schooling_2) & schooling_2 >= 0 & !is.na(schooling_years_2) & schooling_years_2 >= 0 & schooling_years_2 <= 9 & schooling_months_2 >= 0 & schooling_months_2 <= 11 & !is.na(schooling_months_2 & !is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1)), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    ) 
  }
  
  ## Remove Concessions line
  #d <- d[d$CareerPath != "Near-Minimum Wage Job",]
  x <- 5000
  
  maxincome <- max(d$income)
  if(maxincome > 99999){
    x <- 10000
  }else if(maxincome > 149999){
    x <- 15000
  }
  
  if(occupation_1 == occupation_2){
    d1 <- d[d$careerpathID == 1,]
    d2 <- d[d$careerpathID == 2,]
    exp_1 <- sum(d1$experience)
    exp_2 <- sum(d2$experience)
    rm(d1)
    rm(d2)
    if(exp_1 != exp_2){
      d$CareerPath <- as.character(d$CareerPath)
      d$CareerPath[d$careerpathID == 1] <- "Baseline Occupation"
      d$CareerPath <- as.factor(d$CareerPath)
    }else{
      d <- d[d$careerpathID == 2,]
    }
    
  }
  
  ## Remo
  d$`After Tax Income` <- ceiling(d$income.aftertax.noTC)
  d$`Minimum Household Budget` <- ceiling(d$SelfSufficiency)
  d$`Career Path` <- d$CareerPath

  
  if(plottype == "ggplotly"){
    
    after.tax <- ggplot(d, aes(x=Year, shape = `Career Path`)) +
      
      geom_line(aes(y=`After Tax Income`, group = `Career Path`, color = CareerPath), size=1.5) +
      
      geom_point(aes(y=`After Tax Income`, color = CareerPath, group = `Career Path`,
                     text = paste("<br>Career Path:", `Career Path`,
                                  "<br>Income After Taxes: $", 
                                  format(`After Tax Income`, digits = 1, big.mark = ",", scientific = FALSE))), size=4) +
     
      
      scale_x_continuous(limits=c(min(d$Year), max(d$Year)), 
                       breaks = seq(min(d$Year), max(d$Year), 1), expand = c(0.035, 0.035)) + 
    scale_y_continuous(label = comma, limits=c(0, pmax(round_any(max(d$SelfSufficiency[d$CareerPath != "Near-Minimum Wage Job"]), x, f = ceiling), round_any(max(d$income.aftertax.noTC), x, f=ceiling))), 
                       breaks = seq(0, pmax(round_any(max(d$SelfSufficiency[d$CareerPath != "Near-Minimum Wage Job"]), x, f=ceiling), round_any(max(d$income.aftertax.noTC), x, f=ceiling)), x))+  
    ylab("$ per year") + xlab("Year") +
    theme(plot.title = element_text(hjust = .5, size = 20), 
          axis.text.x = element_text(angle = 90, vjust = .5), 
          axis.text = element_text(size = 17, colour = "black"), 
          axis.title = element_text(size = 16),
          panel.grid.major = element_line(colour = "gray79"),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"), 
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          axis.title.y = element_text(angle = 90, size=18, vjust = 0.525)) +
    labs(title="", 
         caption=c,
         color = "") 
    
      after.tax <-after.tax +
      ggtitle("Annual Take-Home Pay and the Minimum Household Budget") +
       geom_line(aes(y=`Minimum Household Budget`, color = "Minimum Household Budget", shape = "Minimum Household Budget"), lty=2, size = 1.5) 

  }else{
    
    
     after.tax <- ggplot(d, aes(x=Year, group = `Career Path`, shape = `Career Path`)) +
      geom_line(aes(x=Year,y=`After Tax Income`, color = `Career Path`), size=1.5) 
    
      after.tax<- after.tax+ geom_line(aes(x=Year,y=`Minimum Household Budget`, color = "Minimum Household Budget"), inherit.aes=FALSE, linetype="longdash", size=1.5)
    
    after.tax<-after.tax+ geom_point(aes(x=Year,y=`After Tax Income`, color = CareerPath), size=5) +
      scale_x_continuous(limits=c(min(d$Year), max(d$Year)), 
                         breaks = seq(min(d$Year), max(d$Year), 1)) + 
      scale_y_continuous(label = comma, limits=c(0, pmax(round_any(max(d$SelfSufficiency[d$CareerPath != "Near-Minimum Wage Job"]), x, f = ceiling), round_any(max(d$income.aftertax.noTC), x, f=ceiling))), 
                         breaks = seq(0, pmax(round_any(max(d$SelfSufficiency[d$CareerPath != "Near-Minimum Wage Job"]), x, f=ceiling), round_any(max(d$income.aftertax.noTC), x, f=ceiling)), x))+  
      ylab("$ per year") + xlab("Year") +
      theme(plot.title = element_text(hjust = .5, size = 20), 
            axis.text.x = element_text(angle = 90, vjust = .5), 
            axis.text = element_text(size = 17, colour = "black"), 
            axis.title = element_text(size = 16),
            panel.grid.major = element_line(colour = "gray79"),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.key = element_rect(fill = "white"), 
            legend.position=c(.7,.1),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            axis.title.y = element_text(angle = 90, size=18, vjust = 0.525)) +
      labs(title="", 
           caption=c,
           color = "") +
      ggtitle("Annual Take-Home Pay and the Minimum Household Budget")

    
  }
  
  ## set fixed color for Self-Sufficiency line
 
  ## grab career path names
  #path_names <- as.character(unique(d$CareerPath))
  
  path_names <- unique(d$CareerPath)
  career1 <- as.character(d$CareerPath[d$careerpathID==1])
  career2 <- as.character(d$CareerPath[d$careerpathID==2])
  
  ##copy line_colors vector
  path_colors_vector <- line_colors2
  
  ## add names to vector
  names(path_colors_vector) <- path_names
  
  ## add Self-Sufficiency target to vector
  path_colors_vector <- c(path_colors_vector, "#999999")
  
      names(path_colors_vector)[length(path_colors_vector)] <-  "Minimum Household Budget"

  after.tax <- after.tax + scale_color_manual(values = path_colors_vector)
  
  
  shapes <- c(shapes,"Minimum Household Budget")
  
  after.tax <- after.tax + scale_shape_manual(values = shapes)
  
  
  ## add annotations to indicate schooling period in each career path
  after.tax <- after.tax + annotate("rect", xmin = ts1, xmax = te1,ymin = -Inf , ymax = Inf,
                                    alpha = .2, fill = line_colors3[1])+ 
    annotate("rect", xmin = ts2, xmax = te2,ymin = -Inf , ymax = Inf, alpha = .2, fill = line_colors3[2])+
    annotate("rect", xmin = ts3, xmax = te3,ymin = -Inf , ymax = Inf, alpha = .2, fill = line_colors3[3]) +
    geom_hline(yintercept = 0, lty =2, size = 1.5)
  #tooltip = c("x", "y", "group")
  if (plottype=="ggplotly"){
  after.tax <- ggplotly(after.tax, tooltip = c("x","y","group")) %>% 
    layout(legend = list(orientation = "h", x = 0, y = -0.4, title = list(text = ' '))) %>%  # GGPLOTLY doesn't know how to deal with legend location so have to do that
    layout(annotations = list(x = 0.85, y = -0.4, text = c, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='center', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=15, color="black"))) %>% 
    config(displayModeBar = F)
  
  }
  return(print(after.tax))
  
} 

#####################################
##Total Taxes Paid to Gov Bar Chart##
#####################################

total.tax.values <- function(d,c,ts1,te1,ts2,te2,ts3,te3,schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype){
  
  validate(
    need(!is.na(d), "Please complete all fields and hit the Calculate Results button to continue.")
  )
  
  validate(
    need(num_adults > 0 & !is.na(num_adults), "Tool does not support family profiles without adults. Please provide information about adults in the family.")
  )
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  validate(
    need(error_kids == 0 & error_adults == 0, "The number of children and/or adults is missing. Please specify valid values.")
  )
  
  
  validate(
    need(nrow(d[d$CareerPath != "Near-Minimum Wage Job",])>0, "There are not enough people with this job in this county to accurately estimate wages. Please switch to a different location or select an alternative career.")
  )
  validate(
    need(sum(d$missing)==0 & sum(d$negative)==0, "Age of an adult, child(ren), and/ or spouse or other family member income are negative, not specified, or incorrectly filled. Please specify valid values.")
  )
  
  if(manual == "manual_no"){
    validate(
      need(!is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    )
  }
  
  
  
  if(manual == "manual_yes"){
    validate(
      need(!is.na(schooling_2) & schooling_2 >= 0 & !is.na(schooling_years_2) & schooling_years_2 >= 0 & schooling_years_2 <= 9 & schooling_months_2 >= 0 & schooling_months_2 <= 11 & !is.na(schooling_months_2 & !is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1)), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    ) 
  }
  
  if(occupation_1 == occupation_2){
    d1 <- d[d$careerpathID == 1,]
    d2 <- d[d$careerpathID == 2,]
    exp_1 <- sum(d1$experience)
    exp_2 <- sum(d2$experience)
    rm(d1)
    rm(d2)
    if(exp_1 != exp_2){
      d$CareerPath <- as.character(d$CareerPath)
      d$CareerPath[d$careerpathID == 1] <- "Baseline Occupation"
      d$CareerPath <- as.factor(d$CareerPath)
    }else{
      d <- d[d$careerpathID == 2,]
    }
    
  }
  
  d$`Total Taxes` <- ceiling(d$total.taxes)
  d$`Career Path` <- d$CareerPath
  
  
  x <- 2500
  
  maxincome <- max(d$income)
  if(maxincome > 99999){
    x <- 5000
  }else if(maxincome > 149999){
    x <- 10000
  }
  
  if(plottype == "ggplotly"){
    taxes.total <- ggplot(d, aes(x=Year, group = `Career Path`, shape = `Career Path`)) +
      geom_line(aes(y=`Total Taxes`, color = `Career Path`), size=1.5) +
      geom_point(aes(y=`Total Taxes`, color = `Career Path`), size=5) +
      scale_x_continuous(limits=c(min(d$Year), max(d$Year)), 
                         breaks = seq(min(d$Year), max(d$Year), 1), expand = c(0.035, 0.035)) + 
      scale_y_continuous(label = comma, limits=c(min(0, round_any(min(d$total.taxes), x, f = floor)), 
                                                 round_any(max(d$total.taxes), x, f = ceiling)), 
                         breaks = seq(min(0, round_any(min(d$total.taxes), x, f = floor)),
                                      round_any(max(d$total.taxes), x, f = ceiling), x)) +  
      
      ylab("$ per year") + xlab("Year") +
      theme(plot.title = element_text(hjust = .5, size = 20), 
            axis.text.x = element_text(angle = 90, vjust = .5), 
            axis.text = element_text(size = 17, colour = "black"), 
            axis.title = element_text(size = 16),
            panel.grid.major = element_line(colour = "gray79"),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.key = element_rect(fill = "white"), 
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            axis.title.y = element_text(angle = 90, size=18, vjust = 0.54)) +
      labs(title="", 
           caption=c,
           color = " ") +
      ggtitle("Taxes Paid")
    
  }else{
    
    taxes.total <- ggplot(d, aes(x=Year, group = `Career Path`, shape = `Career Path`)) +
      geom_line(aes(y=`Total Taxes`, color = `Career Path`), size=1.5) +
      geom_point(aes(y=`Total Taxes`, color = `Career Path`), size=5) +
      scale_x_continuous(limits=c(min(d$Year), max(d$Year)), 
                         breaks = seq(min(d$Year), max(d$Year), 1)) + 
      scale_y_continuous(label = comma, 
                         breaks = seq(min(0,(round_any(min(d$total.taxes),1000,f=ceiling))),
                                      round_any(max(d$total.taxes)+5000,1000, f=ceiling), 
                                      by=round_any(max(d$total.taxes)+5000,1000,f=ceiling)/10)) +  
      ylab("$ per year") + xlab("Year") +
      theme(plot.title = element_text(hjust = .5, size = 20), 
            axis.text.x = element_text(angle = 90, vjust = .5), 
            axis.text = element_text(size = 17, colour = "black"), 
            axis.title = element_text(size = 16),
            panel.grid.major = element_line(colour = "gray79"),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.key = element_rect(fill = "white"), 
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.position=c(.7,.1),
            axis.title.y = element_text(angle = 90, size=18, vjust = 0.54)) +
      labs(title="", 
           caption=c,
           color = " ") +
      ggtitle("Taxes Paid")
    
    
  }

  ## add annotations to indicate schooling period in each career path
  path_names <- unique(d$CareerPath)
  
  ##copy line_colors vector
  path_colors_vector <- line_colors2
  
  ## add names to vector
  names(path_colors_vector) <- path_names
  
  ## add Self-Sufficiency target to vector
  path_colors_vector <- c(path_colors_vector, "darkgrey")
  
  names(path_colors_vector)[length(path_colors_vector)] <-  "Minimum Household Budget"

  # after.tax <- after.tax + scale_color_manual(values = c(line_colors[1:(length(career_path)+1)]))
  taxes.total <- taxes.total + scale_color_manual(values = path_colors_vector)
  
  
  taxes.total <- taxes.total + annotate("rect", xmin = ts1, xmax = te1, ymin = -Inf , ymax = Inf,
                                        alpha = .2, fill = line_colors3[1])
  
  taxes.total <- taxes.total + annotate("rect", xmin = ts2, xmax = te2, ymin = -Inf , ymax = Inf,
                                        alpha = .2, fill = line_colors3[2])
  
  taxes.total <- taxes.total + annotate("rect", xmin = ts3, xmax = te3, ymin = -Inf , ymax = Inf,
                                        alpha = .2, fill = line_colors3[3])
  
  
  
  taxes.total <- taxes.total + geom_hline(yintercept = 0, lty =2, size = 1.5)
  
  if (plottype=="ggplotly"){
  taxes.total <- ggplotly(taxes.total, tooltip = c("y", "x", "group")) %>% 
    layout(legend = list(orientation = "h", x = 0, y = -0.4, title = list(text = ' '))) %>%  # GGPLOTLY doesn't know how to deal with legend location so have to do that
    layout(annotations = list(x = 0.85, y = -0.4, text = c, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='center', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=15, color="black")))%>% 
                        config(displayModeBar = F)
  
}
  return(print(taxes.total))
  
  
}

net.tax.life.values <- function(result.tax.data.frame, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, c, occupation_1, occupation_2, plottype){
  
  validate(
    need(!is.na(result.tax.data.frame), "Please complete all fields and hit the Calculate Results button to continue.")
  )
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  validate(
    need(num_adults > 0 & !is.na(num_adults), "Tool does not support family profiles without adults. Please provide information about adults in the family.")
  )
  
  validate(
    need(error_kids == 0 & error_adults == 0, "The number of children and/or adults is missing. Please specify valid values.")
  )
  
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  
  validate(
    need(nrow(result.tax.data.frame[result.tax.data.frame$CareerPath != "Near-Minimum Wage Job"])>0, "There are not enough people with this job in this county to accurately estimate wages. Please switch to a different location or select an alternative career.")
  )
  
  if(manual == "manual_no"){
    validate(
      need(!is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    )
  }
  
  
  
  if(manual == "manual_yes"){
    validate(
      need(!is.na(schooling_2) & schooling_2 >= 0 & !is.na(schooling_years_2) & schooling_years_2 >= 0 & schooling_years_2 <= 9 & schooling_months_2 >= 0 & schooling_months_2 <= 11 & !is.na(schooling_months_2 & !is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1)), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    ) 
  }
  
  result.tax.data.frame$`Total Net Taxes` <- ceiling(result.tax.data.frame$Total_Net_Taxes)
  result.tax.data.frame$`Career Path` <- result.tax.data.frame$CareerPath
  
  net.tax.life <-  ggplot(result.tax.data.frame) + scale_y_continuous(label = comma) +
    geom_bar(aes(x = Period, y = `Total Net Taxes`, fill = `Career Path`, label = `Total Net Taxes`), stat = "identity", position = "dodge") +
    scale_x_discrete("Time Period") +
    scale_fill_manual(values = line_colors[1])  +
    theme(plot.title = element_text(hjust = .5, size = 20), 
          axis.text = element_text(size = 16, colour = "black"), 
          axis.title = element_text(size = 16),
          panel.grid.major = element_line(colour = "gray79"),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"), 
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16)) +
    labs(title="",
         caption="",
         fill = "") + 
    ggtitle("Difference in Net Taxes of Target & Comparison Occupations")+ylab("Difference in Net Taxes")
  
  net.tax.life <- net.tax.life + geom_hline(yintercept = 0, lty = 1, size = 1)
  
  if (plottype=="ggplotly"){
  net.tax.life <- ggplotly(net.tax.life, tooltip = c("x", "label", "group")) %>% 
    layout(legend = list(orientation = "h", x = 0, y = -0.4)) %>%  # GGPLOTLY doesn't know how to deal with legend location so have to do that
    layout(annotations = list(x = 0.85, y = -0.4, text = c, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='center', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=15, color="black"))) %>% 
    config(displayModeBar = F)
  }
  
  return(print(net.tax.life))
  
  #ggsave('graphs/net.res.life.png', width = 14, height = 8, units = "in", dpi = 500)
  
  # Return a list containing the filename
  #list(src = 'graphs/net.res.life.png',
  #     contentType = 'image/png',
  #     width = 1200,
  #     height = 690,
  #     alt = "This is alternate text")
  
  
}


##################################################
##Public Assistance  on chosen Career Path##
##################################################

transfers.breakdown.values <- function(d,c,b1,b2,income.otherfamily,schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype){
  
  validate(
    need(!is.na(d), "Please complete all fields and hit the Calculate Results button to continue.")
  )
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  validate(
    need(num_adults > 0 & !is.na(num_adults), "Tool does not support family profiles without adults. Please provide information about adults in the family.")
  )
  
  validate(
    need(error_kids == 0 & error_adults == 0, "The number of children and/or adults is missing. Please specify valid values.")
  )
  
  
  
  validate(
    need(nrow(d[d$CareerPath != "Near-Minimum Wage Job",])>0, "There are not enough people with this job in this county to accurately estimate wages. Please switch to a different location or select an alternative career.")
  )
  validate(
    need(sum(d$missing)==0 & sum(d$negative)==0, "Age of an adult, child(ren), and/ or spouse or other family member income are negative, not specified, or incorrectly filled. Please specify valid values.")
  )
  
  if(manual == "manual_no"){
    validate(
      need(!is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    )
  }
  
  
  
  if(manual == "manual_yes"){
    validate(
      need(!is.na(schooling_2) & schooling_2 >= 0 & !is.na(schooling_years_2) & schooling_years_2 >= 0 & schooling_years_2 <= 9 & schooling_months_2 >= 0 & schooling_months_2 <= 11 & !is.na(schooling_months_2 & !is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1)), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    ) 
  }
  
  if(manual == "manual_yes"){
    d1 <- d[d$careerpathID == 1,]
    d2 <- d[d$careerpathID == 2,]
    exp_1 <- sum(d1$experience)
    exp_2 <- sum(d2$experience)
    rm(d1)
    rm(d2)
    
    if(occupation_1 == occupation_2){
      
      if(exp_1 != exp_2){
        d$CareerPath <- as.character(d$CareerPath)
        d$CareerPath[d$careerpathID == 1] <- "Baseline Occupation"
        d$CareerPath <- as.factor(d$CareerPath)
      }else{
        d <- d[d$careerpathID == 2,]
      }
      
      
    }else{
      d<-d
    }
    
    
  }
  if (plottype=="ggplotly"){
    
  validate(
    need((sum(d$total.transfers) != 0), "Public Assistance not selected or applicable to current scenario.")
  )
  }
  
  if(sum(d$total.transfers) ==0){
    maxbenefits <- 10000
  }else{
    maxbenefits <- max(d$total.transfers)
  }
  

  d[is.na(d)] <- 0
  
  d$value.aca <- as.integer(d$value.aca)
  d$value.medicaid.adult <- as.integer(d$value.medicaid.adult)
  d$value.medicaid.child <- as.integer(d$value.medicaid.child)
  d$value.section8 <- as.integer(d$value.section8)
  d$value.HeadStart <- as.integer(d$value.HeadStart)
  d$value.earlyHeadStart <- as.integer(d$value.earlyHeadStart)
  d$value.PreK <- as.integer(d$value.PreK)
  d$value.CCDF <- as.integer(d$value.CCDF)
  d$value.snap <- as.integer(d$value.snap)
  d$value.schoolmeals <- as.integer(d$value.schoolmeals)
  d$value.cdctc <- as.integer(d$value.cdctc)
  d$value.ctc <- as.integer(d$value.ctc)
  d$value.wic <- as.integer(d$value.wic)
  d$value.eitc <- as.integer(d$value.eitc)
  d$value.liheap <- as.integer(d$value.liheap)
  d$value.tanf <- as.integer(d$value.tanf)
  d$value.FATES <- as.integer(d$value.FATES)
  d$value.ssi <- as.integer(d$value.ssi)
  d$value.ssdi <- as.integer(d$value.ssdi)
  
  if(b1 != "No programs" & maxbenefits != 10000){
    d <- replace(d, d==0, NA)
  }else{
    d<-d
  }
  

  d <- subset(d, select = c("Year", "CareerPath","ProgramLoss",
                            "value.aca",
                            "value.medicaid.adult",
                            "value.medicaid.child",
                            "value.section8",
                            "value.HeadStart",
                            "value.earlyHeadStart",
                            "value.PreK",
                            "value.CCDF", 
                            "value.snap",
                            "value.schoolmeals",
                            "value.cdctc",
                            "value.ctc",
                            "value.wic",
                            "value.eitc",
                            "value.liheap",
                            "value.tanf",
                            "value.FATES",
                            "value.ssi",
                            "value.ssdi"
  ))
  

  
  ## Rename Factors
  names(d)[names(d)=="value.medicaid.adult"] <- "Medicaid for Adults"
  names(d)[names(d)=="value.medicaid.child"] <- "Medicaid for Children/CHIP"
  names(d)[names(d)=="value.snap"] <- "SNAP"
  names(d)[names(d)=="value.wic"] <- "WIC"
  names(d)[names(d)=="value.cdctc"] <- "CDCTC"
  names(d)[names(d)=="value.section8"] <- "Housing Voucher"
  names(d)[names(d)=="value.CCDF"] <- "CCDF"
  names(d)[names(d)=="value.aca"] <- "Health Insurance Marketplace Subsidy"
  names(d)[names(d)=="value.eitc"] <- "EITC"
  names(d)[names(d)=="value.ctc"] <- "CTC"
  names(d)[names(d)=="value.schoolmeals"] <- "Free or Reduced Price School Meals"
  names(d)[names(d)=="value.liheap"] <- "LIHEAP"
  names(d)[names(d)=="value.HeadStart"] <- "Head Start"
  names(d)[names(d)=="value.earlyHeadStart"] <- "Early Head Start"
  names(d)[names(d)=="value.PreK"] <- "PreK"
  names(d)[names(d)=="tax.income.fed"] <- "Federal Income Tax"
  names(d)[names(d)=="tax.income.state"] <- "State Income Tax"
  names(d)[names(d)=="tax.FICA"] <- "FICA Tax"
  names(d)[names(d)=="value.tanf"] <- "TANF"
  names(d)[names(d)=="value.FATES"] <- "FATES"
  names(d)[names(d)=="value.ssi"] <- "SSI"
  names(d)[names(d)=="value.ssdi"] <- "SSDI"
  
  
  #remove fates column if not fates tab
 # if (fatestab==FALSE) {d<-d %>% select(-c("FATES"))}
  
  #rehape 
  benefits.decomp <- melt(d, id.vars=c("Year","CareerPath", "ProgramLoss"),
                          variable.name="Program",value.name="Transfer")
  
  benefits.decomp<-cbind(benefits.decomp,maxbenefits)
  
  ## define colors
  benefit_colors <- c("CDCTC"="#F0E442", "Medicaid for Adults"="#332288", 
                      "Medicaid for Children/CHIP"="#E69F00", "SNAP"="#D55E00", "Housing Voucher"="#CC79A7", 
                      "CCDF"="#882255", "Health Insurance Marketplace Subsidy"="#117733", "EITC"="#888888", "CTC"="#44AA99", "TANF"="#0072B2",
                      "Free or Reduced Price School Meals" = "#999933", "Head Start" = "#88CCEE", "Early Head Start" = "light green", "PreK" = "black"
                      , "LIHEAP" = "#CC6677", "WIC" = "#AA4499", "FATES" = "medium blue","SSI"="red3","SSDI"="turquoise4"
  )
  
  ##########################
  ## plot stacked bar chart 
  ##########################
  
  unique_Careers <- unique(benefits.decomp$CareerPath)
  
  sum_career_1 <- sum(benefits.decomp$Transfer[benefits.decomp$CareerPath == unique_Careers[1]], na.rm=T)
  sum_career_2 <- sum(benefits.decomp$Transfer[benefits.decomp$CareerPath == unique_Careers[2]], na.rm=T)
  unique_benefits <- c(as.character(benefits.decomp$Program[benefits.decomp$CareerPath == unique_Careers[1] & !is.na(benefits.decomp$Transfer)], na.rm=T))
  unique_benefits_1 <- unique(unique_benefits)
  unique_benefits <- c(as.character(benefits.decomp$Program[benefits.decomp$CareerPath == unique_Careers[2] & !is.na(benefits.decomp$Transfer)], na.rm=T))
  unique_benefits_2 <- unique(unique_benefits)
  
  
  if(sum_career_1 == 0){
    benefits.decomp$Transfer[benefits.decomp$CareerPath == unique_Careers[1] & benefits.decomp$Program %in% unique_benefits_2] <- 0
  }
  
  if(sum_career_2 == 0){
    benefits.decomp$Transfer[benefits.decomp$CareerPath == unique_Careers[2] & benefits.decomp$Program %in% unique_benefits_1] <- 0
    
  }
  
  
  benefits.plot <- ggplot(benefits.decomp, aes(fill = Program, y = Transfer, x = Year)) + 
    geom_bar(position = "stack", stat = "identity") + facet_wrap(~CareerPath)
  
  benefits.plot <- benefits.plot + scale_fill_manual(values = benefit_colors) +
    scale_x_continuous(labels = seq(min(d$Year), min(d$Year)+24, 1), limits =c(min(d$Year)-0.51, min(d$Year)+24.51), breaks = seq(min(d$Year), min(d$Year)+24, 1), expand = c(0.03, 0.03)) +
    scale_y_continuous(label = comma, limits = c(0, round_any(max(maxbenefits), 5000, f = ceiling)), #ET: changed to refer to maxbenefits
                       breaks = seq(0, round_any(max(maxbenefits), 5000, f = ceiling), 5000)) 
  
  list_benefits <- as.character(b2)
  
  benefits.plot <- benefits.plot +
    theme(plot.title = element_text(hjust = .5, size = 20),
          axis.text = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 90),
          panel.grid.major = element_line(colour = "gray79"),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position = "bottom") +
    labs(title = "*Public Assistance on Chosen Career Path",
         caption = paste0(c),
         fill = "Programs:") 
  

  
  #turn ggplot into ggplotly
  if (plottype=="ggplotly"){
    benefits.plot <- ggplotly(benefits.plot) %>% 
      layout( legend = list(orientation = "h", x = 0, y = -0.4)) %>%  # GGPLOTLY doesn't know how to deal with legend location so have to do that
      layout(annotations = list(x = 0.85, y = -0.4, text = c, 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='center', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=15, color="black"))) %>% 
      config(displayModeBar = F) %>%  layout(xaxis = list(
        title=list(text='Year',
                   font=list(size=18))),
      xaxis2 = list(
        title=list(text='Year',
                   font=list(size=18)))
      )
    
    benefits.plot <- benefits.plot %>%     layout(                                
      yaxis = list(title="$ per year"))

  }
  
  if(plottype=="ggplot"){
    benefits.plot <- benefits.plot + theme(legend.text = element_text(size = 9)) 
  }
  
  return(print(benefits.plot)) 
} #end of transfers.breakdown.values



############################
#Self-suff target breakdown#
############################

expenses.breakdown.values <- function(d,c,schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, plottype){

  
  validate(
    need(!is.na(d), "Please complete all fields and hit the Calculate Results button to continue.")
  )
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  validate(
    need(num_adults > 0 & !is.na(num_adults), "Tool does not support family profiles without adults. Please provide information about adults in the family.")
  )
  ## Remove Concessions line
 # d <- d[d$CareerPath != "Near-Minimum Wage Job",]
  d <- d[d$careerpathID == 2,]
  
  validate(
    need(error_kids == 0 & error_adults == 0, "The number of children and/or adults is missing. Please specify valid values.")
  )

  validate(
    need(nrow(d)>0, "There are not enough people with this job in this county to accurately estimate wages. Please switch to a different location or select an alternative career.")
  )
  validate(
    need(sum(d$missing)==0 & sum(d$negative)==0, "Age of an adult, child(ren), and/ or spouse or other family member income are negative, not specified, or incorrectly filled. Please specify valid values.")
  )

  if(manual == "manual_no"){
    validate(
      need(!is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    )
  }

  if(manual == "manual_yes"){
    validate(
      need(!is.na(schooling_2) & schooling_2 >= 0 & !is.na(schooling_years_2) & schooling_years_2 >= 0 & schooling_years_2 <= 9 & schooling_months_2 >= 0 & schooling_months_2 <= 11 & !is.na(schooling_months_2 & !is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1)), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    )
  }

  d$exp.misc <- d$exp.misc 
  d$exp.childcare <- as.integer(d$exp.childcare)
  d$exp.food <- as.integer(d$exp.food)
  d$exp.healthcare.SS <- as.integer(d$exp.healthcare.SS)
  d$exp.transportation <- as.integer(d$exp.transportation)
  d$exp.misc <- as.integer(d$exp.misc)
  d$exp.tech <- as.integer(d$exp.tech)
  d$exp.housing <- as.integer(d$exp.housing)
  
  d <- d %>%
    select(c("Year", "CareerPath", "exp.childcare","exp.healthcare.SS","exp.food",'exp.rentormortgage',
             "exp.utilities","exp.transportation","exp.misc","exp.tech"))
  
  maxexpenses <- d %>%
    mutate(totalexpenses=exp.childcare+exp.healthcare.SS+exp.food+exp.rentormortgage+exp.utilities+
             exp.utilities+exp.transportation+exp.misc+exp.tech) %>%
    summarize(maxexpensetotal=max(totalexpenses))
  
  ## reshape data into long form
  expenses.decomp <- melt(d, id.vars=c("Year","CareerPath"),
                          variable.name="Expense",value.name="Amount")
  
  expenses.decomp$Expense <- revalue(expenses.decomp$Expense,
                                     c("exp.childcare" = "Child care",
                                       "exp.healthcare.SS" = "Health care",
                                       "exp.food" = "Food",
                                       "exp.rentormortgage" = "Rent",
                                       "exp.utilities" = "Utilities",
                                       "exp.transportation" = "Transportation",
                                       "exp.tech" = "Technology",
                                       "exp.misc" = "Misc"))
  
  expense_colors <- c("Child care"="#117733","Food"= "#AA4499","Health care"= "#D55E00",
                      "Rent" = "#44AA99", "Utilities" = "#332288",
                      "Transportation"=  "#661100", "Technology" = "#56B4E9", "Misc"= "#DDCC77")
  
  ## plot stacked bar chart

  expenses.decomp<-cbind(expenses.decomp,maxexpenses)

  expenses.plot <- ggplot(expenses.decomp, aes(fill = Expense, y = Amount, x = Year)) +
    geom_bar(position = "stack", stat = "identity")

  expenses.plot <- expenses.plot + scale_fill_manual(values = expense_colors)

  expenses.plot <- expenses.plot + scale_x_continuous(labels = min(expenses.decomp$Year):max(expenses.decomp$Year),
                                                      breaks = min(expenses.decomp$Year):max(expenses.decomp$Year), expand = c(0.03, 0.03))

  expenses.plot <- expenses.plot + scale_y_continuous(label = comma, limits = c(0, round_any(max(maxexpenses), 5000, f = ceiling)),
                                                      breaks = seq(0, round_any(max(maxexpenses), 5000, f = ceiling), 5000))

  expenses.plot <- expenses.plot + xlab("Year")

  expenses.plot <- expenses.plot +
    theme(plot.title = element_text(hjust = .5, size = 20),
          axis.text = element_text(size = 17, colour = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 90, vjust = .5),
          panel.grid.major = element_line(colour = "gray79"),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          axis.title.y = element_text(angle = 90, size=18,  vjust = 0.615),
          legend.position="bottom")

    expenses.plot<-expenses.plot+labs(title="Expenses Included in the Minimum Household Budget",
                                     caption=paste0(c),
                                     fill = "Expense:") + ylab("$ per year")
    #}

  if (plottype=="ggplotly"){
    expenses.plot <- ggplotly(expenses.plot) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.4)) %>%  # GGPLOTLY doesn't know how to deal with legend location so have to do that
      layout(annotations = list(x = 0.85, y = -0.4, text = c,
                                showarrow = F, xref='paper', yref='paper',
                                xanchor='center', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=15, color="black"))) %>%
      config(displayModeBar = F)
  }

  return(print(expenses.plot))
} #end of function


################################################
##Public Assistance by Employment income chart##
################################################

csbenefit.breakdown.values <- function(d,b1,b2,c,schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, plottype){
  
  validate(
    need(!is.na(d), "Please complete all fields and hit the Calculate Results button to continue.")
  )
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  validate(
    need(num_adults > 0 & !is.na(num_adults), "Tool does not support family profiles without adults. Please provide information about adults in the family.")
  )
  
  validate(
    need(error_kids == 0 & error_adults == 0, "The number of children and/or adults is missing. Please specify valid values.")
  )
  
  validate(
    need(sum(d$missing)==0 & sum(d$negative)==0, "Age of an adult, child(ren), and/ or spouse or other family member income are negative, not specified, or incorrectly filled. Please specify valid values.")
  )
  
  
  if(manual == "manual_no"){
    validate(
      need(!is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    )
  }
  
  
  
  if(manual == "manual_yes"){
    validate(
      need(!is.na(schooling_2) & schooling_2 >= 0 & !is.na(schooling_years_2) & schooling_years_2 >= 0 & schooling_years_2 <= 9 & schooling_months_2 >= 0 & schooling_months_2 <= 11 & !is.na(schooling_months_2 & !is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1)), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    ) 
  }
  
  if (plottype=="ggplotly"){
    validate(
      need((sum(d$total.transfers) != 0), "Public Assistance not selected or applicable to current scenario.")
    )
  }
  
    if(sum(d$total.transfers) ==0){ maxbenefits <- 10000}
    else{maxbenefits <- max(d$total.transfers)}
  
    yyz <- 0
  if(d$famsize[1] > 2){yyz <- 0}
  
  d <- subset(d, select = c("income", "total.transfers",
                            "ProgramLoss",
                            "value.aca",
                            "value.medicaid.adult", 
                            "value.medicaid.child",
                            "value.section8",
                            "value.HeadStart",
                            "value.earlyHeadStart",
                            "value.PreK",
                            "value.CCDF",
                            "value.tanf",
                            "value.wic",
                            "value.snap",
                            "value.schoolmeals",
                            "value.cdctc",
                            "value.ctc",
                            "value.eitc",
                            "value.liheap",
                            "value.FATES",
                            "value.ssi",
                            "value.ssdi"
  ))
  
  
  d$value.aca <- as.integer(d$value.aca)
  d$value.medicaid.adult <- as.integer(d$value.medicaid.adult)
  d$value.medicaid.child <- as.integer(d$value.medicaid.child)
  d$value.section8 <- as.integer(d$value.section8)
  d$value.HeadStart <- as.integer(d$value.HeadStart)
  d$value.earlyHeadStart <- as.integer(d$value.earlyHeadStart)
  d$value.PreK <- as.integer(d$value.PreK)
  d$value.CCDF <- as.integer(d$value.CCDF)
  d$value.snap <- as.integer(d$value.snap)
  d$value.schoolmeals <- as.integer(d$value.schoolmeals)
  d$value.cdctc <- as.integer(d$value.cdctc)
  d$value.ctc <- as.integer(d$value.ctc)
  d$value.wic <- as.integer(d$value.wic)
  d$value.eitc <- as.integer(d$value.eitc)
  d$value.liheap <- as.integer(d$value.liheap)
  d$value.tanf <- as.integer(d$value.tanf)
  d$value.FATES <- as.integer(d$value.FATES)
  d$value.ssi <- as.integer(d$value.ssi)
  d$value.ssdi <- as.integer(d$value.ssdi)
  
  names(d)[names(d)=="value.medicaid.adult"] <- "Medicaid for Adults"
  names(d)[names(d)=="value.medicaid.child"] <- "Medicaid for Children/CHIP"
  names(d)[names(d)=="value.snap"] <- "SNAP"
  names(d)[names(d)=="value.wic"] <- "WIC"
  names(d)[names(d)=="value.cdctc"] <- "CDCTC"
  names(d)[names(d)=="value.section8"] <- "Housing Voucher"
  names(d)[names(d)=="value.CCDF"] <- "CCDF"
  names(d)[names(d)=="value.aca"] <- "Health Insurance Marketplace Subsidy"
  names(d)[names(d)=="value.eitc"] <- "EITC"
  names(d)[names(d)=="value.ctc"] <- "CTC"
  names(d)[names(d)=="value.schoolmeals"] <- "Free or Reduced Price School Meals"
  names(d)[names(d)=="value.liheap"] <- "LIHEAP"
  names(d)[names(d)=="value.HeadStart"] <- "Head Start"
  names(d)[names(d)=="value.earlyHeadStart"] <- "Early Head Start"
  names(d)[names(d)=="value.PreK"] <- "PreK"
  names(d)[names(d)=="tax.income.fed"] <- "Federal Income Tax"
  names(d)[names(d)=="tax.income.state"] <- "State Income Tax"
  names(d)[names(d)=="tax.FICA"] <- "FICA Tax"
  names(d)[names(d)=="value.tanf"] <- "TANF"
  names(d)[names(d)=="value.FATES"] <- "FATES"
  names(d)[names(d)=="value.ssi"] <- "SSI"
  names(d)[names(d)=="value.ssdi"] <- "SSDI"
 
  #data for plot
  csbenefit.decomp <- d %>% 
    select(-c(total.transfers)) %>% 
    melt(id.vars=c("income", "ProgramLoss"),
         variable.name="Program",value.name="Transfer")
  
  ## Rename Factors 
  
  if(b1 != "No programs" & sum(d$ProgramLoss)>0){
    csbenefit.decomp$Transfer <- replace(csbenefit.decomp$Transfer, csbenefit.decomp$Transfer==0, NA)
  }
  
  csbenefit.decomp <- csbenefit.decomp[is.na(csbenefit.decomp$Transfer) == FALSE, ]
  csbenefit.decomp$Program <- droplevels(csbenefit.decomp$Program)
  
  
  csbenefit.decomp<-cbind(csbenefit.decomp,maxbenefits)
  ## define color
  
  
  csbenefit_colors <- c("CDCTC"="#F0E442", "Medicaid for Adults"="#332288", 
                        "Medicaid for Children/CHIP"="#E69F00", "SNAP"="#D55E00", "Housing Voucher"="#CC79A7", 
                        "CCDF"="#882255", "Health Insurance Marketplace Subsidy"="#117733", "EITC"="#888888", "CTC"="#44AA99", "TANF"="#0072B2",
                        "Free or Reduced Price School Meals" = "#999933", "Head Start" = "#88CCEE", "Early Head Start" = "light green", "PreK" = "black"
                        , "LIHEAP" = "#CC6677", "WIC" = "#AA4499", "FATES" = "medium blue","SSI"="red3","SSDI"="turquoise4")
  
  ####################
  ## plot stacked bar chart 
  ####################
  
  
  csbenefits.plot <- ggplot(csbenefit.decomp, aes(fill = Program, y = Transfer, x = income)) + 
    geom_bar(position = "stack", stat = "identity") 
  
  csbenefits.plot <- csbenefits.plot + scale_fill_manual(values = csbenefit_colors)
  
  
  if(b1 != "No programs" & sum(d$ProgramLoss)>0){
    
    #x axis will only show income through which programs are received
    
    csbenefits.plot <- csbenefits.plot + scale_x_continuous(labels = comma, limits=c(0,min(120000,round_any(max(d$income[d$total.transfers > 0]),5000, f=ceiling)
                                                                                           +15000)), 
                                                            breaks = seq(0,min(120000,round_any(max(d$income[d$total.transfers > 0]),5000, f=ceiling)+15000),5000), expand = c(0.025, 0.025))
    csbenefits.plot <- csbenefits.plot + scale_y_continuous(label = comma, limits = c(0, round_any(maxbenefits, 5000, f = ceiling) + yyz), 
                                                            breaks = seq(0, round_any(maxbenefits, 5000, f = ceiling) + yyz, 5000))
  }else{
    csbenefits.plot <- csbenefits.plot + scale_x_continuous(labels = comma, limits=c(0,75000), 
                                                            breaks = seq(0,75000,5000))
    
    csbenefits.plot <- csbenefits.plot + scale_y_continuous(label = comma, limits = c(0, 40000), #ET: changed to refer to maxbenefits
                                                            breaks = seq(0, 40000, 5000))
  }
  
  
  csbenefits.plot <- csbenefits.plot + xlab("Employment Income ($)")
  
  if(b1 != "No programs" & sum(d$ProgramLoss)>0){
    csbenefits.plot <- csbenefits.plot +
      theme(plot.title = element_text(hjust = .5, size = 20),
            
            axis.text.x = element_text(angle = 90, vjust = .5), 
            axis.text = element_text(size = 15, colour = "black"),
            axis.title = element_text(size = 16),
            
            panel.grid.major = element_line(colour = "gray79"),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.key = element_rect(fill = "white"),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.position = "bottom") +
      labs(title = "Public Assistance by Employment Income",
           caption = paste0(c),
           fill = "Programs:") + ylab("Dollars ($)") 
  }else{
    csbenefits.plot <- csbenefits.plot +
      theme(plot.title = element_text(hjust = .5, size = 20),
            
            axis.text.x = element_text(angle = 90, vjust = .5), 
            axis.text = element_text(size = 17, colour = "black"),
            axis.title = element_text(size = 16),
            
            panel.grid.major = element_line(colour = "gray79"),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "none") +
      labs(title = "Public Assistance by Employment Income",
           caption = paste0(c),
           fill = "Programs:") + ylab("Dollars ($)") 
  }
  if (plottype=="ggplotly"){
    csbenefits.plot <- ggplotly(csbenefits.plot) %>% 
      layout(legend = list(orientation = "h", x = 0, y = -0.6)) %>%  # GGPLOTLY doesn't know how to deal with legend location so have to do that
      layout(annotations = list(x = 0.85, y = -0.6, text = c, 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='center', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=15, color="black")))  
    csbenefits.plot <- csbenefits.plot %>% config(displayModeBar = F)
  }
  
  if(plottype=="ggplot"){
    csbenefits.plot <- csbenefits.plot + theme(legend.text = element_text(size = 10)) 
  }
  
  return(print(csbenefits.plot)) 
  
} #end of csbenefit.breakdown.values




gross.income.values <- function(d,c,ts1,te1,ts2,te2,ts3,te3,schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype){
  
  validate(
    need(!is.na(d), "Please complete all fields and hit the Calculate Results button to continue.")
  )
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  validate(
    need(num_adults > 0 & !is.na(num_adults), "Tool does not support family profiles without adults. Please provide information about adults in the family.")
  )
  
  
  validate(
    need(error_kids == 0 & error_adults == 0, "The number of children and/or adults is missing. Please specify valid values.")
  )
  
  validate(
    need(sum(d$missing)==0 & sum(d$negative)==0, "Age of an adult, child(ren), and/ or spouse or other family member income are negative, not specified, or incorrectly filled. Please specify valid values.")
  )
  
  if(manual == "manual_no"){
    validate(
      need(!is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    )
  }
  
  if(manual == "manual_yes"){
    validate(
      need(!is.na(schooling_2) & schooling_2 >= 0 & !is.na(schooling_years_2) & schooling_years_2 >= 0 & schooling_years_2 <= 9 & schooling_months_2 >= 0 & schooling_months_2 <= 11 & !is.na(schooling_months_2 & !is.na(schooling_1) & schooling_1 >= 0 & !is.na(schooling_years_1) & schooling_years_1 >= 0 & schooling_years_1 <= 9 & schooling_months_1 >= 0 & schooling_months_1 <= 11 & !is.na(schooling_months_1)), "Duration of training / schooling is negative or unspecified. Please specify valid values.")
    ) 
  }
  
  validate(
    need(nrow(d[d$CareerPath != "Near-Minimum Wage Job",])>0, "There are not enough people with this job in this county to accurately estimate wages. Please switch to a different location or select an alternative career.")
  )
  
  x <- 5000
  
  maxincome <- max(d$income)
  if(maxincome > 99999){
    x <- 10000
  }else if(maxincome > 149999){
      x <- 15000}
  
  if(occupation_1 == occupation_2){
    d1 <- d[d$careerpathID == 1,]
    d2 <- d[d$careerpathID == 2,]
    exp_1 <- sum(d1$experience)
    exp_2 <- sum(d2$experience)
    rm(d1)
    rm(d2)
    if(exp_1 != exp_2){
      d$CareerPath <- as.character(d$CareerPath)
      d$CareerPath[d$careerpathID == 1] <- "Baseline Occupation"
      d$CareerPath <- as.factor(d$CareerPath)
    }
    else{d <- d[d$careerpathID == 2,]}
    
  }
  
  d$`Income Before Taxes` <- ceiling(d$individual_income_BT)
  d$`Career Path` <- d$CareerPath
  
  if(plottype == "ggplotly"){
  gross.income <- ggplot(d, aes(x=Year, group = `Career Path`, shape = `Career Path`)) +
    geom_line(aes(y=`Income Before Taxes`, color = CareerPath), size=1.5) +
    geom_point(aes(y=`Income Before Taxes`, color = CareerPath), size=5) +
    scale_x_continuous(limits=c(min(d$Year), max(d$Year)), 
                       breaks = seq(min(d$Year), max(d$Year), 1), expand = c(0.035, 0.035)) + 
    scale_y_continuous(label = comma, limits=c(0, pmax(round_any(max(d$individual_income_BT), x, f = ceiling), round_any(max(d$individual_income_BT),x, f=ceiling))), 
                       breaks = seq(0, pmax(round_any(max(d$individual_income_BT), x, f=ceiling), round_any(max(d$individual_income_BT), x, f=ceiling)), x))+  
    ylab("$ per year")+ 
    xlab("Year") +
    theme_classic() +
    theme(plot.title = element_text(hjust = .5, size = 20), 
          axis.text.x = element_text(angle = 90), 
          axis.text = element_text(size = 17, colour = "black"), 
          axis.title.x = element_text(size = 16),
            panel.grid.major = element_line(colour = "gray79"),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"), 
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          axis.title.y = element_text(angle = 90, size=18, vjust = 0.525)) +
    labs(title="", 
         caption=c,
         color = "") +
    ggtitle("Individual Income Before Taxes")
  }else{
    gross.income <- ggplot(d, aes(x=Year, group = `Career Path`, shape = `Career Path`)) +
    geom_line(aes(y=`Income Before Taxes`, color = CareerPath), size=1.5) +
    geom_point(aes(y=`Income Before Taxes`, color = CareerPath), size=4) +
    scale_x_continuous(limits=c(min(d$Year), max(d$Year)), 
                       breaks = seq(min(d$Year), max(d$Year), 1)) + 
    scale_y_continuous(label = comma, limits=c(0, pmax(round_any(max(d$individual_income_BT), x, f = ceiling), round_any(max(d$individual_income_BT),x, f=ceiling))), 
                       breaks = seq(0, pmax(round_any(max(d$individual_income_BT), x, f=ceiling), round_any(max(d$individual_income_BT), x, f=ceiling)), x))+  
    ylab("$ per year")+ 
    xlab("Year") +
    theme(plot.title = element_text(hjust = .5, size = 20), 
          axis.text.x = element_text(angle = 90, vjust = .5), 
          axis.text = element_text(size = 17, colour = "black"), 
          axis.title.x = element_text(size = 16),
          panel.grid.major = element_line(colour = "gray79"),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"), 
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          legend.position=c(.7,.1),
          axis.title.y = element_text(angle = 90, size=18, vjust = 0.525)) +
    labs(title="", 
         caption=c,
         color = "") +
    ggtitle("Individual Income Before Taxes")
  }
  ########
  ## set fixed color for Self-Sufficiency line
  ########
  path_names <- unique(d$CareerPath)
  
  ##copy line_colors vector
  path_colors_vector <- line_colors2
  
  ## add names to vector
  names(path_colors_vector) <- path_names
  
  ## add Self-Sufficiency target to vector
  path_colors_vector <- c(path_colors_vector, "darkgrey")
  
  names(path_colors_vector)[length(path_colors_vector)] <-  "Minimum Household Budget"
  
  shapes <- shapes
  
  gross.income <- gross.income + scale_shape_manual(values = shapes)
  
  gross.income <- gross.income + scale_color_manual(values = path_colors_vector)+
                  annotate("rect", xmin = ts1, xmax = te1,ymin = -Inf , ymax = Inf,  alpha = .2, fill = line_colors3[1])+
                  annotate("rect", xmin = ts2, xmax = te2,ymin = -Inf , ymax = Inf, alpha = .2, fill = line_colors3[2]) +
                  annotate("rect", xmin = ts3, xmax = te3,ymin = -Inf , ymax = Inf, alpha = .2, fill = line_colors3[3])+
                  geom_hline(yintercept = 0, lty =2, size = 1.5)
  
    if (plottype=="ggplotly"){
  
      gross.income <- ggplotly(gross.income, tooltip = c("x", "y", "group")) %>% 
                      layout(legend = list(orientation = "h", x = 0, y = -0.4, title = list(text = ' '))) %>%  # GGPLOTLY doesn't know how to deal with legend location so have to do that
                      layout(annotations = list(x = 0.85, y = -0.4, text = c, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='center', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=15, color="black")))  %>% config(displayModeBar = F) 
    }
  
  return(print(gross.income))
  
} #end of gross.income.values


