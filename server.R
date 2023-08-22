#######################################################
#######################################################
# Server - NO CUSTOMIZATION IS REQUIRED
#######################################################
#######################################################

# To do:
# 1. Adjust input names (BLS occupations, county, ST etc) + renaming of occupations in the end
# 2. Cross-section data generation
# 3. Other functions: program loss, net taxes, net resources
# 4. Separate individual income vs family income
# 5. Set maxyear later

server <- function(input, output, session){
  
  observeEvent(input$getresults, {
     js$getresults();
    updateTabsetPanel(session, "tabs", selected = "Results")
  })  

  # UPDATE LIST OF COUNTIES
  toListenCounty <- reactive({list(input$state)})
  observeEvent(toListenCounty(),{
  state<-isolate(as.character(input$state))
  updateSelectizeInput(session, 'county_main',
                       choices =  loc_meta[stateAbbrev == state, countyortownName],
                       server = TRUE,
                       selected = character(0))
  })
 
  # Update list of occupation 1 based on chosen county
  toListenOccupation1 <- reactive({ # React if one of these inputs is updated 
    list(input$industry1, input$state, input$county_main)
  })
  
  observeEvent(toListenOccupation1(),{
    
    # Determine what MSA occupation belongs
    MSA<-table.msamap$MSA[table.msamap$stateAbbrev==input$state & table.msamap$countyortownName==input$county_main]
    
    # Filter on industry
    industry1<-isolate(as.character(input$industry1)) 
    if(is.null(industry1)){industry1<-unique(occ_meta$OCC_BROAD_IND)}
    
    # Update current occupation
    updateSelectizeInput(session, 'occupation1',
                         choices = occ_meta[AREA_NAME %in% MSA & OCC_BROAD_IND==industry1, OCC_TITLE],
                         server = TRUE,
                         selected = character(0))
    })
  
  # Update list of occupation 2
  toListenOccupation2 <- reactive({ # React if one of these inputs is updated 
    list(input$industry2, input$state, input$county_main)
  })
  
  observeEvent(toListenOccupation2(),{
    
    # Determine what MSA occupation belongs
    MSA<-table.msamap$MSA[table.msamap$stateAbbrev==input$state & table.msamap$countyortownName==input$county_main]
    
    # Filter on industry
    industry2<-isolate(as.character(input$industry2)) 
    if(is.null(industry2)){industry2<-unique(occ_meta$OCC_BROAD_IND)}
    
    # Update current occupation
    updateSelectizeInput(session, 'occupation2',
                         choices = occ_meta[AREA_NAME %in% MSA & OCC_BROAD_IND==industry2, OCC_TITLE],
                         server = TRUE,
                         selected = character(0))
    })
  

  toListen <- reactive({ # Recalculate if "Get Results" or "Calculate Budgets" are clicked
    list(input$tabs)
    
  })

#-------------------------------------------------------------------
#-------------------------------------------------------------------
# ###THIS SECTION LISTENS TO THE CLICK ON THE 'CALCLUATE BUTTON'
#-------------------------------------------------------------------   
#-------------------------------------------------------------------

  observeEvent(input$getresults, {
   # delay(3000, toListen())
   
    page <<- 1
    
    observeEvent(toListen(),{

    
    if(input$tabs == "Results" & page == 1){
      
     updateActionButton(session, "getresults", label = "Recalculate")
      
      # Read in user inputs
     # fam_disab <- isolate(as.character(input$fam_disab))
      prev_ssi <- isolate(as.character(input$prev_ssi))
      
      careercluster1<-isolate(as.character(input$industry1))
      industry1<-isolate(as.character(input$industry1))
      careercluster2<-isolate(as.character(input$industry2))
      industry2<-isolate(as.character(input$industry2))
      edu_req_1 <- isolate(as.character(input$edu_req_1))
      edu_req_2 <- isolate(as.character(input$edu_req_2))
      
      edu_req_1_ob <<- edu_req_1
      edu_req_2_ob <<- edu_req_2
      
      manual <- isolate(as.character(input$manual_selection_occupation))
      
      occupation_1<-isolate(as.character(input$occupation1))
      occupation_2<-isolate(as.character(input$occupation2))
      
      benefit1<-isolate(as.character(input$benefit1))
      
      numadults <- isolate(as.numeric(input$numadults))
      numkids <- isolate(as.numeric(input$numkids))
      state <- isolate(as.character(input$state))
      
      num_adults <<- numadults
      
      # Initialize catches for potential errors such as leaving fields blank or selecting incorrect ages for children >19
      error_kids <<- 0
      error_adults <<- 0
      error_kids_num <<- 0
      error_adults_num <<- 0
      
      if(numkids > 6 | numkids < 0 | is.na(numkids)){
        if(is.na(numkids)){
        error_kids <<- 1
        }else{
          error_kids_num <<- 1
        }
      }
      
      if(numadults > 6 |  is.na(numadults)){
        if(is.na(numadults)){
        error_adults <<- 1
        }else{
          error_adults_num <<- 1
        }
      }
      
      # To avoid crashing and have error message appear, need to assign something
      if(numkids>6 | is.na(numkids)){numkids<-6}
      if(numadults>6 | numadults==0 | is.na(numadults)){numadults<-6}
      
   
     # zyx <- 0
      # States with CCDF & TANF
    #  if(state!='FL' & state!='CT')
    #  {benefit2<-isolate(as.character(input$benefit2a))}
      #FL adds fates
    #  else if(state %in% c("FL"))
    #  {benefit2<-isolate(as.character(input$benefit2d))}
      #CT adds RAP
    #  else if(state %in% c("CT"))
    #  {benefit2<-isolate(as.character(input$benefit2e))}
    #  else{
        # benefit2<-isolate(as.character(input$benefit2c))
    #    zyx <- 1
    #  }
    
      # Read in benefits. Takes into account FL & CT specific programs
      if(state!="FL" & state!="CT" & state!="empty"){
        benefit2<-isolate(as.character(input$benefit2a))
      }
      
      #FL adds fates
      else if(state %in% c("FL"))
      {benefit2<-isolate(as.character(input$benefit2d))
      }
      #CT adds RAP
      else if(state %in% c("CT"))
      {benefit2<-isolate(as.character(input$benefit2e))
      }
      else if(state == 'empty'){
        benefit2<-' '
      }
      
      # CT - cannot choose Section8 & RAP
      s8_error <- 0
      if('Section 8 Housing Choice Voucher' %in% benefit2 & 'RAP' %in% benefit2){
        s8_error <- 1
      }
      
    
      city.name <- isolate(as.character(input$county_main))
      
      region <- as.character(paste(city.name, state, sep=", ", collapse = NULL))
      
      city.name <- region
      
    if(edu_req_1 != "yes_custom_1"){
      schooling_1 <- 0
      schooling_years_1 <- 0
      schooling_months_1 <- 0
    }else{
      schooling_years_1<-isolate(as.numeric(input$schooling_years_1))
      schooling_months_1<-isolate(as.numeric(input$schooling_months_1))
    }
    
    if(schooling_months_1 > 6 & !is.na(schooling_months_1) & !is.na(schooling_years_1)){
      schooling_1 <- schooling_years_1 + 1
    }else{
      schooling_1 <- schooling_years_1
    }
      
      if(manual == "manual_no" | (manual == "manual_yes" & edu_req_2 != "yes_custom_2")){
        schooling_2 <- 0
        schooling_years_2 <- 0
        schooling_months_2 <- 0
      }else{
        schooling_years_2<-isolate(as.numeric(input$schooling_years_2))
        schooling_months_2<-isolate(as.numeric(input$schooling_months_2))
      }
      
      if(schooling_months_2 > 6 & !is.na(schooling_months_2) & !is.na(schooling_years_2)){
        schooling_2 <- schooling_years_2 + 1
      }else{
        schooling_2 <- schooling_years_2
      }
    
    age_child_1<-isolate(as.numeric(input$age_child_1))
    age_child_2<-isolate(as.numeric(input$age_child_2))
    age_child_3<-isolate(as.numeric(input$age_child_3))
    age_child_4<-isolate(as.numeric(input$age_child_4))
    age_child_5<-isolate(as.numeric(input$age_child_5))
    age_child_6<-isolate(as.numeric(input$age_child_6))
    
    if(numadults==1){
      age_adult_1<-isolate(as.numeric(input$age_adult_1))
      age_adult_2<-NA_real_
      age_adult_3<-NA_real_
      age_adult_4<-NA_real_
      age_adult_5<-NA_real_
      age_adult_6<-NA_real_
    }else if(numadults==2){
      age_adult_1<-isolate(as.numeric(input$age_adult_1))
      age_adult_2<-isolate(as.numeric(input$age_adult_2))
      age_adult_3<-NA_real_
      age_adult_4<-NA_real_
      age_adult_5<-NA_real_
      age_adult_6<-NA_real_
    }else if(numadults==3){
      age_adult_1<-isolate(as.numeric(input$age_adult_1))
      age_adult_2<-isolate(as.numeric(input$age_adult_2))
      age_adult_3<-isolate(as.numeric(input$age_adult_3))
      age_adult_4<-NA_real_
      age_adult_5<-NA_real_
      age_adult_6<-NA_real_
    }else if(numadults==4){
      age_adult_1<-isolate(as.numeric(input$age_adult_1))
      age_adult_2<-isolate(as.numeric(input$age_adult_2))
      age_adult_3<-isolate(as.numeric(input$age_adult_3))
      age_adult_4<-isolate(as.numeric(input$age_adult_4))
      age_adult_5<-NA_real_
      age_adult_6<-NA_real_
    }else if(numadults==5){
      age_adult_1<-isolate(as.numeric(input$age_adult_1))
      age_adult_2<-isolate(as.numeric(input$age_adult_2))
      age_adult_3<-isolate(as.numeric(input$age_adult_3))
      age_adult_4<-isolate(as.numeric(input$age_adult_4))
      age_adult_5<-isolate(as.numeric(input$age_adult_5))
      age_adult_6<-NA_real_
    }else if(numadults==6){
    age_adult_1<-isolate(as.numeric(input$age_adult_1))
    age_adult_2<-isolate(as.numeric(input$age_adult_2))
    age_adult_3<-isolate(as.numeric(input$age_adult_3))
    age_adult_4<-isolate(as.numeric(input$age_adult_4))
    age_adult_5<-isolate(as.numeric(input$age_adult_5))
    age_adult_6<-isolate(as.numeric(input$age_adult_6))
    }
    
 
    
    if(manual == "manual_no"){
    career_path <- c("Near-Minimum Wage Job", occupation_1)
    }else{
      career_path <- c(occupation_2, occupation_1)
    }
 
    list_benefits <<- as.character(benefit2)
    public_package <- "the following public assistance package: "
    
    if(benefit1 == "All programs"){
      ben <- "the following public assistance package: Earned Income Tax Credit, Child Tax Credit, Child and Dependent Tax Credit, Food Subsidies, Health Insurance Subsidies, Childcare Subsidies (CCDF), Section 8 Housing Subsidy, and TANF"
    }else if(benefit1 == "No programs" | (benefit1 == "Select a custom list" & length(list_benefits)<1)){
      ben <- "no public assistance package."
    }else if(benefit1 == "Select a custom list" & length(list_benefits)>0){
      
      for(i in 1:length(list_benefits)){
        public_package <- paste0(public_package, list_benefits[i], ", ")
      }
      ben <<- gsub('.{2}$', '.', public_package)
        
    }

    abcde <<- 2
        
    # Initialize disability inputs
    disability1 <- 0
    disability2 <- 0
    disability3 <- 0
    disability4 <- 0
    disability5 <- 0
    disability6 <- 0
    disability7 <- 0
    disability8 <- 0
    disability9 <- 0
    disability10 <- 0
    disability11 <- 0
    disability12 <- 0
    disab.work.exp<-0
    ssdiPIA1<-0
    ssdiPIA2<-0
    ssdiPIA3<-0
    ssdiPIA4<-0
    ssdiPIA5<-0
    ssdiPIA6<-0
    blind1<-0
    blind2<-0
    blind3<-0
    blind4<-0
    blind5<-0
    blind6<-0
    fam_disab <- "No" # Only Alabama should have these uncommented - opted to not use disability programs
    prev_ssi <- "No" # Only Alabama should have these uncommented
     
     if(isolate(as.character(input$marital_status))=="Yes"){
       married<-1
     }else{
       married<-0
     }
    married <<- married
    
    if(fam_disab=="Yes"){
         
    disability1<-isolate(as.logical(input$disab1))
    if(disability1==TRUE){
      disability1<-1
    }else{
      disability1<-0
    }
    
    disability2<-isolate(as.logical(input$disab2))
    if(disability2==TRUE){
      disability2<-1
    }else{
      disability2<-0
    }
    
    disability3<-isolate(as.logical(input$disab3))
    if(disability3==TRUE){
      disability3<-1
    }else{
      disability3<-0
    }
    
    disability4<-isolate(as.logical(input$disab4))
    if(disability4==TRUE){
      disability4<-1
    }else{
      disability4<-0
    }
    
    disability5<-isolate(as.logical(input$disab5))
    if(disability5==TRUE){
      disability5<-1
    }else{
      disability5<-0
    }
    
    disability6<-isolate(as.logical(input$disab6))
    if(disability6==TRUE){
      disability6<-1
    }else{
      disability6<-0
    }
    
    disability7<-isolate(as.logical(input$disab7))
    if(disability7==TRUE){
      disability7<-1
    }else{
      disability7<-0
    }
    
    disability8<-isolate(as.logical(input$disab8))
    if(disability8==TRUE){
      disability8<-1
    }else{
      disability8<-0
    }
    
    disability9<-isolate(as.logical(input$disab9))
    if(disability9==TRUE){
      disability9<-1
    }else{
      disability9<-0
    }
    
    disability10<-isolate(as.logical(input$disab10))
    if(disability10==TRUE){
      disability10<-1
    }else{
      disability10<-0
    }
    
    disability11<-isolate(as.logical(input$disab11))
    if(disability11==TRUE){
      disability11<-1
    }else{
      disability11<-0
    }
    
    disability12<-isolate(as.logical(input$disab12))
    if(disability12==TRUE){
      disability12<-1
    }else{
      disability12<-0
    }
    
    }else{
      disability1 <- 0
      disability2 <- 0
      disability3 <- 0
      disability4 <- 0
      disability5 <- 0
      disability6 <- 0
      disability7 <- 0
      disability8 <- 0
      disability9 <- 0
      disability10 <- 0
      disability11 <- 0
      disability12 <- 0
    }
     

    disab.work.exp <- 0
    disab.work.exp.error <- 0
    
    if(prev_ssi=="Yes"){
      prev_ssi<-1}
    else
    {prev_ssi<-0}
    
     # if((benefit1 == "All programs" | (benefit1 == "Select a custom list" & 'Supplemental Security Income (SSI)' %in% benefit2)) & fam_disab=="Yes"){
    #  disab.work.exp<-isolate(as.numeric(input$disab.work.exp))
    #  if(is.na(disab.work.exp) | disab.work.exp < 0){
    #    disab.work.exp<-0
    #    disab.work.exp.error <- 1
    #  }
    #  }else{
    #    disab.work.exp <- 0
    #  }
  
    # Initialize other family incomes
    spouse_income<-0
    third_adult_income <- 0
    fourth_adult_income <- 0
    fifth_adult_income <- 0
    sixth_adult_income <- 0
    income.otherfamily <- 0
    
    # logically determine when individual income are accounted -  only when someone has a disability
 #   if(fam_disab == "Yes"){
#        if(numadults > 1 & !is.na(numadults)){
#          if(isolate(as.character(input$marital_status))=="Yes"){
#            spouse_income<-isolate(as.numeric(input$spouse_income))
#          }else{
#            spouse_income<-isolate(as.numeric(input$second_adult_income))
#          }
#        }else if(numadults <= 1 | is.na(numadults)){
#          spouse_income<-0
#        }
#        
#        if(numadults > 2 & !is.na(numadults)){
#          third_adult_income <- isolate(as.numeric(input$third_adult_income))
#        }else{
#          third_adult_income <- 0
#        }
#    
#    if(numadults > 3 & !is.na(numadults)){
#      fourth_adult_income <- isolate(as.numeric(input$fourth_adult_income))
#    }else{
#      fourth_adult_income <- 0
#    }
#    
#    if(numadults > 4 & !is.na(numadults)){
#      fifth_adult_income <- isolate(as.numeric(input$fifth_adult_income))
#    }else{
#      fifth_adult_income <- 0
#    }
#    
#    if(numadults > 5 & !is.na(numadults)){
#      sixth_adult_income <- isolate(as.numeric(input$sixth_adult_income))
#    }else{
#      sixth_adult_income <- 0
#    }
#    
#}
    
    # When no one has a disability, we ask for all other incomes in one questions
    if(numadults>=2 & !is.na(numadults)){
      income.otherfamily <- isolate(as.numeric(input$income_other_family))
    }else{
      income.otherfamily <- 0
    }
    
    ssdi_error_1 <- 0
    ssdi_error_2 <- 0
    ssdi_error_3 <- 0
    ssdi_error_4 <- 0
    ssdi_error_5 <- 0
    ssdi_error_6 <- 0
    
   # if((benefit1 == "All programs" | (benefit1 == "Select a custom list" & 'Social Security Disability Insurance (SSDI)' %in% benefit2)) & fam_disab=="Yes"){
      
  #    if(numadults>=1 & disability1==1){
  #      ssdiPIA1<-isolate(as.numeric(input$ssdiPIA1))
        
  #      if(is.na(ssdiPIA1) | ssdiPIA1<= 0){
  #        ssdi_error_1 <- 1
  #        ssdiPIA1<-0
  #      }else{
  #        ssdi_error_1 <- 0
  #      }
        
        
  #    }else{
  #      ssdiPIA1<-0
  #    }
      
  #    if(numadults>=2 & disability2==1){
  #      ssdiPIA2<-isolate(as.numeric(input$ssdiPIA2))
        
        
  #      if(is.na(ssdiPIA2) | ssdiPIA2<= 0){
  #        ssdi_error_2 <- 1
  #        ssdiPIA2<-0
  #      }else{
  #        ssdi_error_2 <- 0
  #      }
        
        
   #   }else{
  #      ssdiPIA2<-0
  #    }
      
  #    if(numadults>=3 & disability3==1){
  #      ssdiPIA3<-isolate(as.numeric(input$ssdiPIA3))
        
  #      if(is.na(ssdiPIA3) | ssdiPIA3<= 0){
  #        ssdi_error_3 <- 1
  #        ssdiPIA3<-0
  #      }else{
  #        ssdi_error_3 <- 0
  #      }
        
   #   }else{
  #      ssdiPIA3<-0
  #    }
      
  #    if(numadults>=4 & disability4==1){
  #      ssdiPIA4<-isolate(as.numeric(input$ssdiPIA4))
        
  #      if(is.na(ssdiPIA4) | ssdiPIA4<= 0){
  #        ssdi_error_4 <- 1
  #        ssdiPIA4<-0
  ##      }else{
  #        ssdi_error_4 <- 0
  #      }
        
  #    }else{
  #      ssdiPIA4<-0
  #    }
      
  #    if(numadults>=5 & disability5==1){
  #      ssdiPIA5<-isolate(as.numeric(input$ssdiPIA5))
        
  #      if(is.na(ssdiPIA5) | ssdiPIA5<= 0){
  #        ssdi_error_5 <- 1
  #        ssdiPIA5<-0
  #      }else{
  #        ssdi_error_5 <- 0
  #      }
        
  #    }else{
  #      ssdiPIA5<-0
  #    }
      
  #    if(numadults>=6 & disability6==1){
  #      ssdiPIA6<-isolate(as.numeric(input$ssdiPIA6))
        
  #      if(is.na(ssdiPIA6) | ssdiPIA6<= 0){
  #        ssdi_error_6 <- 1
  ##        ssdiPIA6<-0
  #      }else{
  #        ssdi_error_6 <- 0
  #      }
        
  #    }else{
  #      ssdiPIA6<-0
  #    }
      
      
  #    blind1<-isolate(as.logical(input$blind1))
  #    if(blind1==TRUE){
  #      blind1<-1
  #    }else{
  #      blind1<-0
  #    }
      
  #    blind2<-isolate(as.logical(input$blind2))
  #    if(blind2==TRUE){
  #      blind2<-1
  #    }else{
  #      blind2<-0
  #    }
      
  #    blind3<-isolate(as.logical(input$blind3))
  #    if(blind3==TRUE){
  ##      blind3<-1
  #    }else{
  #      blind3<-0
  #    }
      
  #    blind4<-isolate(as.logical(input$blind4))
  #    if(blind4==TRUE){
  #      blind4<-1
  #    }else{
  #      blind4<-0
  #    }
      
      
  #    blind5<-isolate(as.logical(input$blind5))
  #    if(blind5==TRUE){
  #      blind5<-1
  #    }else{
  #      blind5<-0
  #    }
      
  #    blind6<-isolate(as.logical(input$blind6))
  #    if(blind6==TRUE){
  #      blind6<-1
  #    }else{
  #      blind6<-0
  #    }
      
   # }else {
      ssdiPIA1<-0
      ssdiPIA2<-0
      ssdiPIA3<-0
      ssdiPIA4<-0
      ssdiPIA5<-0
      ssdiPIA6<-0
      blind1<-0
      blind2<-0
      blind3<-0
      blind4<-0
      blind5<-0
      blind6<-0
 #   }

    abcde <<- 3
    
    
    ###################
    # DISABILITY CATCHES
    ##################
    
    disab_error <- 0
  #  if(fam_disab == "Yes" & 
  #     disability1 == 0 & 
  #     disability2 == 0 & 
  #     disability3 == 0 & 
  #     disability4 == 0 & 
  #     disability5 == 0 & 
  #     disability6 == 0 & 
  #     disability7 == 0 & 
  ##     disability8 == 0 & 
  #     disability9 == 0 & 
  #     disability10 == 0 & 
  #     disability11 == 0 & 
  #     disability12 == 0){
  #    disab_error <- 1
  #  }
    
   
    
    
    ssdi_error <- 0
#    if(ssdi_error_1 == 1 | ssdi_error_2 == 1 | ssdi_error_3 == 1 | ssdi_error_4 == 1 | ssdi_error_5 ==1 | ssdi_error_6 == 1){
#      ssdi_error <- 1
#    }
    
    
    ssdi_no_adults <- 0
    
  #  if((benefit1 == "All programs" | (benefit1 == "Select a custom list" & 'Social Security Disability Insurance (SSDI)' %in% benefit2)) & fam_disab=="Yes"){
      
   #   if(disability1 == 0 & disability2 == 0 & disability3 == 0 & disability4 == 0 & disability5 == 0 & disability6 == 0){
  #      ssdi_no_adults <- 1
  #    }else{
  #      ssdi_no_adults <- 0
  #    }
      
  #  }
    
     
    ssi_ssdi_error <- 0
    
  #  if(fam_disab == "No" & benefit1 == "Select a custom list" & ('Supplemental Security Income (SSI)' %in% benefit2 | 'Social Security Disability Insurance (SSDI)' %in% benefit2)){
  #    ssi_ssdi_error <- 1
  #  }else{
  #    ssi_ssdi_error <- 0
  #  }
    
    fam_disab_error <- 0
  #  if(fam_disab=="empty"){
  #    fam_disab_error <- 1
  ##    fam_disab <- "No"
  #  }else{
  #    fam_disab_error <- 0
  #  }
    
    #-------------------------------------------
    # Step 1: Start generating data here
    #------------------------------------------
    
    careerpathIDs <- c(1,2)
     data <- NA
     csdata <- NA
     NetResources_table <- NA
     NetTax_table <- NA
  
    
    if((manual == "manual_no" &input$state != "empty" &
       input$county_main != "" &
     !is.na(input$numadults) &
     !is.na(input$numkids) &
       input$benefit1 != "empty" &
       input$industry1 != "empty" & 
       input$occupation1 != "" & 
       input$edu_req_1 != "empty" &
       !is.na(input$schooling_years_1) & 
       !is.na(input$schooling_months_1)& disab_error ==0 &
     ssdi_no_adults ==0 &
     ssdi_error==0 &
     ssi_ssdi_error == 0
     &fam_disab_error == 0 & 
     fam_disab!="empty"
     & disab.work.exp.error==0
     & s8_error == 0)
       |
       (manual == "manual_yes" &input$state != "empty" &
        input$county_main != "" &
        !is.na(input$numadults) &
        !is.na(input$numkids) &
        input$benefit1 != "empty" &
        input$industry1 != "empty" & 
        input$occupation1 != "" & 
        input$edu_req_1 != "empty" &
        !is.na(input$schooling_years_1) & 
        !is.na(input$schooling_months_1)&
        input$industry2 != "empty" & 
        input$occupation2 != "" & 
        input$edu_req_2 != "empty" &
        !is.na(input$schooling_years_2) & 
        !is.na(input$schooling_months_2)
        & disab_error ==0 &
        ssdi_no_adults ==0 &
        ssdi_error==0 &
        ssi_ssdi_error == 0
        &fam_disab_error == 0 & 
        fam_disab!="empty"
        & disab.work.exp.error==0
        & s8_error == 0
        )){
    
      abcde <<- 42
      
      ########################### DEBUG CHECK - MANUAL
      benefit1 <<- benefit1
      benefit2 <<- benefit2
      city.name <<- city.name
      career_path <<- career_path
      occupation_1 <<- occupation_1
      occupation_2 <<- occupation_2
      numadults <<- numadults
      numkids <<- numkids
      careerpathIDs <<- careerpathIDs
      age_adult_1 <<- age_adult_1
      age_adult_2 <<- age_adult_2
      age_adult_3 <<- age_adult_3
      age_adult_4 <<- age_adult_4
      age_adult_5 <<- age_adult_5
      age_adult_6 <<- age_adult_6
      age_child_1 <<- age_child_1
      age_child_2 <<- age_child_2
      age_child_3 <<- age_child_3
      age_child_4 <<- age_child_4
      age_child_5 <<- age_child_5
      age_child_6 <<- age_child_6
      schooling_1 <<- schooling_1
      schooling_2 <<- schooling_2
      edu_req_1 <<- edu_req_1
      edu_req_2 <<- edu_req_2
      manual <<- manual
      disability1 <<- disability1
      disability2 <<- disability2
      disability3 <<- disability3
      disability4 <<- disability4
      disability5 <<- disability5
      disability6 <<- disability6
      disability7 <<- disability7
      disability8 <<- disability8
      disability9 <<- disability9
      disability10 <<-  disability10
      disability11 <<-  disability11
      disability12 <<- disability12
      ssdiPIA1 <<- ssdiPIA1
      ssdiPIA2 <<- ssdiPIA2
      ssdiPIA3 <<- ssdiPIA3
      ssdiPIA4 <<- ssdiPIA4
      ssdiPIA5 <<- ssdiPIA5
      ssdiPIA6 <<- ssdiPIA6 
      blind1 <<- blind1
      blind2 <<- blind2
      blind3 <<- blind3
      blind4 <<- blind4
      blind5 <<- blind5
      blind6 <<- blind6
      disab.work.exp <<- disab.work.exp 
      aa1 <<- age_adult_1
      aa2 <<- age_adult_2
      aa3 <<- age_adult_3
      aa4 <<- age_adult_4
      aa5 <<- age_adult_5
      aa6 <<- age_adult_6
      ac1 <<- age_child_1
      ac2 <<- age_child_2
      ac3 <<- age_child_3
      ac4 <<- age_child_4
      ac5 <<- age_child_5
      ac6 <<- age_child_6 
      ###############################
      
      ####################
      # TIME SERIES DATA
      ####################  
    data <- time.series(benefit1, benefit2, city.name, career_path, occupation_1, occupation_2, numadults, numkids, careerpathIDs, age_adult_1, age_adult_2, age_adult_3, age_adult_4, age_adult_5, age_adult_6, age_child_1, age_child_2, age_child_3, age_child_4, age_child_5, age_child_6, spouse_income, third_adult_income, fourth_adult_income, fifth_adult_income, sixth_adult_income, income.otherfamily, schooling_1, schooling_2, edu_req_1, edu_req_2, manual, married, disability1, disability2, disability3, disability4, disability5, disability6, disability7, disability8, disability9, disability10, disability11, disability12, ssdiPIA1, ssdiPIA2, ssdiPIA3, ssdiPIA4, ssdiPIA5, ssdiPIA6, blind1, blind2, blind3, blind4, blind5, blind6, disab.work.exp, prev_ssi,fam_disab)
   
    abcde <<- 99
    
    data_check <<- data

    
    data$schooling_1 <- schooling_1
    data$schooling_2 <- schooling_2
    
    training_start_one <- 0
    training_end_one <- 0
    training_start_two <- 0 
    training_end_two <- 0
    training_start_three <- 0 
    training_end_three <- 0 
    
    data$NetTax <- data$total.taxes - data$total.transfers
    data$NetResourcesDisc<-data$NetResources*(1/(1+0.0645)^(data$Year-min(data$Year)))
    
    data$NetTaxDisc<-data$NetTax*(1/(1+0.0645)^(data$Year-min(data$Year)))
    
    NetTax_table<-data.frame(Total_Net_Taxes=character(0),Career_Path=character(0),Period=character(0), city=character(0))
    NetResources_table<-data.frame(Total_Net_Resources=character(0),Career_Path=character(0),Period=character(0), city=character(0))
    
    
   dataOG <- data
   
   if(manual == "manual_yes"){
     dataOG$CareerPath <- as.character(dataOG$CareerPath)
     dataOG$CareerPath[dataOG$careerpathID == 1] <- "Near-Minimum Wage Job"
   }
    
    data$CareerPath <- as.factor(data$CareerPath)
    dataOG$CareerPath <- as.factor(dataOG$CareerPath)

    cities <- as.character(unique(dataOG$countyortownName))
    careers <- as.character(unique(dataOG$CareerPath))
    
    careers <- careers[careers != "Near-Minimum Wage Job"]
    
      for(city in cities){
        for(career in careers){
          career_path_NR <- c("Near-Minimum Wage Job", career)
          
          tfa.3 <- dataOG[dataOG$Year <= min(dataOG$Year)+2 &  dataOG$countyortownName == city  & dataOG$CareerPath %in% career_path_NR, ]
          tfa.5 <- dataOG[dataOG$Year <= min(dataOG$Year)+4 &  dataOG$countyortownName == city  & dataOG$CareerPath %in% career_path_NR, ]
          tfa.10 <- dataOG[dataOG$Year <= min(dataOG$Year)+9 &  dataOG$countyortownName == city  & dataOG$CareerPath %in% career_path_NR, ]
          tfa.lt <- dataOG[dataOG$countyortownName == city  & dataOG$CareerPath %in% career_path_NR,]
          
          tfa.3.period <- tfa.3 %>% dplyr::group_by(CareerPath) %>%
            dplyr::summarise(NrPerDiscPeriod = sum(NetResources))
          
          tfa.3.period$Period <- "Year 1-3"
          
          tfa.5.period <- tfa.5 %>% dplyr::group_by(CareerPath) %>%
            dplyr::summarise(NrPerDiscPeriod = sum(NetResources))
          
          tfa.5.period$Period <- "Year 1-5"
          
          tfa.10.period <- tfa.10 %>% dplyr::group_by(CareerPath) %>%
            dplyr::summarise(NrPerDiscPeriod = sum(NetResources))
          
          tfa.10.period$Period <- "Year 1-10"
          
          tfa.lt.period <- tfa.lt %>% dplyr::group_by(CareerPath) %>%
            dplyr::summarise(NrPerDiscPeriod = sum(NetResources))
          
          tfa.lt.period$Period <- "Lifetime"
          
          tfa.times <- list(tfa.3.period, tfa.5.period, tfa.10.period, tfa.lt.period)
          
          ## apply function
          result.by.period <- lapply(tfa.times, getPeriodValue)
          
          ## append list into a dataframe
          
          result.data.frame <- do.call(rbind, result.by.period)
          
          ## rename columns
          names(result.data.frame ) <- c("Total_Net_Resources", "Career_Path", "Period")
          
          ## Remove base career path option
          
          
          result.data.frame <- cbind(result.data.frame, city)
          

          NetResources_table <- rbind(NetResources_table, result.data.frame)
          
          
        }
      }

    x <- c(tfa.3.period$NrPerDiscPeriod[1] - tfa.3.period$NrPerDiscPeriod[2], tfa.5.period$NrPerDiscPeriod[1] - tfa.5.period$NrPerDiscPeriod[2], 
           tfa.10.period$NrPerDiscPeriod[1] - tfa.10.period$NrPerDiscPeriod[2], tfa.lt.period$NrPerDiscPeriod[1] - tfa.lt.period$NrPerDiscPeriod[2])
    
    
    NetResources_table <- NetResources_table[NetResources_table$Career_Path != "Near-Minimum Wage Job",]
    
    if(is.na(sum(NetResources_table$Total_Net_Resources))){
      NetResources_table$Total_Net_Resources <- x
    }
    
    names(NetResources_table)[names(NetResources_table) == "Career_Path"] <- "CareerPath"
    names(NetResources_table)[names(NetResources_table) == "city"] <- "countyortownName"

    cities <- as.character(unique(dataOG$countyortownName))
    careers <- as.character(unique(dataOG$CareerPath))
    
    careers <- careers[careers != "Near-Minimum Wage Job"]
    
      for(city in cities){
        for(career in careers){
          
          career_path_NT <- c("Near-Minimum Wage Job", career)

          tfa.tax.3 <- dataOG[dataOG$Year <= min(dataOG$Year)+2 & dataOG$countyortownName == city  & dataOG$CareerPath %in% career_path_NT, ]
          tfa.tax.5 <- dataOG[dataOG$Year <= min(dataOG$Year)+4 & dataOG$countyortownName == city  & dataOG$CareerPath %in% career_path_NT, ]
          tfa.tax.10 <- dataOG[dataOG$Year <= min(dataOG$Year)+9  & dataOG$countyortownName == city  & dataOG$CareerPath %in% career_path_NT, ]
          tfa.tax.lt <- dataOG[dataOG$countyortownName == city  & dataOG$CareerPath %in% career_path_NT,]
          ## Reshape data by summing net resources per time period
          
          tfa.tax.3.period <- tfa.tax.3 %>% dplyr::group_by(CareerPath) %>%
            dplyr::summarise(NtPerDiscPeriod = sum(NetTax))
          
          tfa.tax.3.period$Period <- "Year 1-3"
          
          tfa.tax.5.period <- tfa.tax.5 %>% dplyr::group_by(CareerPath) %>%
            dplyr::summarise(NtPerDiscPeriod = sum(NetTax))
          
          tfa.tax.5.period$Period <- "Year 1-5"
          
          tfa.tax.10.period <- tfa.tax.10 %>% dplyr::group_by(CareerPath) %>%
            dplyr::summarise(NtPerDiscPeriod = sum(NetTax))
          
          tfa.tax.10.period$Period <- "Year 1-10"
          
          tfa.tax.lt.period <- tfa.tax.lt %>% dplyr::group_by(CareerPath) %>%
            dplyr::summarise(NtPerDiscPeriod = sum(NetTax))
          
          tfa.tax.lt.period$Period <- "Lifetime"
          
          ## Convert dataframes into a list 
          
          tfa.tax.times <- list(tfa.tax.3.period, tfa.tax.5.period, tfa.tax.10.period, tfa.tax.lt.period)
          
          ########################################
          ###Apply function to each list item; build dataframe for plotting
          ########################################
          
          ## apply function
          result.by.period.tax <- lapply(tfa.tax.times, getPeriodValueTax)
          
          ## append list into a dataframe
          
          result.tax.data.frame <- do.call(rbind, result.by.period.tax)
          
          ## rename columns 
          names(result.tax.data.frame) <- c("Total_Net_Taxes", "Career_Path", "Period") 
          
          result.tax.data.frame <- cbind(result.tax.data.frame, city)
          
          result.tax.data.frame <- result.tax.data.frame[result.tax.data.frame$Career_Path != "Near-Minimum Wage Job", ]
          
          NetTax_table <- rbind(NetTax_table, result.tax.data.frame)
          
        }
      }

    NetTax_table <- NetTax_table[NetTax_table$Career_Path != "Near-Minimum Wage Job",]
    
    y <- c(tfa.tax.3.period$NtPerDiscPeriod[1] - tfa.tax.3.period$NtPerDiscPeriod[2], tfa.tax.5.period$NtPerDiscPeriod[1] - tfa.tax.5.period$NtPerDiscPeriod[2], 
           tfa.tax.10.period$NtPerDiscPeriod[1] - tfa.tax.10.period$NtPerDiscPeriod[2], tfa.tax.lt.period$NtPerDiscPeriod[1] - tfa.tax.lt.period$NtPerDiscPeriod[2])
    
    if(is.na(sum(NetTax_table$Total_Net_Taxes))){
      NetTax_table$Total_Net_Taxes <- y
    }
    
    names(NetTax_table)[names(NetTax_table) == "Career_Path"] <- "CareerPath"
    names(NetTax_table)[names(NetTax_table) == "city"] <- "countyortownName"
    
    rm(dataOG)
    
    error_nr_nt <- 0
    
    if(sum(data$missing) > 0 | sum(data$negative) > 0){
      error_nr_nt <- 1
    }
    
    NetResources_table$Period <- factor(NetResources_table$Period, levels=c("Year 1-3", "Year 1-5", "Year 1-10", "Lifetime"))
    NetTax_table$Period <- factor(NetTax_table$Period, levels=c("Year 1-3", "Year 1-5", "Year 1-10", "Lifetime"))
    
    max_year <- min(data$Year)+24
    data <- data[data$Year <= max_year,]
    
    ####################
    # CROSS SECTION DATA
    ####################
    
  abcde <<- 30
    
    csdata <- cross.section(benefit1, benefit2, city.name, career_path, occupation_1, numadults, numkids, careerpathIDs, age_adult_1, age_adult_2, age_adult_3, age_adult_4, age_adult_5, age_adult_6, age_child_1, age_child_2, age_child_3, age_child_4, age_child_5, age_child_6, married, disability1, disability2, disability3, disability4, disability5, disability6, disability7, disability8, disability9, disability10, disability11, disability12, ssdiPIA1, ssdiPIA2, ssdiPIA3, ssdiPIA4, ssdiPIA5, ssdiPIA6, blind1, blind2, blind3, blind4, blind5, blind6, disab.work.exp, prev_ssi)

    abcde <<- 50
    
    NetTax_table <- NetTax_table[NetTax_table$CareerPath != "Near-Minimum Wage Job",]
    NetResources_table <- NetResources_table[NetResources_table$CareerPath != "Near-Minimum Wage Job",]
    
    csdata <- csdata[csdata$careerpathID == 2,]
    
    }else{
      data <- NA
      csdata <- NA
      NetResources_table <- NA
      NetTax_table <- NA
      
    }
     
     abcde <<- 51
  ######################################################################
  # Plot Net Resources Linechart
  ######################################################################
  output$net.res <- renderPlotly({
   # "Please complete all fields and hit the Calculate Results button to continue."
    
    
    validate(
      need(disab_error ==0, "                ")
      
      
      
    )
    
    
    validate(
      need(
        ssi_ssdi_error==0, "               "
      )
    )
    
    validate( 
      need(ssdi_no_adults ==0, "      ")
    )
    
    
    validate(
      need(ssdi_error==0, "     ")
    )
    
    validate(
      need(fam_disab_error == 0, "     ")
    )
    
    validate(
      need(fam_disab!="empty", "     ")
    )
    
    validate(
      need(disab.work.exp.error==0, "    ")
    )
    
    validate(
      need(
        s8_error == 0, "    "
      )
    )
    
    
    net.res.values(data, city.name, training_start_one, training_end_one, training_start_two, training_end_two, training_start_three, training_end_three, benefit1, benefit2,  schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype="ggplotly")
    
  })
    
    ######################################################################
    # Plot Net Resources Linechart - w/ and w/o FATES
    ######################################################################
    # output$net.res.fates <- renderPlotly({
    #   
    #   net.res.values.fates(data, city.name, training_start_one, training_end_one, training_start_two, training_end_two, training_start_three, training_end_three, benefit1, benefit2,  schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual)
    #   
    # })

     abcde <<- 52
  ######################################################################
  # Plot Net Resources Barchart
  ######################################################################      
  output$net.res.life <- renderPlotly({
   
    validate(
      need(disab_error ==0, "If you have selected that at least one member of your family is disabled, make sure to correctly select that family member if disabled.")
    )
    
    validate(
      need(ssdi_no_adults ==0, "You have selected SSDI, but none of the adults have a disability.")
    )
    
    validate(
      need(
        ssi_ssdi_error==0, "If you are receiving SSI or SSDI you must select 'Yes' to the Question 'Does anyone have a disability?'"
      )
    )
    
    
    validate(
      need(ssdi_error==0, "SSDI values need to be positive.")
    )
    
    
    validate(
      need(fam_disab_error == 0, "Please select 'Yes' or 'No' in regards to whether or not someone in your family has a disability.")
    )
    
    validate(
      need(disab.work.exp.error==0, "Please input a non-negative value for amount spent per month on specialized equipment or services that enable household member(s) with disabilities to work")
    )
    
    validate(
      need(error_nr_nt==0," ")
    )
    
    validate(
      need(
        s8_error == 0, "Both Section 8 & RAP have been selected. Please unselect one of these programs."
      )
    )
    
    
    net.res.life.values(NetResources_table, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, city.name, plottype="ggplotly")
    
  })
    
     abcde <<- 53
  ######################################################################
  # Plot After Tax Income
  ######################################################################      
    output$after.tax.self.sufficiency <- renderPlotly({
   
      
      validate(
        need(disab_error ==0, "                ")
        
        
        
      )
      
      
      validate(
        need(
          ssi_ssdi_error==0, "               "
        )
      )
      
      validate( 
        need(ssdi_no_adults ==0, "      ")
      )
      
      
      validate(
        need(ssdi_error==0, "     ")
      )
      
      validate(
        need(fam_disab_error == 0, "    ")
      )
      
      validate(
        need(disab.work.exp.error==0, "             ")
      )
      
      
      validate(
        need(
          s8_error == 0, "    "
        )
      )
      
      
    after.tax.self.sufficiency.values(data, city.name, training_start_one, training_end_one, training_start_two, training_end_two, training_start_three, training_end_three, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype="ggplotly")
      
    })
    
     abcde <<- 54
    output$gross.income <- renderPlotly({
      
      
      validate(
        need(disab_error ==0, "If you have selected that at least one member of your family is disabled, make sure to correctly select that family member if disabled.")
      )
      
      validate(
        need(ssdi_no_adults ==0, "You have selected SSDI, but none of the adults have a disability.")
      )
      
      validate(
        need(
          ssi_ssdi_error==0, "If you are receiving SSI or SSDI you must select 'Yes' to the Question 'Does anyone have a disability?'"
        )
      )
      
      
      validate(
        need(ssdi_error==0, "SSDI values need to be positive.")
      )
      
      validate(
        need(fam_disab_error == 0, "Please select 'Yes' or 'No' in regards to whether or not someone in your family has a disability.")
      )
      
      validate(
        need(disab.work.exp.error==0, "Please input a non-negative value for amount spent per month on specialized equipment or services that enable household member(s) with disabilities to work")
      )
      
      
      validate(
        need(
          s8_error == 0, "Both Section 8 and RAP have been selected. Please unselect one of these programs."
        )
      )
      
       gross.income.values(data, city.name, training_start_one, training_end_one, training_start_two, training_end_two, training_start_three, training_end_three, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype="ggplotly")
      
    }
    )
    
  

    abcde <<- 55
  
  ######################################################################
  # Plot Total Taxes
  ######################################################################      
  output$taxes.total <- renderPlotly({
    
    validate(
      need(disab_error ==0, "If you have selected that at least one member of your family is disabled, make sure to correctly select that family member if disabled.")
    )
    
    validate(
      need(ssdi_no_adults ==0, "You have selected SSDI, but none of the adults have a disability.")
    )
    
    validate(
      need(
        ssi_ssdi_error==0, "If you are receiving SSI or SSDI you must select 'Yes' to the Question 'Does anyone have a disability?'"
      )
    )
    
    
    validate(
      need(ssdi_error==0, "SSDI values need to be positive.")
    )
    
    validate(
      need(fam_disab_error == 0, "Please select 'Yes' or 'No' in regards to whether or not someone in your family has a disability.")
    )
    
    validate(
      need(disab.work.exp.error==0, "Please input a non-negative value for amount spent per month on specialized equipment or services that enable household member(s) with disabilities to work")
    )
    
    
    validate(
      need(
        s8_error == 0, "Both Section 8 and RAP have been selected. Please unselect one of these programs."
      )
    )
    
    total.tax.values(data, city.name, training_start_one, training_end_one, training_start_two, training_end_two, training_start_three, training_end_three, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype="ggplotly")
    
  })
  
    abcde <<- 56
  ######################################################################
  # Plot Taxpayers Gains
  ######################################################################
  output$net.tax.life <- renderPlotly({
    
    
    validate(
      need(disab_error ==0, "                ")
      
      
      
    )
    
    
    validate(
      need(
        ssi_ssdi_error==0, "               "
      )
    )
    
    validate( 
      need(ssdi_no_adults ==0, "      ")
    )
    
    
    validate(
      need(ssdi_error==0, "     ")
    )
    
    validate(
      need(fam_disab_error == 0, "     ")
    )
    
    validate(
      need(disab.work.exp.error==0, "             ")
    )
    
    validate(
      need(error_nr_nt==0," ")
      )
    
    validate(
      need(
        s8_error == 0, " "
      )
    )
    
    net.tax.life.values(NetTax_table, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, city.name, occupation_1, occupation_2, plottype="ggplotly")
    
  })
  
    abcde <<- 57
  ######################################################################
  # Plot Transfers: Breakdown
  ###################################################################### 
  
  output$transfer.breakdown <- renderPlotly({
    
    
    validate(
      need(disab_error ==0, "If you have selected that at least one member of your family is disabled, make sure to correctly select that family member if disabled.")
    )
    
    validate(
      need(ssdi_no_adults ==0, "You have selected SSDI, but none of the adults have a disability.")
    )
    
    validate(
      need(
        ssi_ssdi_error==0, "If you are receiving SSI or SSDI you must select 'Yes' to the Question 'Does anyone have a disability?'"
      )
    )
    
    
    validate(
      need(ssdi_error==0, "SSDI values need to be positive.")
    )
    
    validate(
      need(fam_disab_error == 0, "Please select 'Yes' or 'No' in regards to whether or not someone in your family has a disability.")
    )
   
    validate(
      need(disab.work.exp.error==0, "Please input a non-negative value for amount spent per month on specialized equipment or services that enable household member(s) with disabilities to work")
    )
    
    validate(
      need(
        s8_error == 0, "Both Section 8 and RAP have been selected. Please unselect one of these programs."
      )
    )
    
        transfers.breakdown.values(data, city.name, benefit1, benefit2, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype="ggplotly")
    
  })
  
    abcde <<- 58
    ######################################################################
    # Plot Transfers over income: Breakdown
    ######################################################################     
  
  output$csbenefit.breakdown <- renderPlotly({
   
    
    validate(
      need(disab_error ==0, "If you have selected that at least one member of your family is disabled, make sure to correctly select that family member if disabled.")
    )
    
    validate(
      need(ssdi_no_adults ==0, "You have selected SSDI, but none of the adults have a disability.")
    )
    
    validate(
      need(
        ssi_ssdi_error==0, "If you are receiving SSI or SSDI you must select 'Yes' to the Question 'Does anyone have a disability?'"
      )
    )
    
    
    validate(
      need(ssdi_error==0, "SSDI values need to be positive.")
    )
    
    validate(
      need(fam_disab_error == 0, "Please select 'Yes' or 'No' in regards to whether or not someone in your family has a disability.")
    )
    
    validate(
      need(disab.work.exp.error==0, "Please input a non-negative value for amount spent per month on specialized equipment or services that enable household member(s) with disabilities to work")
    )
    
    validate(
      need(
        s8_error == 0, "Both Section 8 and RAP have been selected. Please unselect one of these programs."
      )
    )
    
      
     csbenefit.breakdown.values(csdata, benefit1, benefit2, city.name, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, plottype="ggplotly")
      
    })
    
 
    abcde <<- 59
  ######################################################################
  # Plot Total Expenses: Breakdown
  ######################################################################  
  output$expenses.breakdown <- renderPlotly({
    
    validate(
      need(disab_error ==0, "If you have selected that at least one member of your family is disabled, make sure to correctly select that family member if disabled.")
    )
    
    validate(
      need(ssdi_no_adults ==0, "You have selected SSDI, but none of the adults have a disability.")
    )
    
    validate(
      need(
        ssi_ssdi_error==0, "If you are receiving SSI or SSDI you must select 'Yes' to the Question 'Does anyone have a disability?'"
      )
    )
    
    validate(
      need(
        s8_error == 0, "Both Section 8 and RAP have been selected. Please unselect one of these programs."
      )
    )
    
    
    validate(
      need(ssdi_error==0, "SSDI values need to be positive.")
    )
    
    validate(
      need(fam_disab_error == 0, "Please select 'Yes' or 'No' in regards to whether or not someone in your family has a disability.")
    )
    
    validate(
      need(disab.work.exp.error==0, "Please input a non-negative value for amount spent per month on specialized equipment or services that enable household member(s) with disabilities to work")
    )
    
    
    expenses.breakdown.values(data,city.name,schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, plottype="ggplotly")
    
    
  })
    abcde <<- 599
    if((manual == "manual_no" &input$state != "empty" &
        input$county_main != "" &
        #  input$family != "empty" &
        !is.na(input$numadults) &
        !is.na(input$numkids) &
        input$benefit1 != "empty" &
        input$industry1 != "empty" & 
        input$occupation1 != "" & 
        input$edu_req_1 != "empty" &
        !is.na(input$schooling_years_1) & 
        !is.na(input$schooling_months_1)& disab_error ==0 &
        ssdi_no_adults ==0 &
        ssdi_error==0 &
        ssi_ssdi_error == 0
        &fam_disab_error == 0 & 
        fam_disab!="empty"
        & disab.work.exp.error==0
        & s8_error == 0)
       |
       (manual == "manual_yes" &input$state != "empty" &
        input$county_main != "" &
        !is.na(input$numadults) &
        !is.na(input$numkids) &
        input$benefit1 != "empty" &
        input$industry1 != "empty" & 
        input$occupation1 != "" & 
        input$edu_req_1 != "empty" &
        !is.na(input$schooling_years_1) & 
        !is.na(input$schooling_months_1)&
        input$industry2 != "empty" & 
        input$occupation2 != "" & 
        input$edu_req_2 != "empty" &
        !is.na(input$schooling_years_2) & 
        !is.na(input$schooling_months_2)
        & disab_error ==0 &
        ssdi_no_adults ==0 &
        ssdi_error==0 &
        ssi_ssdi_error == 0
        &fam_disab_error == 0 & 
        fam_disab!="empty"
        & disab.work.exp.error==0
        & s8_error == 0
       )){
  
  data_TS <<- data_return(data)
  data_CS <<- data_return(csdata)
  data_NR <<- data_return(NetResources_table)
  data_NT <<- data_return(NetTax_table)
  
  benefit1 <<- benefit1
  benefit2 <<- benefit2
  
  data_OCC <<- data[data$careerpathID == 2 & data$hours == 20,]
  data_BASE <<- data[data$careerpathID == 1 & data$hours == 20,]
  

  sX <<- as.character(nrow(data_OCC))
  
  sY <<- as.character(nrow(data_BASE))
  

  data$benefit1 <- as.character(benefit1)
  csdata$benefit1 <- as.character(benefit1)

  
  data_1<-data.table(data %>% 
                       select(Year, countyortownName, stateAbbrev, benefit1,  CareerPath, careerpathID, #FamilyType, 
                              agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6, agePerson7, agePerson8, agePerson9, 
                              agePerson10, agePerson11, agePerson12, famsize, numadults, 
                              numkids, numkidsunder13, FilingStatus, hasdependent, stateName, 
                              stateFIPS, MSA, occ_title, hours, experience, empl_healthcare, healthcare.source, income, AfterTaxIncome,# ATI,
                              income.aftertax.noTC, SelfSufficiency, income.investment, totalassets, value.CCDF, value.FATES, value.HeadStart, value.earlyHeadStart, value.PreK, value.section8,
                              value.liheap, value.snap, value.schoolmeals, value.medicaid.child, value.medicaid.adult, value.medicaid, value.wic, value.aca, 
                              value.cdctc, value.ctc, value.eitc,value.ssdi,value.ssi, total.transfers, NetResources, NetResourcesDisc,
                              famsize.foremployerhealthcare, value.employerhealthcare, exp.childcare, exp.transportation, exp.food, exp.misc, exp.tech,
                              exp.utilities, exp.rentormortgage, exp.healthcare, exp.healthcare.SS, exp.schoolMeals, 
                              exp.healthcare.employer, exp.healthcare.healthexchange, premium.outofpocket, netexp.childcare, 
                              netexp.rentormortgage, netexp.housing, netexp.utilities, netexp.food, netexp.healthcare, total.expenses, total.expenses.SS,
                              tax.income.fed, tax.FICA, tax.federal, tax.income.state, total.taxes, NetTax
                       ))
  
  csdata_1<-data.table(csdata %>% 
                         select(Year, countyortownName, stateAbbrev, benefit1, CareerPath, careerpathID, #FamilyType, 
                                agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6, agePerson7, agePerson8, agePerson9, 
                                agePerson10, agePerson11, agePerson12,famsize, numadults, 
                                numkids, numkidsunder13, FilingStatus, hasdependent, stateName, 
                                stateFIPS, MSA, empl_healthcare, healthcare.source, income, AfterTaxIncome, 
                                income.aftertax.noTC, SelfSufficiency, income.investment, totalassets,value.CCDF, value.FATES, value.HeadStart,value.earlyHeadStart, value.PreK, value.section8,
                                value.liheap, value.snap, value.schoolmeals, value.medicaid.child, value.medicaid.adult, value.medicaid, value.wic, value.aca, 
                                value.cdctc, value.ctc, value.eitc,value.ssdi,value.ssi, total.transfers, NetResources,
                                famsize.foremployerhealthcare, value.employerhealthcare, exp.childcare, exp.transportation, exp.food, exp.misc, exp.tech,
                                exp.utilities, exp.rentormortgage, exp.healthcare, exp.healthcare.SS, exp.schoolMeals, 
                                exp.healthcare.employer, exp.healthcare.healthexchange, premium.outofpocket, netexp.childcare, 
                                netexp.rentormortgage, netexp.housing, netexp.utilities, netexp.food, netexp.healthcare, total.expenses, total.expenses.SS,
                                tax.income.fed, tax.FICA, tax.federal, tax.income.state, total.taxes 
                         ))
  
  xyzyx <- 0
    
  dashboard <- "DataUnderlying"
  if(!exists("data_1") & !exists("csdata_1")){
    xyzyx <- 1
  }else{
    
    output$print2_1 <- downloadHandler(
      
      filename = function(){
        paste0(dashboard, "_CLIFFDashboard.xlsx")
      },
      
      content = function(file){
        # save(data, file=file)
        # write.xlsx(data, file=file)
        require(openxlsx)
        list_datasets <- list("Annual Results" = data_1, "Cross Section Results" = csdata_1, "Data Dictionary" = Data_Dictionary)
        write.xlsx(list_datasets, file=file)
        
      }
      
    )
    
    
    output$print2_2 <- downloadHandler(
      
      filename = function(){
        paste0(dashboard, "_Results.xlsx")
      },
      
      content = function(file){
        # save(data, file=file)
        # write.xlsx(data, file=file)
        require(openxlsx)
        list_datasets <- list("Annual Results" = data_1, "Cross Section Results" = csdata_1, "Data Dictionary" = Data_Dictionary)
        write.xlsx(list_datasets, file=file)
        
      }
      
    )
    
   }
  
 careerCT <- as.character(unique(data$CareerPath[data$careerpathID == 2 & data$occ_title != "cashiers"]))
 careerCT_2 <- as.character(unique(data$CareerPath[data$careerpathID == 1]))

  nn <- 7
  nnn <- 2
  
  #rv <- reactiveValues(download_flag = 0)
  
  output$print_1 <- downloadHandler(
    filename = "CLIFF_Dashboard_Results_1.docx",
    content = function(file){
      withProgress(message = "Downloading...",
                   value=0,
                   {
                     
                     Income_Before_Taxes <- gross.income.values(data, city.name, training_start_one, training_end_one, training_start_two, training_end_two, training_start_three, training_end_three, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype="ggplot")
                     incProgress(1/nn)
                     
                     
                     Annual_Take_Home_Pay_Self_Sufficiency <- after.tax.self.sufficiency.values(data, city.name, training_start_one, training_end_one, training_start_two, training_end_two, training_start_three, training_end_three,schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype="ggplot")
                     incProgress(1/nn)
                     
                     
                     Expenses_Breakdown <- expenses.breakdown.values(data,city.name,schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, plottype="ggplot")
                     incProgress(1/nn)
                     
                     Transfers_Breakdown <-  transfers.breakdown.values(data[data$CareerPath != "Near-Minimum Wage Job",], city.name, benefit1, benefit2, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype="ggplot")
                     incProgress(1/nn)
                     
                     Net_Res <-  net.res.values(data, city.name, training_start_one, training_end_one, training_start_two, training_end_two, training_start_three, training_end_three, benefit1, benefit2, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype="ggplot")
                     incProgress(1/nn)
                     
                     Net_Res_Life <-  net.res.life.values(NetResources_table[NetResources_table$CareerPath != "Near-Minimum Wage Job",], schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, city.name, plottype="ggplot")
                     incProgress(1/nn)
                     
            
                      CS_Benefits_Bar <-  csbenefit.breakdown.values(csdata[csdata$CareerPath != "Near-Minimum Wage Job",], benefit1, benefit2, city.name, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, plottype="ggplot")

                    incProgress(1/nn)
                     
                    if (USEALICE==FALSE){
                      src <- normalizePath('report.Rmd')
                      owd <- setwd(tempdir())
                      on.exit(setwd(owd))
                      file.copy(src, 'report.Rmd', overwrite=TRUE)
                      
                      incProgress(1/nn)
                      library(rmarkdown)
                      out <- rmarkdown::render('report.Rmd', "word_document")
                    }
                    
                    if (USEALICE==TRUE){
                      src <- normalizePath('report_alice.Rmd')
                      owd <- setwd(tempdir())
                      on.exit(setwd(owd))
                      file.copy(src, 'report_alice.Rmd', overwrite=TRUE)
                      
                      incProgress(1/nn)
                      library(rmarkdown)
                      out <- rmarkdown::render('report_alice.Rmd', "word_document")
                    }
                     
                     file.rename(out, file)
                   }
      )
     
    }
  )
  
  output$print_2 <- downloadHandler(
    filename = "CLIFFDashboard_Report_ForPolicymakers.docx",
    content = function(file){
      withProgress(message = "Downloading...",
                   value=0,
                   {
                     
                        
                      Total_Tax_Paid <- total.tax.values(data, city.name, training_start_one, training_end_one, training_start_two, training_end_two, training_start_three, training_end_three, schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, occupation_1, occupation_2, plottype="ggplot")
                      incProgress(1/nnn)
                     
                      Tax_Life <-   net.tax.life.values(NetTax_table[NetTax_table$CareerPath != "Near-Minimum Wage Job",], schooling_1, schooling_years_1, schooling_months_1, schooling_2, schooling_years_2, schooling_months_2, manual, city.name, occupation_1, occupation_2, plottype="ggplot")
                      incProgress(1/nnn)
                       
                     src <- normalizePath('report2.rmd')
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     file.copy(src, 'report2.rmd', overwrite=TRUE)
                     
                     incProgress(1/nnn)
                     library(rmarkdown)
                     out <- rmarkdown::render('report2.rmd', "word_document")
                     
                     
                     file.rename(out, file)
                   }
      )
      
    }
  )
  page <<- 0
    }
    }
  }
)
}
)


}