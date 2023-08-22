#######################################################
#######################################################
# User Interface - Requires Certain Customization
#######################################################
#######################################################

# Define UI ----
ui <- fluidPage(title = "CLIFF",#theme = "www/custom.css", # change theme of the App here
                theme = "style.css", # Load style
                tags$head(includeHTML(("www/google-analytics.html"))),
                tags$html(lang="en"),
                tags$style(".span12 {background-color: black;}"),
                tags$head(tags$style(HTML(".shiny-output-error-validation{color: red; font-size:24px; }"))),
                tags$head(tags$style(".shiny-notification{color: red; font-size:32px; position: fixed; top: 87% ;left: 2%  }")),
                     div(class = "header", includeHTML("www/include_header.html")), 
                    tags$head(
                  tags$style(
                    type ="text/css", "#years1 label{ display: table-cell; text-align: left; vertical-align: middle;}
                    #years1 .form-group { display: table-row;}
                    "
                  )
                ),
                tags$head(
                  tags$style(
                    type ="text/css", "#years2 label{ display: table-cell; text-align: left; vertical-align: middle;}
                    #years2 .form-group { display: table-row;}"
                  )
                ),
                tags$head(
                  tags$style(
                    type ="text/css", "#months1 label{ display: table-cell; text-align: left; vertical-align: middle; }
                    #months1 .form-group { display: table-row;}"
                  )
                ),
                tags$head(
                  tags$style(
                    type ="text/css", "#months2 label{ display: table-cell; text-align: left; vertical-align: middle; }
                    #months2 .form-group { display: table-row;}"
                  )
                ),
                  
                         
           
                    
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      selectInput("state", "State of Residency", # Note: collect state abbreviation
                  list(  
                         "Alabama" = "AL"
                        # , "Alaska" = "AK"
                        #  , "Arizona" = "AZ"
                        #  , "Arkansas" = "AR"
                        #  , "California" = "CA"
                        #  , "Colorado" = "CO"
                        #  , "Connecticut" = "CT"
                        #  , "Delaware" = "DE"
                        #  , "District of Columbia" = "DC"
                        #  , "Florida" = "FL"
                        #  , "Georgia" = "GA"
                        #  , "Hawaii" = "HI"
                        #  , "Idaho" = "ID"
                        #  , "Illinois" = "IL"
                        #  , "Indiana" = "IN"
                        #  , "Iowa" = "IA"
                        #  , "Kansas" = "KS"
                        #  , "Kentucky" = "KY"
                        #  , "Louisiana" = "LA"
                        #  , "Maine" = "ME"
                        #  , "Maryland" = "MD"
                        #  , "Massachusetts" = "MA"
                        #  , "Michigan" = "MI"
                        #  , "Minnesota" = "MN"
                        #  , "Mississippi" = "MS"
                        #  , "Missouri" = "MO"
                        #  , "Montana" = "MT"
                        #  , "Nebraska" = "NE"
                        #  , "Nevada" = "NV"
                        #  , "New Hampshire" = "NH"
                        #  , "New Jersey" = "NJ"
                        #  , "New Mexico" = "NM"
                        #  , "New York" = "NY"
                        #  , "North Carolina" = "NC"
                        # , "North Dakota" = "ND"
                        # , "Ohio" = "OH"
                        #  , "Oklahoma" = "OK"
                        #  , "Oregon" = "OR"
                        #  , "Pennsylvania" = "PA"
                        #  , "Rhode Island" = "RI"
                        #  , "South Carolina" = "SC"
                        #  , "South Dakota" = "SD"
                        #  , "Tennessee" = "TN"
                        #  , "Texas" = "TX"
                        #  , "Utah" = "UT"
                        #  , "Vermont" = "VT"
                        #  , "Virginia" = "VA"
                        #  , "Washington" = "WA"
                        #  , "West Virginia" = "WV"
                        #  , "Wisconsin" = "WI"
                        #  , "Wyoming" = "WY"
                  ), selected = "AL", selectize=FALSE),
  
      
      conditionalPanel(condition = "input.state != 'empty'",
                       selectizeInput("county_main", label = "County of Residency", choices = NULL, 
                                      options = list(
                                        placeholder = 'select',
                                        onInitialize = I('function() { this.setValue(""); }')
                                      ))
      ),
     
      
    fluidRow(
      column(6, offset = 0,
             numericInput("numadults",label="Number of Adults (19+)", value=1,min=0, max=6)),
      column(5, offset = 0,
             numericInput("numkids",label="Number of Children", value=0,min=0, max=6))
      
    ),
    
    conditionalPanel(condition = "input.numadults>1",
                     radioButtons("marital_status", label="Is the First Adult Married?",
                                  c("Yes" = "Yes",
                                    "No" = "No"
                                  ), selected = "No", inline = TRUE)),
    
    # selectInput("fam_disab", label="Does anyone in the home have a disability?",
    #             list("select" = "empty"
    #                  ,"No" = "No"
    #                  ,"Yes" = "Yes"), selectize=FALSE, selected = "empty"),
    
    
    ################## Age of Adult, Disability for Adult, and Adult Blind / not Blind
    
    conditionalPanel(condition = "input.numadults>=1",
                     
                     numericInput("age_adult_1", label = "Age of First Adult (Older than 19)", value = 25, min=19,max=99)
    ),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes'& input.numadults>=1)",
                     checkboxInput("disab1", "First adult has a disability" , FALSE)),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.numadults>=1)",
                     checkboxInput("blind1", label="First adult is legally blind", FALSE)),
    
    
      
    conditionalPanel(condition = "input.numadults>=2",
                     
                     numericInput("age_adult_2", label = "Age of Second Adult (Older than 19)", value = 25, min=19,max=99)
    ),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.numadults>=2)",
                     checkboxInput("disab2", "Second adult has a disability", FALSE)),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.numadults>=2)",
                     checkboxInput("blind2", label="Second adult is legally blind", FALSE)),
    
    
    
    conditionalPanel(condition = "input.numadults>=3",
                     
                     numericInput("age_adult_3", label = "Age of Third Adult (Older than 19))", value = 25, min=19,max=99)
    ),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes'  & input.numadults>=3)",
                     checkboxInput("disab3", "Third adult has a disability", FALSE)),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.numadults>=3)",
                     checkboxInput("blind3", label="Third adult is legally blind", FALSE)),
    
    
    
    conditionalPanel(condition = "input.numadults>=4",
                     
                     numericInput("age_adult_4", label = "Age of Fourth Adult (Older than 19)", value = 25, min=19,max=99)
    ),
    conditionalPanel(condition = "(input.fam_disab=='Yes'   & input.numadults>=4)",
                     checkboxInput("disab4", "Fourth adult has a disability", FALSE)),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.numadults>=4)",
                     checkboxInput("blind4", label="Fourth adult is legally blind", FALSE)),
    
     
    
    conditionalPanel(condition = "input.numadults>=5",
                     
                     numericInput("age_adult_5", label = "Age of Fifth Adult (Older than 19)", value = 25, min=19,max=99)
    ),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes'   & input.numadults>=5)",
                     checkboxInput("disab5", "Fifth adult has a disability", FALSE)),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.numadults>=5)",
                     checkboxInput("blind5", label="Fifth adult is legally blind", FALSE)),
    
    
  
    conditionalPanel(condition = "input.numadults>=6",
                     
                     numericInput("age_adult_6", label = "Age of Sixth Adult (Older than 19)", value = 25, min=19,max=99)
    ),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes'   & input.numadults>=6)",
                     checkboxInput("disab6", "Sixth adult has a disability", FALSE)),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.numadults>=6)",
                     checkboxInput("blind6", label="Sixth adult is legally blind", FALSE)),
    

    
    ################## Age of Child, Disability of Child
    
    conditionalPanel(condition = "input.numkids>=1",
                     
                     numericInput("age_child_1", label = "Age of First Child (Under 19)", value = NA_real_, min=0,max=18)
                     , h6("If child is less than 1 year old put 0.")
    ),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes'   & input.numkids>=1)",
                     checkboxInput("disab7", "First child has a disability", FALSE)),
    
    conditionalPanel(condition = "input.numkids>=2",
                     
                     numericInput("age_child_2", label = "Age of Second Child (Under 19)", value = NA_real_, min=0,max=18)
                     , h6("If child is less than 1 year old put 0.")
    ),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes'   & input.numkids>=2)",
                     checkboxInput("disab8", "Second child has a disability", FALSE)),
    
    conditionalPanel(condition = "input.numkids>=3",
                     
                     numericInput("age_child_3", label = "Age of Third Child (Under 19)", value = NA_real_, min=0,max=18)
                     , h6("If child is less than 1 year old put 0.")
    ),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes'   & input.numkids>=3)",
                     checkboxInput("disab9", "Third child has a disability", FALSE)),
    
    conditionalPanel(condition = "input.numkids>=4",
                     
                     numericInput("age_child_4", label = "Age of Fourth Child (Under 19)", value = NA_real_, min=0,max=18)
                     , h6("If child is less than 1 year old put 0.")
                     
                     
    ),
    conditionalPanel(condition = "(input.fam_disab=='Yes'   & input.numkids>=4)",
                     checkboxInput("disab10", "Fourth child has a disability", FALSE)),
    
    conditionalPanel(condition = "input.numkids>=5",
                     
                     numericInput("age_child_5", label = "Age of Fifth Child (Under 19)", value = NA_real_, min=0,max=18)
                     , h6("If child is less than 1 year old put 0.")
                     
    ),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes'   & input.numkids>=5)",
                     checkboxInput("disab11", "Fifth child has a disability", FALSE)),
    
    conditionalPanel(condition = "input.numkids>=6",
                     
                     numericInput("age_child_6", label = "Age of Sixth Child (Under 19)", value = NA_real_, min=0,max=18)
                     , h6("If child is less than 1 year old put 0.")
                     
    ),
    
    conditionalPanel(condition = "(input.fam_disab=='Yes'   & input.numkids>=6)",
                     checkboxInput("disab12", "Sixth child has a disability", FALSE)),
    
    
    
    ################## Incomes of other family members. Asks for individual incomes of adults if someone has a disability
    
    conditionalPanel(condition = "input.numadults>=2",
                     numericInput("income_other_family", label = "Monthly Earnings of Spouse/Partner and/or Other Family Members", value = NULL,  min=0, max=1000000000)
    ),
     
 #   conditionalPanel(condition = "input.fam_disab=='Yes' & input.numadults>=2 & input.marital_status == 'Yes'",
#                       numericInput("spouse_income", label = "Spouse Income (Monthly)", value = NULL,  min=0, max=1000000000)
#    ),
    
     
#    conditionalPanel(condition = "input.numadults>=2 & input.marital_status == 'No' & input.fam_disab=='Yes'",
#                      numericInput("second_adult_income", label = "Monthly Income of Second Adult", value = NULL,  min=0, max=1000000000)
#    ),
    
#     conditionalPanel(condition = "input.numadults>=3 & input.fam_disab=='Yes'",
#                        numericInput("third_adult_income", label = "Monthly Income of Third Adult", value = NULL,  min=0, max=1000000000)
#    ),
    
#    conditionalPanel(condition = "input.numadults>=4 & input.fam_disab=='Yes'",
#                     numericInput("fourth_adult_income", label = "Monthly Income of Fourth Adult", value = NULL,  min=0, max=1000000000)
#    ),
#    conditionalPanel(condition = "input.numadults>=5 & input.fam_disab=='Yes'",
#                        numericInput("fifth_adult_income", label = "Monthly Income of Fifth Adult", value = NULL,  min=0, max=1000000000)
#    ),
    
#      conditionalPanel(condition = "input.numadults>=6 & input.fam_disab=='Yes'",
#                        numericInput("sixth_adult_income", label = "Monthly Income of Sixth Adult", value = NULL,  min=0, max=1000000000)
#   ),
    
   
   # Public Benefits drop down menu
      selectInput(
        'benefit1', 'Public Assistance', 
        list("select" = "empty",  "All programs",  "No programs",# "All programs except Section 8 and CCDF", 
             "Select a custom list"), selected = "empty", selectize=FALSE),
    
   # All programs all for all states except FL and CT
    conditionalPanel(condition ="input.benefit1=='Select a custom list' & (input.state != 'FL' & input.state != 'CT')",
                     selectInput("benefit2a", label = "Select Public Assistance Programs", 
                                 list("Supplemental Nutrition Assistance Program (SNAP)", "Free or Reduced Price School Meals", "Women, Infants and Children Nutrition Program (WIC)",
                                      "Temporary Assistance for Needy Families (TANF)",
                                      "Child Care Subsidy (CCDF)", "Head Start/Early Head Start","State-Funded Pre-Kindergarten" , "Section 8 Housing Choice Voucher", 
                                      "Medicaid for Adults","Medicaid for Children/CHIP",
                                      "Health Insurance Marketplace Subsidy", 
                                      "Earned Income Tax Credit (EITC)", "Child Tax Credit (CTC)", "Child and Dependent Care Tax Credit (CDCTC)"
                                      #,"Supplemental Security Income (SSI)", "Social Security Disability Insurance (SSDI)"
                                 ),
                                 selected= NULL, multiple = TRUE),  
    ),
    
    
    #FL (same as 2a w/ FATES in the list)
    conditionalPanel(condition = "input.benefit1=='Select a custom list' & input.state=='FL'",
                     selectInput("benefit2d", label = "Select Public Assistance Programs", 
                                 list("Supplemental Nutrition Assistance Program (SNAP)", "Free or Reduced Price School Meals", "Women, Infants and Children Nutrition Program (WIC)",
                                      "Child Care Subsidy (CCDF)", "Head Start/Early Head Start","State-Funded Pre-Kindergarten" , "Section 8 Housing Choice Voucher", 
                                      "Medicaid for Adults","Medicaid for Children/CHIP","Temporary Assistance for Needy Families (TANF)",
                                      "Health Insurance Marketplace Subsidy", 
                                      "Earned Income Tax Credit (EITC)", "Child Tax Credit (CTC)", "Child and Dependent Care Tax Credit (CDCTC)"
                                     # ,"Supplemental Security Income (SSI)", "Social Security Disability Insurance (SSDI)"#,"FATES"
                                 ),
                                 selected= NULL, multiple = TRUE)   
    ),
    
    # CT (same as 2a, but with RAP)
    conditionalPanel(condition ="input.benefit1=='Select a custom list' & (input.state=='CT') ",
                     selectInput("benefit2e", label = "Select Public Assistance Programs",
                                 list("Supplemental Nutrition Assistance Program (SNAP)", "Free or Reduced Price School Meals", "Women, Infants and Children Nutrition Program (WIC)",
                                      "Temporary Assistance for Needy Families (TANF)",
                                      "Child Care Subsidy (CCDF)", "Head Start/Early Head Start","State-Funded Pre-Kindergarten" , "Section 8 Housing Choice Voucher",
                                      "Medicaid for Adults","Medicaid for Children/CHIP",
                                      "Health Insurance Marketplace Subsidy",
                                      "Earned Income Tax Credit (EITC)", "Child Tax Credit (CTC)", "Child and Dependent Care Tax Credit (CDCTC)", "RAP"
                                      #,"Supplemental Security Income (SSI)", "Social Security Disability Insurance (SSDI)"
                                 ),
                                 selected= NULL, multiple = TRUE),
    ),
  
    # conditionalPanel(condition="input.benefit1 == 'All programs' | input.benefit2a.includes('Supplemental Security Income (SSI)') | input.benefit2d.includes('Supplemental Security Income (SSI)') | input.benefit2e.includes('Supplemental Security Income (SSI)')",
    # 
    #                 h6("Note: Many types of income are assumed to be $0 for the purposes of calculating SSI. See FAQ tab for details.")
    # 
    #                  ),


   # # Former SSI recipients may continue to qualify for Medicaid
   #  conditionalPanel(condition = "(input.fam_disab=='Yes' & (input.benefit1=='All programs' | input.benefit2a.includes('Medicaid for Adults') | input.benefit2d.includes('Medicaid for Adults') | input.benefit2e.includes('Medicaid for Adults')))",
   #                   radioButtons("prev_ssi", "Has anyone in the home ever received SSI?",
   #                                c("No" = "No",
   #                                  "Yes" = "Yes"), selected = "No", inline=TRUE)),
   # 
   # # Ask for expense spent on special equipment for SSI recipients
   #  conditionalPanel(condition = "(input.benefit1 == 'All programs' | input.benefit2a.includes('Supplemental Security Income (SSI)') |  input.benefit2d.includes('Supplemental Security Income (SSI)') | input.benefit2e.includes('Supplemental Security Income (SSI)'))",
   #                   numericInput("disab.work.exp", label="Amount spent ($) per month on specialized equipment or services that enable household member(s) with disabilities to work",value=0, min=0, max=10000)),
   # 
   # # PIA is the benefit received by SSDI recipients - auxiliary benefits for family is calculated
   #  conditionalPanel(condition = "(input.numadults>=1) & (input.benefit1 == 'All programs' | input.benefit2a.includes('Social Security Disability Insurance (SSDI)') | input.benefit2d.includes('Social Security Disability Insurance (SSDI)') | input.benefit2e.includes('Social Security Disability Insurance (SSDI)'))",
   #                   numericInput("ssdiPIA1", label="Amount first adult receives ($) per month in SSDI payments",value=0, min=0, max=10000, step=100),
   #                   h6("Do not include auxiliary benefits that are for children, spouses, or other family members")),
   # 
   #  conditionalPanel(condition = "(input.numadults>=2) & (input.benefit1 == 'All programs' | input.benefit2a.includes('Social Security Disability Insurance (SSDI)')  | input.benefit2d.includes('Social Security Disability Insurance (SSDI)') | input.benefit2e.includes('Social Security Disability Insurance (SSDI)'))",
   #                   numericInput("ssdiPIA2", label="Amount second adult receives ($) per month in SSDI payments",value=0, min=0, max=10000, step=100),
   #                   h6("Do not include auxiliary benefits that are for children, spouses, or other family members")),
   # 
   #  conditionalPanel(condition = "(input.numadults>=3) & (input.benefit1 == 'All programs' | input.benefit2a.includes('Social Security Disability Insurance (SSDI)')  | input.benefit2d.includes('Social Security Disability Insurance (SSDI)') | input.benefit2e.includes('Social Security Disability Insurance (SSDI)'))",
   #                   numericInput("ssdiPIA3", label="Amount third adult receives ($) per month in SSDI payments",value=0, min=0, max=10000, step=100),
   #                   h6("Do not include auxiliary benefits that are for children, spouses, or other family members")),
   # 
   #  conditionalPanel(condition = "(input.numadults>=4) & (input.benefit1 == 'All programs' | input.benefit2a.includes('Social Security Disability Insurance (SSDI)')  | input.benefit2d.includes('Social Security Disability Insurance (SSDI)') | input.benefit2e.includes('Social Security Disability Insurance (SSDI)'))",
   #                   numericInput("ssdiPIA4", label="Amount fourth adult receives ($) per month in SSDI payments",value=0, min=0, max=10000, step=100),
   #                   h6("Do not include auxiliary benefits that are for children, spouses, or other family members")),
   # 
   #  conditionalPanel(condition = "(input.numadults>=5) & (input.benefit1 == 'All programs' | input.benefit2a.includes('Social Security Disability Insurance (SSDI)')  | input.benefit2d.includes('Social Security Disability Insurance (SSDI)') | input.benefit2e.includes('Social Security Disability Insurance (SSDI)'))",
   #                   numericInput("ssdiPIA5", label="Amount fifth adult receives ($) per month in SSDI payments",value=0, min=0, max=10000, step=100),
   #                   h6("Do not include auxiliary benefits that are for children, spouses, or other family members")),
   # 
   #  conditionalPanel(condition = "(input.numadults>=6) & (input.benefit1 == 'All programs' | input.benefit2a.includes('Social Security Disability Insurance (SSDI)')  | input.benefit2d.includes('Social Security Disability Insurance (SSDI)') | input.benefit2e.includes('Social Security Disability Insurance (SSDI)'))",
   #                   numericInput("ssdiPIA6", label="Amount sixth adult receives ($) per month in SSDI payments",value=0, min=0, max=10000, step=100),
   #                   h6("Do not include auxiliary benefits that are for children, spouses, or other family members")),

   
  br(),
  
  # Broad Occupation drop down menu
  tags$div(align = "center", h4("Target Occupation")),
  br(),
  selectInput("industry1", "Broad Occupation Group",
              list("select" = "empty",  
                   "Architecture and Engineering Occupations" = "industry1",
                   "Arts, Design, Entertainment, Sports, and Media Occupations" = "industry2",
                   "Building and Grounds Cleaning and Maintenance Occupations" = "industry3",
                   "Business and Financial Operations Occupations" = "industry4",
                   "Community and Social Service Occupations" = "industry5",
                   "Computer and Mathematical Occupations" = "industry6",
                   "Construction and Extraction Occupations" = "industry7",
                   "Educational Instruction and Library Occupations" = "industry8",
                   "Farming, Fishing, and Forestry Occupations" = "industry9",
                   "Food Preparation and Serving Related Occupations" = "industry10",
                   "Healthcare Practitioners and Technical Occupations" = "industry11",
                   "Healthcare Support Occupations" = "industry12",
                   "Installation, Maintenance, and Repair Occupations" = "industry13",
                   "Legal Occupations" = "industry14",
                   "Life, Physical, and Social Science Occupations" = "industry15",
                   "Management Occupations" = "industry16",
                   "Office and Administrative Support Occupations" = "industry17",
                   "Personal Care and Service Occupations" = "industry18",
                   "Production Occupations" = "industry19",
                   "Protective Service Occupations" = "industry20",
                   "Sales and Related Occupations" = "industry21",
                   "Transportation and Material Moving Occupations" = "industry22"
              ), selected = "empty", selectize=FALSE # "Architecture and Engineering Occupations"
  ),
  
  conditionalPanel(condition = "input.industry1 != 'empty'",
                   selectizeInput("occupation1", "Select occupation", choices = NULL
                                  , options = list(placeholder = 'select'
                                                   ,onInitialize = I('function() { this.setValue(""); }')
                                  ))
                   ,h5(strong("Tip: "),"Try searching by keyword on ", (strong(tags$u(tags$a(href="https://www.mynextmove.org/",target="_blank","mynextmove.org")))), " to get the official BLS occupation name.")
                   ,br()
                   ),
    
    
    selectInput("edu_req_1", "Duration of Education or Training Program", 
                
                list("select" = "empty", 
                     "Use typical duration required for occupation" = "yes_typical_1",
                  "Enter custom duration" = "yes_custom_1",
                  "No education or training required" = "no_1"
                  
                ), selected = "empty", selectize=FALSE
             
    ),
    
    
    conditionalPanel(condition = "input.edu_req_1=='yes_custom_1'",
                     fluidRow(
                       column(5
                              ,h5(strong("Duration of schooling:"))
                              ,br()
                              ,br()
                       )
                       ,column(3,
                               #textInput("degree_title", label=NULL, value=NULL)
                               numericInput("schooling_years_1", value = 0, label=NULL, min=0, max=9, width='91%'),
                               numericInput("schooling_months_1", value = 0, label=NULL, min=0, max=11, width='91%')
                       )
                       ,column(3,
                               div(style="margin-top:0.5em; margin-left:-2.5em",
                                   strong(p("year(s)")))
                               ,br()
                               ,div(style="margin-top:0.5em; margin-left:-2.5em",strong(p("month(s)")))
                       )
                       
                     )
    ),    
    
  
  br(),
  
  br(),
  radioButtons("manual_selection_occupation", label = "Occupation for Comparsion", 
              
              list("Near-minimum wage job" = "manual_no",
                   "Other" = "manual_yes"
                   
              ), selected = "manual_no"
              
  ),
    
  
  conditionalPanel(condition = "input.manual_selection_occupation == 'manual_yes'",
                   
                   
                   selectInput("industry2", "Broad Occupation Group",
                               list("select" = "empty",  
                                    "Architecture and Engineering Occupations" = "industry1",
                                    "Arts, Design, Entertainment, Sports, and Media Occupations" = "industry2",
                                    "Building and Grounds Cleaning and Maintenance Occupations" = "industry3",
                                    "Business and Financial Operations Occupations" = "industry4",
                                    "Community and Social Service Occupations" = "industry5",
                                    "Computer and Mathematical Occupations" = "industry6",
                                    "Construction and Extraction Occupations" = "industry7",
                                    "Educational Instruction and Library Occupations" = "industry8",
                                    "Farming, Fishing, and Forestry Occupations" = "industry9",
                                    "Food Preparation and Serving Related Occupations" = "industry10",
                                    "Healthcare Practitioners and Technical Occupations" = "industry11",
                                    "Healthcare Support Occupations" = "industry12",
                                    "Installation, Maintenance, and Repair Occupations" = "industry13",
                                    "Legal Occupations" = "industry14",
                                    "Life, Physical, and Social Science Occupations" = "industry15",
                                    "Management Occupations" = "industry16",
                                    "Office and Administrative Support Occupations" = "industry17",
                                    "Personal Care and Service Occupations" = "industry18",
                                    "Production Occupations" = "industry19",
                                    "Protective Service Occupations" = "industry20",
                                    "Sales and Related Occupations" = "industry21",
                                    "Transportation and Material Moving Occupations" = "industry22"
                               ), selected = "empty", selectize=FALSE # "Architecture and Engineering Occupations"
                   ),
                   
                   conditionalPanel(condition = "input.industry2 != 'empty'",
                                    selectizeInput("occupation2", "Select occupation", choices = NULL
                                                   , options = list(placeholder = 'select'
                                                                    ,onInitialize = I('function() { this.setValue(""); }')
                                                   ))
                                    ,h5(strong("Tip: "),"If an occupation is missing, try searching by keyword on ", (strong(tags$u(tags$a(href="https://www.mynextmove.org/",target="_blank","mynextmove.org")))), " to get the official BLS occupation name.")
                                    ,br()
                   ),
                   
                   
                   selectInput("edu_req_2", "Duration of Education or Training Program", 
                               
                               list("select" = "empty",  
                                    "Use typical duration required for occupation" = "yes_typical_2",
                                    "Enter custom duration" = "yes_custom_2",
                                    "No education or training required" = "no_2"
                                    
                               ), selected = "empty", selectize=FALSE
                               
                   ),
                   
                   
                   conditionalPanel(condition = "input.edu_req_2=='yes_custom_2'",
                                    fluidRow(
                                      column(5
                                             ,h5(strong("Duration of schooling:"))
                                             ,br()
                                             ,br()
                                      )
                                      ,column(3,
                                              #textInput("degree_title", label=NULL, value=NULL)
                                              numericInput("schooling_years_2", value = 0, label=NULL, min=0, max=9, width='91%'),
                                              numericInput("schooling_months_2", value = 0, label=NULL, min=0, max=11, width='91%')
                                      )
                                      ,column(3,
                                              div(style="margin-top:0.5em; margin-left:-2.5em",
                                                  strong(p("year(s)")))
                                              ,br()
                                              ,div(style="margin-top:0.5em; margin-left:-2.5em",strong(p("month(s)")))
                                      )
                                      
                                    )
                   ),      
                   
                   ),

      
  br()
  ,tags$div(align = "center",
            useShinyjs(),
            extendShinyjs(text = jscode1, functions = c("getresults")),
                        actionButton("getresults","Calculate Results", 
                                     style="background-color: rgb(20,68,104); border-color: rgb(20,68,104)",
                                     class="btn btn-primary btn-lg",
                                     width = '50%')
            )
            
             
 
 
 ) #closes sidebar panel 
  
  
  ,mainPanel(
    
    tabsetPanel(id="tabs",
      
      tabPanel("Welcome"
               ,includeHTML("home.html") # Home page - written in HTML
               ),
      
      tabPanel("Results",
               
               conditionalPanel(condition = "input.getresults==0",
                                tags$div(align = "center",
                                         
                                         br(),
                                         br(),
                                         br(),
                                         h3('Click the ', strong('Calculate Results'), 'button to get results')
                                         
                                         
                                )),
               
               conditionalPanel(condition = "input.getresults>0" 
                                
                                ,br()
                                ,tags$div(align = "center",
                                fluidRow(
                                  column(7),
                                  column(2),
                                  column(3, 
                                                  downloadButton("print2_1", "Download Chart Data",
                                                                 style="background-color: rgb(20,68,104); border-color: rgb(20,68,104)",
                                                                 class="btn btn-primary btn-sm"))
                                )),
                                br(),
                                
                                tags$div(align = "center",
                                         tags$h4(strong("View the results of your projected career path below."))
                                         ,h4(strong("To save these results, hit CTRL+P on your keyboard (COMMAND+P on Mac) and print to PDF.")),
                                )
                                
                           ,br()
                          ,includeHTML("results1.html") # results text - written in HTML
                         
                           ,br()  
                         ,br()
                         ,br()
                           ,withSpinner(plotlyOutput('gross.income', width = "1000px", height = "700px"),type=2)
                           
                           ,br()
                           ,br()

                           ,includeHTML("results2.html") # results text - written in HTML
                        ,br()
                        ,br()
                        ,br()
                           ,withSpinner(plotlyOutput('after.tax.self.sufficiency', width = "1000px", height = "700px"),type=2)
                           
                           ,br()
                           ,br()
                           ,br()
                          
                           
                           ,bsCollapse(id = "grossIncome", multiple = TRUE,
                                      bsCollapsePanel("What's Included in the Minimum Household Budget?" 
                                                    ,includeHTML("results3.html") 
                                                     ,br()
                                                   ,br()
                                                      ,plotlyOutput("expenses.breakdown", width = "900px", height = "700px")
                                                      ,br()
                                                      ,br()
                                                      ,br()
                                                      ,br()
                                                      ,br()
                                                      ,br()
                                                      ,br()                                                                                                             
                                                      ,style = "primary")   
                           )
                           ,br()
                           
                        ,includeHTML("results4.html")         
                           , br()
                           ,br()
                           
                        ,     withSpinner(plotlyOutput('transfer.breakdown'
                                        , width = "1050px", height = "700px"),type=2)
               , h5("*All jobs are assumed to provide employer sponsored health insurance as an option. Of all options available to the household, we assume the lowest cost option(s) for health insurance is chosen. See the FAQ tab for details")
                                   ,br()
                              ,br()
                           ,br()
                           ,br()
                           ,br()
                           ,br()
                           ,br()
                           ,br()
                           ,br()
                         
                           
                           ,bsCollapse(id = "pubAssistance", multiple = TRUE,
                                      bsCollapsePanel("How does Public Assistance Eligibility change by Family Income?", 
                                                      h4("The chart below shows how the dollar value and composition of public assistance changes as income increases in $1,000 increments.",HTML(paste0(tags$sup("1")))),
                                                      br(),
                                                      br(),
                                                      br(),
                                                      plotlyOutput('csbenefit.breakdown', width = "900px", height = "700px"),
                                                                   br(),
                                                       br(),
                                                      br(),
                                                      br(),
                                                      br(),
                                                      br(),
                                                      br(),
                                                      h6(strong(HTML(paste0(tags$sup("1"))), " Here, we assume an individual is not eligible for health insurance through their employer. For all other charts, employer sponsored health insurance is assumed to be provided."))
                                                      ,style = "primary")
                           )
                           
                           
                           
                           ,br()
               ,includeHTML("results5.html")         
               
                                ,br()
                           ,br()
               ,br()
                           , plotlyOutput('net.res', width = "1000px", height = "700px")
                           ,br()
                           
                           
                           ,bsCollapse(id = "netResources", multiple = TRUE,
                                       bsCollapsePanel("What Is the Financial Return of the Target Occupation?" 
                                                       
                                                       ,br()
                                                    ,includeHTML("results6.html")   
                                                      ,br()
                                                       ,br()
                                                    ,br()
                                                       ,withSpinner(plotlyOutput('net.res.life', width = "900px", height = "700px"),type=2)
                                                       ,br()
                                                       ,br()
                                                       
                                                       ,style = "primary")   
                           ),
                        
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br()

               )  
      
       ) #closes tabPanel("results")

      ,tabPanel("For Policymakers",
                
                conditionalPanel(condition = "input.getresults==0",
                                 tags$div(align = "center",
                                          br(),
                                          br(),
                                          br(),
                                 h3('Click ', strong('Calculate Results'), 'button to get results')
                                 
                                 
                                 )),
                
                conditionalPanel(condition = "input.getresults>0" ,
                br()
                ,tags$div(align = "center",
                          fluidRow(
                            column(7),
                            column(2),
                            column(3, 
                                   downloadButton("print2_2", "Download Chart Data",
                                                  style="background-color: rgb(20,68,104); border-color: rgb(20,68,104)",
                                                  class="btn btn-primary btn-sm"))
                          )),
                br(),
                tags$div(align = "center",
                         tags$h4(strong("View the effects of one's projected career path on federal and state budgets below."))
                         ,h4(strong("To save these results, hit CTRL+P on your keyboard (COMMAND+P on Mac) and print to PDF.")),
                )
                ,br()
                ,includeHTML("results7.html")         
            ,br()
            , br()
            ,br()
               , withSpinner(plotlyOutput('taxes.total', width = "900px", height = "700px"),type=2)
               , br()
               ,br()
            ,includeHTML("results8.html")         
            
                 , br()
               , withSpinner(plotlyOutput('net.tax.life', width = "950px", height = "700px"),type=2)
               , br()
               ,br()
      )
               
        ) #closes tabPanel("policiy makers")
      
      , tabPanel("FAQ"
                 ,br()
                 ,includeHTML("FAQ1.html")         
        ,br()
        ,br()                   
                           , h2("Methodology")
                           , br()
                       ,includeHTML("FAQ2.html")         
        
             ,br()
        ,br()                   
                           ,h2("Programs and Tax Credits")
                           ,br()
        ,includeHTML("FAQ3.html")  
      
                           
              
                           )
#closes tab panel FAQ    
      
      
    ) #closes tabsetpanel 
  ) #closes sidebarlayout 
  ) #closes mainpanel
 
 ,div(class = "footer",
       includeHTML("www/include_footer.html"))
 
) #closes fluidpage

