#######################################################
#######################################################
# Global - NO CUSTOMIZATION IS REQUIRED
#######################################################
#######################################################
#library(shinyjs)

library(shinycssloaders)
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

scenario1 <<- FALSE
scenario2 <<- FALSE

# Call all the functions
source(paste0("mainFiles/loadFilesandFunctions.R"), local=TRUE) # Load auxiliary files and required functions
source(paste0("mainFiles/libraries.R"), local=TRUE) # Load required packages
source(paste0("mainFiles/functionsForGlobal.R"), local=TRUE) # Load required packages


loc_meta <- fread('locations_list.csv')
occ_meta <- fread('occupations_list.csv')
# START 9:14 AM 
budget.ALICE<-"survivalforcliff"

ruleYear <- 2023

#childcare needs assumptions
k_ftorpt <- "FT" #not used anywhere in benefits calc right now
schoold <- dagesummercare <- "PT" #don't change - ben calc not yet set up to handle FT
headstart_ftorpt <-"PT" #use the same for both headstart & earlyheadstart 
preK_ftorpt <- "PT"
contelig.headstart <- TRUE
contelig.earlyheadstart <- TRUE
contelig.ccdf <- TRUE

# Austin, why do you need all these settings below ? Can they go some place else

shapes <- c(16,17,15)
#shapes_2 <- c(15,16,17)
line_colors <- c("#661100","#E69F00")
line_colors2 <- c("#661100","#E69F00")
line_colors3 <- c("#E69F00","#661100")
line_colors4 <- c("#661100","#D55E00")
line_colors3 <- c("#E69F00", "#661100")
FATES_colors <- c("Net Resources without FATES", "Net Resources with FATES")

# Global parameters for lifetime projections
# Lifetime horizon is fixed
years<-seq(ruleYear,2070,by=1)
income<-seq(1000,119000,by=1000)

# Assumption on when child leaves the house
child.leaves.house<-19

# Two default career paths
careerpathIDs<-c(1,2)

 schooling_1 <- 0
 schooling_2 <- 0
 income.otherfamily<-0
 
Data_Dictionary <- read_csv("Data_Dictionary.csv")
 
 jscode1 <- "shinyjs.getresults = function() {window.scrollTo(0, 0);}"

 page <- 0