
# Load all necessary supplementary files
load(paste0("mainFiles/Database/tables.rdata"))
load(paste0("mainFiles/Database/parameters.defaults.rdata"))
load(paste0("mainFiles/Database/expenses.rdata"))
load(paste0("mainFiles/Database/benefit.parameters.rdata"))
load(paste0("mainFiles/Database/coeff_all.rdata")) # Estimated wage growth (Current methodology)

# Load all necessry functions
source(paste0("mainFiles/functions/benefits_functions.R"), local=TRUE) # Benefits calculations
source(paste0("mainFiles/functions/expense_functions.R"), local=TRUE) # Expenses calculationss
source(paste0("mainFiles/functions/wage_projection.R"), local=TRUE) # Wage growth functionsso
source(paste0("mainFiles/functions/BenefitsCalculator_functions.R"), local=TRUE) # Benefits Calculator functions

#})
