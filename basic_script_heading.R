# ########################################
# filename: filename.R
# author: thiago Alckmin Dec/2022
# purpose: explain what the code is about
# ########################################

# Section 0: set-up --------------------

print(Sys.time())
rm(list = ls())
gc()

# Section 0.1: load helper functions ----
source("https://raw.githubusercontent.com/Thiago-Alckmin/helper_functions_scripts/main/helper_functions_analysis.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/Thiago-Alckmin/helper_functions_scripts/main/helper_functions_simple.R", encoding = "UTF-8")

save_global_env(directory="/home/tresende/Global_Political_Connections/general_purpose/code/global_env/")