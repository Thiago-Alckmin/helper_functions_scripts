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

# these are the helper functions I continuously create and update on Github
source("https://raw.githubusercontent.com/Thiago-Alckmin/helper_functions_scripts/main/helper_functions_analysis.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/Thiago-Alckmin/helper_functions_scripts/main/helper_functions_simple.R", encoding = "UTF-8")

# since I update helper functions regularly, its always a good idea to save the current global environment so that we at least have access to older functions 
save_global_env(directory="your_working_directory/global_env/")