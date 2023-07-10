# ##################################################
# filename: search_within_files.R
# author: thiago Aug 2022
# purpose: helper code to find stings within files
# status: DONE
# ##################################################

require(readtext)
require(glue)
require(data.table)
require(magrittr)

# function to find string in files for given file type and directory
find_string <- function(
    
  # parameters
  directory = "C:/Users/alckm/Dropbox/Uganda/Programs/Archive",
  string = "PDE_Panel\\.dta",
  file_type = "\\.do"){
  # get files in directory which match extension
  files <- directory %>%
    list.files(path = ., full.names = T) %>%
    .[grep(x = ., pattern = file_type)]
  
  # for each file in directory
  for( file in files){
    # create a unit vector T/F to determine if string exists in doc
    tmp <- file %>%
      read_lines(file = .) %>%
      glue_collapse(x = .) %>%
      str_detect(., string) %>%
      c()
    
    # name the element the file without extenssion
    names(tmp) <- file %>% str_remove_all(., pattern = directory)
    # convert to dt
    tmp <- tmp %>%
      as.data.table(keep.rownames = T) %>%
      dplyr::rename(., file = rn, contains_string = ".")
    
    
    if (file == files[1]) {
      out <- copy(tmp)
    } else{
      out <- tmp %>% copy() %>% rbind(out, .)
    }
  }
  
  out %>% .[order(-contains_string)] %>% return()
}


# example
# check_here <- find_string(  directory = "C:/Users/alckm/Dropbox/Uganda/Programs/",
#               string = "proc_rec_all\\.dta",
#               file_type = "\\.do" ) 
# 
# View(check_here)
