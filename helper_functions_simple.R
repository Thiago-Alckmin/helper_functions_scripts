

print_line <- function(){
  print("---------------------------------------------------")
  }



################################################################################
# Section 1: mathematical helper functions #####################################
################################################################################

# Section 1.1: is greater than  ----
is_greater_than <- function(x, n){return(x>n)}

# Section 1.2: is greater than  or equal to ----
is_greater_than_or_equal_to <- function(x, n){return(x>=n)}

# is greater than zero
is_greater_than_zero <- function(x){return(x>0)}

# Section 1.3: is x greater than zero ----
is_greater_than_zero <- function(x) {
  out <- (x > 0) * 1
  return(out)
}


# Section 1.4: missing as zero ----
na_as_zero <- function(x){
  if(is.na(x)){
    return(0)
  }else{return(x)}
}
# create x 0s according to vector specification ----
n_zeroes <- function(vec){
  
  out <- as.character(vec)
  
  for(i in 1:length(vec)){
    
    v <- vec[i]
    
    if((v==0)|is.na(v)){
      
      if(is.na(v)){
        
        out[i] <- ""
        
      }else{
        
        out[i] <- ""
        
      }
      
      
      out[i] <- ""
      
    }else{
      
      out[i] <- v %>% rep("0", .) %>% glue::glue_collapse(x=.)
      
    }
  }
  
  
  out[is.na(vec)] <- NA
  
  
  return(out)
}




################################################################################
# Section 2: column names ######################################################
################################################################################

# Section 2.1: get new names between old and new data.table ------
get_new_column_names <- function(old_datatable,
                                 new_datatable){
  
  # get original names
  old_names <- names(old_datatable)
  
  # get dummy names from owners
  new_additional_names <-
    names(new_datatable) %>%
    .[!(. %in% old_names)]
  
  return(new_additional_names)
  
}




# Section 2.2: function to rename vector of variables from data.table ----
rename_columns <- function(datatable, current_names, new_names){

  # get all names
  all_names <- names(datatable)
  
  # find variables to not change
  do_not_change <- all_names %>% .[!(. %in% current_names)]
  
  # check if names that won't be changed intersect with new names 
  if(sum(new_names %in% do_not_change)==1){
    
    # get the variables that already exist
    variables_that_already_exist <-  new_names %>% .[. %in% do_not_change] %>% glue::glue_collapse(sep = ", ")
    
    warning_message <- paste0("At least one elelment of new_name vector already exists as a variable which will not be renamed.", 
                              " The funciton will return the original data-table instead!!",
                              " The variables in question are: ", 
                              variables_that_already_exist)
    
    warning(warning_message)
    
    return(datatable)
    
  }else{
    
    # separate variables to change and not change
    data_do_not_change <- datatable %>% copy() %>% .[, ..do_not_change]
    data_change <- datatable %>% copy() %>% .[, ..current_names]
    
    # change variables we wish to rename
    names(data_change) <- new_names
    
    # join data back together (tho perhaps in different order)
    out <- cbind(data_do_not_change, data_change)
    
    return(out)
    
  }
}

# get vairable name that matches a suffix ----
get_variable_name_with_suffix <- function(datatable, suffix){
  
  # delete later  
  # datatable <- surname_fyi
  # suffix <- local_origin_num
  
  # ---
  
  column_name <- names(datatable) %>% 
    .[endsWith(x = ., suffix = as.character(suffix))] %>% 
    c(.)
  
  return(column_name)
}


# clean numerical suffix to avoid matching confusion ----
clean_numerical_suffix <- function(variables){ #clean_numerical_suffix
  
  # delete later
  # variables <- c("fy_indicator_alo_education_max_2",
  #                "fy_indicator_alo_education_max_20",
  #                "fy_indicator_alo_education_max_NF",
  #                "fy_indicator_alo_education_max_0", 
  #                "fy_indicator_alo_education_max_", 
  #                "fy_indicator_alo_education_max_987", 
  #                "fy_indicator_alo_education_max_peter")
  
  
  # copy original variables 
  variables_original <- copy(variables)
  
  # get digit suffixes
  digit_suffixes <- variables_original %>% get_digits_after_last_underscore(.)
  # identify which ones are not digits
  suffix_is_not_digit <- is.na(digit_suffixes)
  
  # get variable roots
  roots <- remove_suffix_from_variables(
    variables = variables,
    suffixes = digit_suffixes)
  
  # number of digits in each suffix
  digit_suffixes_nchar <- digit_suffixes %>% nchar()
  # max number of digits in all suffixes
  digit_suffixes_nchar_max <- 3 #digit_suffixes_nchar %>% max(., na.rm=T)
  # difference between max and actual
  diff <- (digit_suffixes_nchar_max - digit_suffixes_nchar)
  
  # get the number of zeros to add to suffix
  zeros_to_add <- n_zeroes(diff)  
  
  # paste the appropriate amount of zeros to the digits 
  new_suffix <- paste0(zeros_to_add, digit_suffixes) #%>% 
  # if the suffix digit is missing/suffix is a string, then it appears as "NANA"; so drop it
  #str_remove_all(., "NANA")
  
  # get the new full variable names 
  renamed <- paste0(roots, new_suffix)
  
  # varables with no digit suffix are returned as the original names 
  renamed[suffix_is_not_digit] <- variables_original[suffix_is_not_digit]
  
  out <- copy(renamed)
  
  return(out)
  
}


# order these first -----
order_these_first <- function(vector, first){
  
  vector %>% 
    append(first, .) %>% 
    .[!duplicated(.)] %>% 
    return()
  
}


# remove suffix from variables ----
remove_suffix_from_variables <- function(variables, suffixes){
  
  variables_nchar <-  nchar(variables)
  suffixes_nchar <-  nchar(suffixes) %>% lapply(., na_as_zero) %>% unlist
  diff_nchar <- variables_nchar - suffixes_nchar
  
  roots <- variables
  for(i in 1:length(variables)){
    
    roots[i] <- substring(variables[i], 1, last = diff_nchar[i])
    
  }
  
  return(roots)
  
}

################################################################################
# Section 3: String operations #################################################
################################################################################

# Section 3.1: clean names  -----
standardize_name_column <- function(
    datatable, 
    column, 
    drop_common_titles_when_one_comma,
    drop_common_titles_when_more_than_one_comma,
    change_order_because_of_comma){
  
  # DELETE LATER
  # datatable <- cia_source_0119 %>% copy()
  # column <- "politician1"
  # change_order_because_of_comma <- T
  # drop_common_titles_when_one_comma <- T
  # drop_common_titles_when_more_than_one_comma <- T
  
  print_line %>% print()
  paste0("Standardizing name column: (", column ,")") %>%  print()
  print_line %>% print()
  
  
  # 1) set-up: get original names ----
  original_names <- names(datatable)
  # keep original columns + clean column 
  keep_these <- paste0(column, "_clean") %>% append(original_names, .)
  
  datatable_tmp <-  datatable %>% copy() %>% 
    # 2)  create an internal column to reorder in the data in end ---- 
  generate_internal_order_column(datatable = .) %>% 
    # 3) rename 'column' for practicality ----
  rename_columns(
    datatable = .,
    current_names = c(column),
    new_names = c("column")) %>%
    # 4) transliterate from latin1 to ASCII (simple 32-127 code point characters) ----
  .[, column2 := stri_trans_general(column, 'Any-Latin') ] %>% 
    .[, column2 := stri_trans_general(column2, 'Any-Latn') ] %>% 
    .[, column2 := stri_trans_general(column2, 'Latin-ASCII') ] %>% 
    # 5) everything upper case ----
  .[, column2 := str_to_upper(column2)] %>% 
    # 6) drop instances of multiple commas together
    .[, column2 := str_replace(column2, pattern = ",,", replacement = ",")]  %>% 
    # indicate number of commas 
    .[, n_commas := str_count(column2, ",")]  %>% 
    .[n_commas>0, column2 := str_remove_all_trailing_commas_and_spaces(column2) ] %>% 
    .[, n_commas := str_count(column2, ",")] 
  
  
  print_line %>% print()
  paste0("1) String column has been transliterated into ASCII, upper case characters.") %>%  print()
  print_line %>% print()
  
  # 6) deal with commas -----
  
  
  # 6.0)  split into three datatables depending on number of commas -----
  contain_commas_char0 <- datatable_tmp %>% copy() %>% .[n_commas==0]
  contain_commas_char1 <- datatable_tmp %>% copy() %>% .[n_commas==1]
  contain_commas_char2 <- datatable_tmp %>% copy() %>% .[n_commas>1]
  
  # 6.1) for rows with more than one comma -----
  if(drop_common_titles_when_more_than_one_comma){
    
    if(nrow(contain_commas_char2)>0){
      
      # 6.3: for rows with more than one comma ----
      contain_commas_char2 %<>%
        # drop titles 
        .[, column2 := drop_common_titles(column2, prefix = " ", suffix = " ")]  %>% 
        .[, column2 := drop_common_titles(column2, prefix = " ", suffix = "")]  %>% 
        .[, column2 := drop_common_titles(column2, prefix = "", suffix = " ")]  %>% 
        .[, column2 := drop_common_titles(column2, prefix = " ", suffix = ",")]  %>% 
        .[, column2 := drop_common_titles(column2, prefix = ",", suffix = " ")]  %>% 
        .[, column2 := str_trim(column2)]  %>% 
        # recompute number of commas
        .[, n_commas := str_count(column2, ",")]  
      
      # store add with 0 commas to zero comma datatable
      contain_commas_char0 <- contain_commas_char2 %>% copy() %>% 
        .[n_commas==0] %>% 
        rbind(contain_commas_char0, .) %>% 
        .[order(INTERNAL_ORDER_COLUMN)]
      
      # store add with 1 comma to 1 comma datatable
      contain_commas_char1 <- contain_commas_char2 %>% copy() %>% 
        .[n_commas==1] %>% 
        rbind(contain_commas_char1, .) %>% 
        .[order(INTERNAL_ORDER_COLUMN)]
      
      # keep observations with more than one commas
      contain_commas_char2 <- contain_commas_char2 %>% copy() %>% 
        .[n_commas>1] %>% 
        .[order(INTERNAL_ORDER_COLUMN)]
      
      
      print_line %>% print()
      paste0("2.1) String column with many commas has had common titles removed & extra commas dropped.") %>%  print()
      print_line %>% print()
      
    }
    
  }
  
  # 6.2) For rows with one comma, remove titles & recompute # of commas ------
  
  if(drop_common_titles_when_one_comma){
    
    if(nrow(contain_commas_char1) >0){
      
      
      # 6.3: for rows with more than one comma ----
      contain_commas_char1 %<>%
        # drop titles 
        .[, column2 := drop_common_titles(column2, prefix = " ", suffix = " ")]  %>% 
        .[, column2 := drop_common_titles(column2, prefix = " ", suffix = "")]  %>% 
        .[, column2 := drop_common_titles(column2, prefix = "", suffix = " ")]  %>% 
        .[, column2 := drop_common_titles(column2, prefix = " ", suffix = ",")]  %>% 
        .[, column2 := drop_common_titles(column2, prefix = ",", suffix = " ")]  %>% 
        # recompute number of commas
        .[, n_commas := str_count(column2, ",")]  
      
      # store add with 0 commas to zero comma datatable
      contain_commas_char0 <- contain_commas_char1 %>% copy() %>% 
        .[n_commas==0] %>% 
        rbind(contain_commas_char0, .) %>% 
        .[order(INTERNAL_ORDER_COLUMN)]
      
      # store add with 1 comma to 1 comma datatable
      contain_commas_char1 <- contain_commas_char1 %>% copy() %>% 
        .[n_commas==1] %>% 
        .[order(INTERNAL_ORDER_COLUMN)]
      
      print_line %>% print()
      paste0("2.2) String column with one comma has had common titles removed & extra commas dropped.") %>%  print()
      print_line %>% print()
      
    }
    
  }
  
  # 6.3) for rows with just one comma, change order due to commas -----
  
  if(change_order_because_of_comma){
    if(nrow(contain_commas_char1) >0){
      
      # 6.3.1) for rows with one comma only, revert order -----
      contain_commas_char1 %<>% 
        # to avoid conflicting column names, rename column once more (then name back after operation)
        rename_columns(
          current_names = c("column"), 
          new_names = c("MAINCOLUMN") 
        ) %>% 
        # invert name order over comas
        str_invert_order_given_one_comma(
          datatable = ., column = "column2") %>% 
        # rename back
        rename_columns(
          current_names = c("MAINCOLUMN"), 
          new_names = c("column") 
        ) 
      
      print("Notice!! The warning message for: (str_invert_order_given_one_comma) is addressed in the code!")
      
      print_line %>% print()
      paste0(
        "3) The order of elements with one comma in (", column, ") has been inverted.",
        " E.g. [Lname, Fname] becomes [Fname Lname]. An indicator n_commas let's us know the number of commas.", 
        " If an element contains multiple commas, nothing is done.") %>% 
        print()
      print_line %>% print()
      
      
    }
    
    
  }
  
  # 6.4)  rbind into three datatables depending on number of commas -----
  
  print_line %>% print()
  paste0(
    "4) Additional punctuation and non-characters have been cleaned &/or removed.") %>% 
    print()
  print_line %>% print()
  
  
  contain_commas_char0 %>%
    rbind(., contain_commas_char1) %>%
    rbind(., contain_commas_char2) %>%
    
    # 7) drop all punctuation & double spaces ----
  .[, column2 := str_replace_all(column2, pattern = "[[:punct:]]", replacement = " ")] %>%
    # 8) remove all double spaces ------
  # 8.1) need to rename `column`
  rename_columns(current_names = c("column"),
                 new_names = c("column_original")) %>%
    # replace double spaces
    str_replace_all_double_spaces(datatable = ., column = "column2") %>%
    # rename back
    rename_columns(current_names = c("column_original"),
                   new_names = c("column"))  %>%
    # remove some additional things -----
  .[, column2 := str_replace(column2, pattern = "\\u008e", replacement =  "Z")] %>%
    .[, column2 := str_remove(column2, pattern = "\\u0092")] %>%
    .[, column2 := str_remove(column2, pattern = "\\u0093")] %>%
    .[, column2 := str_remove(column2, pattern = "\\u0094")] %>%
    .[, column2 := str_remove(column2, pattern = "\\\177")] %>%
    .[, column2 := str_remove(column2, pattern = "\\\177")] %>%
    .[, column2 := str_replace(column2, pattern = "\u009a", replacement = "S")]  %>%
    .[, column2 := str_remove(column2, pattern = "\\^")] %>%
    .[, column2 := str_remove(column2, pattern = "\\´")] %>%
    .[, column2 := str_remove(column2, pattern = "\\ʿ")] %>%
    .[, column2 := str_remove(column2, pattern = "\\`")] %>%
    .[, column2 := str_remove(column2, pattern = "\\+")] %>%
    .[, column2 := str_remove(column2, pattern = "\\>")] %>%
    .[, column2 := str_remove(column2, pattern = "\\<")] %>%
    .[, column2 := str_remove(column2, pattern = "\\^")] %>%
    .[, column2 := str_remove(column2, pattern = "³")] %>%
    .[, column2 := str_remove(column2, pattern = "/")] %>%
    .[, column2 := str_trim(column2, "both")] %>% 
    # rename finalized columns  -----
  rename_columns(datatable = ., 
                 current_names = c("column", "column2"), 
                 new_names = c(column, paste0(column, "_clean"))) %>% 
    # subset to relevant columns + new, clean column 
    .[, ..keep_these] %>% 
    return()
  
  
}

# Section 3.2: given a data.table with a character col, indicate what needs to be removed ----
str_remove_all_strings_and_single_letters <- function( # used to be drop_strings
    datatable,
    strings=c(" DE ", " DA ", " DO ", " DAS ", " DOS "),
    name_column="name", 
    drop_single_letters=T){
  
  
  # datatable <- main %>% copy() 
  # name_column <- "name"
  
  # rename for convenience
  datatable <- datatable %>% copy() %>% 
    rename_columns(datatable=.,
                   current_names=c(name_column),
                   new_names=c("name_column"))
  
  
  # create a crosswalk between the original name and the clean name
  name_dt <-  datatable %>% copy() %>%
    .[, .(name_column)] %>%
    # everything upper case
    .[, name_clean := str_to_upper(name_column)]
  
  # strings to be removed
  if(drop_single_letters){
    
    # drop individual letters
    remove_these <- LETTERS %>% paste0(" ", ., " ") %>%    
      append(strings)
    print("Dropping strings which match the 'strings' list or are single letters with spaces.")
  }else{
    # drop specifed strings only
    remove_these <- strings
    print("Dropping strings which match the 'strings' list.")
  }
  
  
  # for each string, remove it from the
  for(string in remove_these) {
    # delete later
    # string <- strings[1]
    
    x_second_sleep(x = 0.0001, additional = string)
    
    name_dt$name_clean <- name_dt %>%
      .[, .(name_clean)] %>%
      # remove all of the strings and unmatchable single letter names
      apply(.,
            2,
            str_replace_all,
            pattern = string,
            replacement = " ") %>%
      as.data.table()
    
  }
  
  out <- cbind(datatable, name_dt[, .(name_clean)])
  
  # rename for consistency
  out <- out %>% copy() %>% 
    rename_columns(datatable=.,
                   current_names=c("name_column"),
                   new_names=c(name_column))
  
  
  return(out)
  
}

# Section 3.3: split name column by a pattern -----
str_split_column_by_pattern <- function( # used to be split_names_by_pattern
    datatable, 
    str_column = "name_clean", 
    pattern=" "){ 
  
  # rename columns for convenience
  datatable <- restr_columns(datatable=datatable,
                              current_names=c(str_column),
                              new_names=c("str_column"))
  
  # split names by space
  name_dt_pre_split <- datatable %>% copy() %>%
    .[, name_count := str_count(str_column, pattern)+1]
  
  # get max number of surnames in data
  max_name_count <- name_dt_pre_split %>% .[, max(name_count)]
  
  # split by spaces
  name_dt_split <- name_dt_pre_split %>% copy() %>%
    # split names into six names at most
    .[, paste0("str_column", c(1:max_name_count)) := tstrsplit(x = str_column, pattern, fixed =
                                                                  TRUE)] 
  
  # get current column names to be renamed
  str_columnX <- paste0("str_column", c(1:max_name_count)) %>% append("str_column")
  # and what they should be
  str_columnX_out <- paste0(str_column, c(1:max_name_count)) %>% append(str_column)
  
  # rename columns for consistency
  out <- restr_columns(datatable=name_dt_split,
                        current_names=str_columnX,
                        new_names=str_columnX_out)
  
  return(out)
  
}

# Section 3.4: split column names -----
str_split_column_by_space <- function( # used to be split_names_by_space
    datatable, str_column = "name_clean"){
  
  
  out <- split_names_by_pattern(datatable=datatable, str_column = str_column, pattern = " ")
  
  return(out)
  
}

# Section 3.5: str_replace for all double spaces ------
str_replace_all_double_spaces <- function(datatable, column, replacement = " "){
  
  keep_these <- names(datatable)
  
  datatable <- datatable %>% copy() %>% 
    rename_columns(
      datatable = .,
      current_names = c(column), 
      new_names = c("column")
    ) 
  
  # check to see max number of iterations 
  max_double_spaces <- datatable %>%
    .[, num_double_spaces := str_count(string = column, "  ") ] %>% 
    .[, max(num_double_spaces)]
  
  i <- 1
  while(i<max_double_spaces+1){
    
    datatable <- datatable %>% .[, column := str_replace_all(string = column, pattern = "  ", replacement = replacement) ]
    i <- i + 1
    
  }
  
  datatable %>% copy() %>% 
    rename_columns(
      datatable = .,
      current_names = c("column"), 
      new_names = c(column)
    ) %>% 
    .[, ..keep_these] %>%  
    return()
  
}

# Section 3.6: remove anything in parentheses -----
str_remove_anything_in_parenthesis <- function(string){
  
  pattern <- paste0("\\((.*?)\\)")
  
  string %>%
    sub(pattern = pattern,
        replacement = "",
        x = .) %>% 
    return()
  
  
}

# Section 3.7: remove all dates -----
str_remove_all_dates <- function(string, date_pattern = "dd MMM yyyy" ){
  
  warning("For the date_pattern: lower case corresponds to numbers, upper case corresponds to characters, spaces are spaces.")
  
  pattern <- ""
  
  if(date_pattern == "dd MMM yyyy"){
    pattern <-  "(\\d{2}).*(\\w{3}).*(\\d{4})"}
  if(date_pattern == "d MMM yyyy"){
    pattern <-  "(\\d{1}).*(\\w{3}).*(\\d{4})"}
  if(date_pattern == "MMM dd yyyy"){
    pattern <-  "(\\w{3}).*(\\d{2}).*(\\d{4})"}
  if(date_pattern == "dd mm yyyy"){
    pattern <-  "(\\d{2}).*(\\d{2}).*(\\d{4})"}
  if(date_pattern == "dd mm yy"){
    pattern <-  "(\\d{2}).*(\\d{2}).*(\\d{2})"}
  
  
  
  string %>%
    sub(pattern = pattern,
        replacement = "",
        x = .) %>% 
    return()
  
}

# Section 3.8: remove all after and including pattern -----
str_remove_all_after_and_including_pattern <- function(string, pattern){
  
  # string <- "LEE, HSIEN LOONG, BRIGADIER   GENERAL (RES.)" 
  # pattern <- "BRIGADIER"
  
  all_after_and_including_pattern <- paste0("(", pattern, ")(.*)")
  
  string %>%
    #str_replace_all(string = ., pattern = pattern, replacement = paste0(" ", pattern, " ") ) %>% 
    sub(pattern = all_after_and_including_pattern,
        replacement = "",
        x = .) %>% 
    return()
  
}

# Section 3.9: trim white space while it still exists ------
str_trim_ws_iterate <- function(string, whitespace){
  
  while(sum(endsWith(string, whitespace))>0){
    
    string %<>% 
      trimws(., whitespace = whitespace)
    
  }
  
  string %>% return()
  
  
}

# Section 3.10: remove all trailing commas and spaces
str_remove_all_trailing_commas_and_spaces <- function(string){

  while((sum(endsWith(string, " "))>0)|(sum(endsWith(string, ","))>0)){
    
    string %<>% 
      trimws(., whitespace = " ") %>%  
      trimws(., whitespace = ",") 
    
  }
  
  string %>% return()
  
  
}

# Section 3.11: get a vector of common titles  --------
get_common_tites <- function(type = "educ_all") {
  if (type == "") {
    warning("Possible types are: (educ_all), (educ_period), (military) or (poli).")
  }
  
  # every day & educational 
  if (type == "educ_all") {
    out <- c(
      "MR",
      "MSR",
      "MS",
      "BA",
      "BSC",
      "MSC" ,
      "MBA" ,
      "MA",
      "MD",
      "PHD",
      "DR" ,
      "DOCTOR",
      "ESQUIRE",
      "Professor",
      "LLM",
      "LLB",
      "PROF",
      
      "MR." ,
      "MSR." ,
      "MS.",
      "BSC.",
      "MSC.",
      "MBA.",
      "MA." ,
      "MD." ,
      "PH.D.",
      "PHD." ,
      "DR." ,
      "LLM." ,
      "LLB.",
      "PROF.",
      "ENG."
    ) }
  
  # every day & educational 
  if (type == "educ_unambiguous") {
    out <- c(
      "DOCTOR",
      "ESQUIRE", 
      "Professor"
    ) }
  
  if (type == "educ_period") {
    out <- c(
      "MR." ,
      "MSR." ,
      "MS.",
      "BSC.",
      "MSC.",
      "MBA.",
      "MA." ,
      "MD." ,
      "PH.D.",
      "PHD." ,
      "DR." ,
      "LLM." ,
      "LLB.",
      "PROF.",
      "ENG."
    ) 
    
    
  }
  
  # military
  if (type == "military") {
    
    
    
    out <-  c(
      "Warrant Officer",
      "Technical",
      "Sergeant",
      "Sgt.",
      "Staff",
      "Senior Master Sergeant",
      "Senior Airman",
      "Second Lieutenant",
      "Second Lt.",
      "Master Sergeant",
      "Master Sgt.",
      "Major General",
      "Maj. Gen.",
      "Major", 
      "Maj.",
      "lieutenant general",
      "LIEUTENANT GENERAL",
      "Lt. Gen.",
      "lieutenant colonel",
      "Lt. Col.",
      "General", 
      "Gen.",
      "First Sergeant",
      "1st Sgt.",
      "first lieutenant", 
      "1st Lt.",
      "Command Chief Master Sergeant", 
      "Command Chief Master Sgt.",
      "Colonel", 
      "Col.",
      "chief warrant officer" ,
      "Chief Master Sergeant",
      "Chief Sgt.",
      "of the Air Force",
      "Captain",
      "Capt.",
      "brigadier general",
      "Brig. Gen.",
      "Airman Basic",
      "Airman",
      "sergeant major",
      "of the Army",
      "Sgt. Maj." ,
      "command sergeant major",
      "Command Sgt. Maj.",
      "sergeant major",
      "Sgt. Maj.",
      "first sergeant",
      "1st Sgt.",
      "master sergeant",
      "Master Sgt.",
      "sergeant first class",
      "Sgt. 1st Class",
      "staff sergeant",
      "Staff Sgt.",
      "Sergeant", 
      "Sgt.",
      "Corporal", "Cpl.",
      "Specialist", "Spc.",
      "private first class",
      "Admiral",
      "Adm.",
      "vice admiral",
      "Vice Adm.",
      "rear admiral",
      "Rear Adm.",
      "commander",
      "Cmdr.",
      "lieutenant commander", 	
      "Lt. Cmdr.",
      "lieutenant", 
      "Lt.",
      "lieutenant junior grade", 
      "Lt. j.g.",
      "Ensign"	,
      "chief warrant officer",
      "master chief petty officer",
      "Senior Chief Petty Officer",
      "chief petty officer",
      "petty officer", 
      "first class",
      "1st Class",
      "second class", 
      "2nd Class",
      "third class", 
      "3rd Class",
      "Seaman",
      "seaman apprentice",
      "Seaman Recruit",
      "sergeant Major",
      "Sgt. Maj.",
      "Master Gunnery Sergeant",
      "Master Sgt. Maj.",
      "first sergeant",
      "1st Sgt.",
      "master sergeant",
      "Master Sgt.",
      "Gunnery Sergeant",
      "Gunnery Sgt.",
      "staff sergeant",
      "Staff Sgt.",
      "Sergeant", 
      "Sgt.",
      "Corporal", 
      "Cpl.",
      "Lance Corporal",
      "Lance Cpl.",
      "Pfc.",
      "Private",	
      "Pvt.",
      "GEN.",
      "MAJ.",
      "(RET.)",
      "(RETIRED)",
      "COL.",
      "LT.",
      "BRIG.",
      "RADM.",
      "DIV.",
      "FD.",
      "MAR.",
      "CAPT.",
      "ADM.",
      "CORPS",
      "AIR CHIEF",
      "OF THE ARMY", 
      "OF THE NAVY",
      "of the Marine Corp",
      "Reserve",
      "Reservist",
      "Res."
      
    ) 
    
  }
  
  # political & religious
  if (type == "poli") {
    out <-  c(
      "SIR",
      "LORD",
      "ACTING",
      "PRINCESS",
      "PRINCE",
      "ARCHBISHOP",
      "VICE",
      "PRESIDENT",
      "CHAIRMAN",
      "MIN.",
      "CHIEF",
      "AYATOLLAH",
      "CROWN",
      "HOJJAT OL-ESLAM",
      "REVEREND",
      "BISHOP"
    )
    
    
  }
  
  if (type == "oth") {
    
    out <- c(
      "DIRECTOR"
    )
    
  }
  
  out <- out %>% 
    as.data.table() %>% 
    .[, `.`:=str_to_upper(`.`)] %>% 
    .[, .N, `.`] %>% 
    .[, nchar := nchar(`.`)] %>% 
    .[order(-nchar)] %>% 
    .[, `.`] %>% return()
  
  
}

# Section 3.12.1: drop common titles from strings: helper functions -------
drop_common_titles <- function(vector, prefix="", suffix=""){
  
  # load common titles 
  common_titles <- get_common_tites(type = "educ_period") %>% 
    append(
      ., get_common_tites(type = "educ_unambiguous")) %>% 
    append(
      ., get_common_tites(type = "military")) %>%
    append(
      ., get_common_tites(type = "oth")) %>% 
    append(
      ., get_common_tites(type = "poli"))
  
  
  for(title in common_titles){
    
    vector <- str_remove_all(string = vector, paste0(prefix, title, suffix)) 
    
  }
  
  vector %>% 
    # DROP double commas again
    str_replace_all(., ",,", ",")   %>% 
    str_replace_all(., ", ,", ",")   %>% 
    return()
}

# Section 3.12.2: drop common titles from strings: helper functions -------
drop_common_titles_startswith <- function(vector, prefix="", suffix=""){
  
  # load common titles 
  common_titles <- get_common_tites(type = "educ_period") %>% 
    append(
      ., get_common_tites(type = "educ_unambiguous")) %>% 
    
    append(
      ., get_common_tites(type = "military")) %>% 
    append(
      ., get_common_tites(type = "poli"))
  
  # prepare vector as datatable
  vector_dt <- vector %>% data.table(vector=.) %>% 
    .[, internal_order := 1:.N] 
  
  # for each title, remove it from the data 
  for(title in common_titles){
    
    title <- paste0(prefix, title, suffix)
    
    vector_dt[startsWith(vector, title),  vector := str_remove_all(vector, title)]
    
  }
  
  vector_dt %>% 
    .[order(internal_order)] %>% 
    .[, vector] %>% 
    return()
  
}

# Section 3.12.3: drop common titles from strings: helper functions -------
drop_common_titles_endswith <- function(vector, prefix="", suffix=""){
  
  # load common titles 
  common_titles <- get_common_tites(type = "educ_period") %>% 
    append(
      ., get_common_tites(type = "educ_unambiguous")) %>% 
    
    append(
      ., get_common_tites(type = "military")) %>% 
    append(
      ., get_common_tites(type = "poli"))
  
  # prepare vector as datatable
  vector_dt <- vector %>% data.table(vector=.) %>% 
    .[, internal_order := 1:.N] 
  
  # for each title, remove it from the data 
  for(title in common_titles){
    
    title <- paste0(prefix, title, suffix)
    
    vector_dt[endsWith(vector, title),  vector := str_remove_all(vector, title)]
    
  }
  
  vector_dt %>% 
    .[order(internal_order)] %>% 
    .[, vector] %>% 
    return()
  
}

# Section 3.13:  check if a string contains non-letters  -----
str_check_for_nonletters <- function(vector){ # used to be check_for_nonletters
  
  vector %>% 
    str_replace_all(string = ., pattern = " ", replacement = "") %>% 
    str_detect(., regex("\\W")) %>% 
    return()
  
}

# Section 3.14: generate a random string with letters & numbers -----------------
str_generate_random_string <- function(size){ # used to be: generate_random_string  
  samp<-c(0:9,letters,LETTERS)
  paste(sample(samp,size = size),collapse="") %>% return()
}

# Section 3.15: generate a random string with letters & numbers that is not present in a vector -----------------
str_generate_random_string_not_in_vector <- function(size, vector){
  
  # generate random string
  random_string <- generate_random_string(size = size)
  
  # detect if it is in the vector
  check <- str_detect(string = vector, pattern = random_string) %>% sum()
  
  # as size increases, it becomes exponentially more unlikely to appear 
  if(check==1){
    i <- 1
    while (check==1) {
      print(i)  
      # generate random string
      random_string <- generate_random_string(size = size)
      
      # detect if it is in the vector
      check <- str_detect(string = vector, pattern = random_string) %>% sum()
      i <- i+1
    }
  }
  
  return(random_string)
  
}  

# Section 3.16: invert order of column if there is one comma in the text -------
str_invert_order_given_one_comma <- function(datatable, column, suppress=F){
  
  # delete later
  # datatable <- contain_commas_char1 %>% copy()
  # column <- "column2"
  
  if(suppress){
    message("(No issues necessarily, just a message!)
Reminder: Only use this funciton (str_invert_order_given_one_comma) if you are sure there is at most one comma in each element of the specified column.")
  } 
  
  datatable %>% copy() %>% 
    # rename column for practicality
    rename_columns(
      datatable = .,
      current_names = c(column),
      new_names = c("column")) %>%
    # split the column into two by the comma
    .[, c("column__1", "column__2") := tstrsplit(x = column, ",", fixed=T)]  %>% 
    .[is.na(column__1), column__1 := " " ]  %>% 
    .[is.na(column__2), column__2 := " " ]  %>% 
    # combine columns 
    .[, column := paste(column__2, column__1)] %>% 
    .[, `:=`(column__1 = NULL, column__2 = NULL)] %>% 
    # rename column for practicality
    rename_columns(
      datatable = .,
      current_names = c("column"),
      new_names = c(column)) %>% 
    return()
  
}


# get_digits_after_last_underscore ----
str_extract_digits_after_last_underscore <- function(x){ # get_digits_after_last_underscore
  
  # .*_(\d+)
  
  str_extract(string = x, pattern = "(\\d+)$") %>% 
    as.numeric(.) %>% 
    return(.)
  
}

################################################################################
# Section 4: environment and time  #############################################
################################################################################

# Section 4.1: print obj size ----
print_obj_size <- function(message, x){
  
  pacman::p_load(pryr)
  
  # get obj size in GB
  x <- x %>% object_size() %>%
    format(., units = "GB") %>% 
    as.numeric(.)
  
  # print it
  (x / (1024**3)) %>%
    paste(message, ., " GB") %>%
    print()
  
}

# Section 4.2: reduce RAM consumption ----
remove_these <- function(remove_these){
  
  remove_these <- remove_these %>% append(c("remove_these"))
  rm(list =remove_these ) 
  gc()  
  
}


# Section 4.3: get time stamp ----
get_timestamp <- function(){
  out <- Sys.time() %>% 
    str_replace_all(., ":", "-") %>% 
    str_replace_all(., " ", "_") %>%
    substr(., 1, 20) %>%
    paste0("_", ., "_")
  return(out)
}

# Section 4.4: define sleep function to help prevent running the code accidentally -----
x_second_sleep <- function(x, additional){
  
  Sys.time() %>% 
    paste0("Current date/time in Chicago: ", .) %>% 
    print()
  
  print(paste0("Going to sleep for ", x ," seconds. Additional message: ", additional))
  
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
  
}






################################################################################
# Section 5: Internal operations ############################################### 
################################################################################

# generate an internal order column -------
generate_internal_order_column <- function(datatable){
  
  # 1) check if 'INTERNAL_ORDER_COLUMN' exists in the data (it honestly shouldn't lol), change the column name to something random
  # 1.1) If it does exist, just rename the variable in the data-set something that doesn't exist
  if("INTERNAL_ORDER_COLUMN" %in% names(datatable)){
    
    # get column names   
    original_names <- names(datatable)
    
    # generate a random suffix that doesn`t even exist in the orignal names 
    random_suffix <- generate_random_string_not_in_vector(size = 4, vector = original_names)
    
    # new name of old internal order column name 
    old_internal_order_column_new_name <- paste0("INTERNAL_ORDER_COLUMN", "_OLD_", random_suffix)
    
    datatable <- datatable  %>% copy() %>% 
      rename_columns(datatable=., 
                     current_names = c("INTERNAL_ORDER_COLUMN"), 
                     new_names = c(old_internal_order_column_new_name))
    
  }
  
  # 2) 
  
  datatable  %>% copy() %>% .[, INTERNAL_ORDER_COLUMN := 1:.N] %>% return()
  
}

# copy column  ----
copy_column <- function(datatable, column_name, copy_column_name) {
  #   delete later
  # datatable <- surname_fyi %>% copy()
  # column_name <- surname_is_local
  
  # this is what we define as local; therefore its perfectly colinear
  datatable$column_name <-   datatable %>%
    .[, ..column_name]
  
  # rename column
  datatable <- datatable %>%
    rename_columns(
      datatable = .,
      current_names = c("column_name"),
      new_names = c(column_name)
    )
  
  return(datatable)
  
}
