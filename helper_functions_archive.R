# helper funcitons archive 

# Section 3.1: clean names  -----
standardize_name_column <- function(
    datatable, 
    column, 
    drop_common_titles_when_one_comma,
    drop_common_titles_when_more_than_one_comma,
    change_order_because_of_comma){
  
  # DELETE LATER
  # datatable <- str_create_name_column()
  # column <- "name"
  # change_order_because_of_comma <- T
  # drop_common_titles_when_one_comma <- T
  # drop_common_titles_when_more_than_one_comma <- T
  
  paste0("Standardizing name column: (", column ,")") %>%
    message_with_lines()
  
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
  .[, column2 := stringi::stri_trans_general(column, 'Any-Latin') ] %>% 
    .[, column2 := stringi::stri_trans_general(column2, 'Any-Latn') ] %>% 
    .[, column2 := stringi::stri_trans_general(column2, 'Latin-ASCII') ] %>% 
    # 5) everything upper case ----
  .[, column2 := stringr::str_to_upper(column2)] %>% 
    # 6) drop instances of multiple commas together
    .[, column2 := stringr::str_replace(column2, pattern = ",,", replacement = ",")]  %>% 
    # indicate number of commas 
    .[, n_commas := stringr::str_count(column2, ",")]  %>% 
    .[n_commas>0, column2 := str_remove_all_trailing_commas_and_spaces(column2) ] %>% 
    .[, n_commas := stringr::str_count(column2, ",")] %>% 
    .[, column2 := stringr::str_remove_all(column2, "\\n")]
  
  paste0("1) String column has been transliterated into ASCII, upper case characters.") %>%  
    message_with_lines()
  
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
        .[, column2 := str_remove_all_common_titles(column2, prefix = " ", suffix = " ")]  %>% 
        .[, column2 := str_remove_all_common_titles(column2, prefix = " ", suffix = ",")]  %>% 
        .[, column2 := str_remove_all_common_titles(column2, prefix = ",", suffix = " ")]  %>% 
        .[, column2 := str_remove_all_common_titles_startswith(column2, prefix = "", suffix = " ")]  %>% 
        .[, column2 := str_remove_all_common_titles_endswith(column2, prefix = ",", suffix = "")]  %>% 
        # drop some pretty specific titles 
        .[, column2 := str_remove_all_common_titles(column2, specific = get_common_tites(type="unambiguous"))]  %>%
        .[, column2 := str_trim_ws_iterate(column2)]  %>% 
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
      
      
      print_line() # %>% print()
      paste0("2.1) String column with many commas has had common titles removed & extra commas dropped.") %>%  print()
      print_line() # %>% print()
      
    }
    
  }
  
  # 6.2) For rows with one comma, remove titles & recompute # of commas ------
  
  if(drop_common_titles_when_one_comma){
    
    if(nrow(contain_commas_char1) >0){
      
      # 6.3: for rows with more than one comma ----
      contain_commas_char1 %<>%
        # drop titles 
        .[, column2 := str_remove_all_common_titles(column2, prefix = " ", suffix = " ")]  %>% 
        .[, column2 := str_remove_all_common_titles(column2, prefix = " ", suffix = ",")]  %>% 
        .[, column2 := str_remove_all_common_titles(column2, prefix = ",", suffix = " ")]  %>% 
        .[, column2 := str_remove_all_common_titles_startswith(column2, prefix = "", suffix = " ")]  %>% 
        .[, column2 := str_remove_all_common_titles_endswith(column2, prefix = ",", suffix = "")]  %>% 
        # drop some pretty specific titles 
        .[, column2 := str_remove_all_common_titles(column2, specific = get_common_tites(type="unambiguous"))]  %>% 
        .[, column2 := str_trim_ws_iterate(column2)]  %>%
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
      
      print_line() # %>% print()
      paste0("2.2) String column with one comma has had common titles removed & extra commas dropped.") %>%  print()
      print_line() # %>% print()
      
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
      
      print_line() # %>% print()
      paste0(
        "3) The order of elements with one comma in (", column, ") has been inverted.",
        " E.g. [Lname, Fname] becomes [Fname Lname]. An indicator n_commas let's us know the number of commas.", 
        " If an element contains multiple commas, nothing is done.") %>% 
        print()
      print_line() # %>% print()
      
      
    }
    
    
  }
  
  # 6.4)  rbind into three datatables depending on number of commas -----
  
  
  paste0(
    "4) Additional punctuation and non-characters have been cleaned &/or removed.") %>% 
    message_with_lines()
  
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
  .[str_detect(column2, "\\u008e"), column2 := stringr::str_replace(column2, pattern = "\\u008e", replacement =  "Z")] %>%
    .[str_detect(column2, "\\u0092"), column2 := stringr::str_remove(column2, pattern = "\\u0092")] %>%
    .[str_detect(column2, "\\u0093"), column2 := stringr::str_remove(column2, pattern = "\\u0093")] %>%
    .[str_detect(column2, "\\u0094"), column2 := stringr::str_remove(column2, pattern = "\\u0094")] %>%
    .[str_detect(column2, "\\\177"), column2 := stringr::str_remove(column2, pattern = "\\\177")] %>%
    .[str_detect(column2, "\\\177"), column2 := stringr::str_remove(column2, pattern = "\\\177")] %>%
    .[str_detect(column2, "\u009a"), column2 := stringr::str_replace(column2, pattern = "\u009a", replacement = "S")]  %>%
    .[str_detect(column2,  "\\^"), column2 := stringr::str_remove(column2, pattern = "\\^")] %>%
    .[str_detect(column2, "\\´"), column2 := stringr::str_remove(column2, pattern = "\\´")] %>%     .[str_detect(column2, "\\ʿ"), column2 := stringr::str_remove(column2, pattern = "\\ʿ")] %>%
    .[str_detect(column2, "\\`"), column2 := stringr::str_remove(column2, pattern = "\\`")] %>%
    .[str_detect(column2, "\\+"), column2 := stringr::str_remove(column2, pattern = "\\+")] %>%
    .[str_detect(column2, "\\>"), column2 := stringr::str_remove(column2, pattern = "\\>")] %>%
    .[str_detect(column2, "\\<"), column2 := stringr::str_remove(column2, pattern = "\\<")] %>%
    .[str_detect(column2, "\\^"), column2 := stringr::str_remove(column2, pattern = "\\^")] %>%
    .[str_detect(column2, "³"), column2 := stringr::str_remove(column2, pattern = "³")] %>%
    .[str_detect(column2, "/"), column2 := stringr::str_remove(column2, pattern = "/")] %>%
    .[, column2 := str_trim_ws_iterate(string = column2, whitespace = " ")] %>% 
    # rename finalized columns  -----
  rename_columns(datatable = ., 
                 current_names = c("column", "column2"), 
                 new_names = c(column, paste0(column, "_clean"))) %>% 
    # subset to relevant columns + new, clean column 
    .[, ..keep_these] %>% 
    return()
  
  
}