
################################################################################
# Section 1: Dummies ########################################################### 
################################################################################

# Section 1.1: updated function has output just like dummy_cols; but is A LOT FASTER! :)  ----
dummy_cols_for_big_data <- function(datatable, 
                                    index,
                                    col,
                                    vector_of_dummies,
                                    ignore_na=F){
  
  
  # delete later - example
  # datatable <- owners %>% copy()
  # index <- index_fpyi
  # col <- "nationality"
  # vector_of_dummies <- nationality %>% .[, nationality]
  
  # datatable <- owners_fy;
  # index <- index_fyi;
  # col <- characteristic;
  # vector_of_dummies <- unique_characteristics;
  # ignore_na <- T
  
  # make sure elements are unique
  vector_of_dummies <- unique(vector_of_dummies) %>% sort()
  
  # convert into a vector must be a vector
  column <- c(col) 
  
  # set up subset of data.table
  subset <-  append(index, column)
  datatable_subset <- datatable %>% copy() %>% .[, ..subset]
  names(datatable_subset) <- index %>% append(c("column"))
  
  paste0("creating dummies for column: ", col, "; for the following elements: ") %>%  print()
  
  # for each unique element in the vector of dummy elements
  for(element in vector_of_dummies){
    
    print(element)
    
    # temporary data.table just with column 
    datatable_subset_tmp <- datatable_subset %>% copy()
    # create indicator
    datatable_subset_tmp[,indicator := 0] 
    datatable_subset_tmp[column==element, indicator := 1] 
    
    just_element <- paste0(col, "_", element) %>% c()
    
    names(datatable_subset_tmp) <- index %>% append(., c('column')) %>%  append(just_element)
    
    if(element==vector_of_dummies[1]){
      datatable_subset_out <- datatable_subset_tmp %>% copy()
    }else{
      datatable_subset_out <- datatable_subset_tmp %>% copy() %>%
        .[, ..just_element] %>% 
        cbind(datatable_subset_out,. )
    }
    
  }
  
  if(!ignore_na){
    # create a missing column 
    datatable_subset_out[, NA_MISSING := 0][is.na(column), NA_MISSING := 1]
    vars_names <- names(datatable_subset_out)
    names(datatable_subset_out)[length(vars_names)] <- paste0(col, "_NA_MISSING")
  }
  
  # rename column & dummies in the same way as the original dummy cols
  datatable_subset_out[, column := NULL]
  
  # merge with input 
  datatable_out <- datatable %>%  merge(x = ., y = datatable_subset_out, by = index, all=T)
  
  # output
  return(datatable_out)
  
  print("Notice: all dummy columns are output; even those with all zeros! :) ")
  
}

# Section 1.2: create firm-year indexed panel of `at least one` indicators  ----
create_fy_indicator_alo <- function(datatable,
                                    index_fy,
                                    index_fyi,
                                    vars_owner_characteristics){
  
  # delete later
  # datatable <- copy(owners_fyi)
  # index_fy<-index_fy
  # index_fyi<-index_fyi
  # vars_owner_characteristics <- vars_characteristics
  
  
  # section 0: initialize dummies data-set 
  owners_fyi_dummies <- datatable %>% copy()
  
  # section 1: create dummies for all characteristics, for all owners ---- 
  print("1/3: create dummies for all characteristics, for all owners")
  # for all owners; for each characteristic, create full set of indicators 
  for(characteristic in vars_owner_characteristics){
    
    print(characteristic)  
    
    # place variable into vector for subsetting
    vars_characteristic <- c(characteristic)
    
    # get all of the unique characteristics
    unique_characteristics <- owners_fyi_dummies %>% .[, ..vars_characteristic] %>% unique() %>% unlist
    
    owners_fyi_dummies <- dummy_cols_for_big_data(
      datatable = owners_fyi_dummies,
      index = index_fyi, 
      col = characteristic, 
      vector_of_dummies = unique_characteristics,
      ignore_na = F)
    
  } 
  
  # section 2: compute totals for each firmid-year ---- 
  print("2/3: aggregate (sum) dummies at the firm-year level")
  
  # section 2.1: keep dummies and firmid-year identifier ------
  vars_owners_fyi_dummies <- get_new_column_names(
    old_datatable = datatable,
    new_datatable = owners_fyi_dummies)
  
  vars_keep_these <- append(index_fy, vars_owners_fyi_dummies)
  
  # section 2.2: compute totals for each firmid-year ---- 
  
  # owner sum variables
  vars_firms_fy_sum <- paste0("fy_sum_", vars_owners_fyi_dummies)
  
  firms_fy_sum <- owners_fyi_dummies %>% 
    # reduce data-set
    .[, ..vars_keep_these] %>% 
    # aggregate over firm-year index
    .[, lapply(.SD, sum, na.rm=T), index_fy] %>% 
    # rename columns to indicate they are sums
    rename_columns(
      datatable = .,
      current_names = vars_owners_fyi_dummies,
      new_names = vars_firms_fy_sum)
  
  # section 3: create indicators based on totals for each firmid-year ---- 
  print("3/3: create 'At Least One' indicators at the firm-year level")
  
  # owner indicator variable names
  vars_firms_fy_indicator_alo <- paste0("fy_indicator_alo_", vars_owners_fyi_dummies)
  
  # section 2.3.1: create indicators based on totals for each firmid-year ---- 
  firms_fy_indicator_alo <- firms_fy_sum %>% copy() %>%
    # isolate sum variables
    .[, ..vars_firms_fy_sum] %>% 
    # determine if each is greater than zero (greater than or equal to one)
    .[, lapply(.SD, is_greater_than_zero)] %>% 
    # append index
    cbind(firms_fy_sum[, ..index_fy], .) %>% 
    # rename columns to indicate they are indicators 
    rename_columns(datatable = ., 
                   current_names = vars_firms_fy_sum, 
                   new_names = vars_firms_fy_indicator_alo)
  
  # section 4:  export
  return(firms_fy_indicator_alo)
  
}

# Section 1.3: create firm-year-individual indexed panel of indicators  ----
create_fyi_indicator <- function(datatable,
                                 index_fy,
                                 index_fyi,
                                 vars_individual_characteristics){
  
  # delete later
  # datatable <- copy(workers_fyi)
  # index_fy<-index_fy
  # index_fyi<-index_fyi
  # vars_individual_characteristics <- vars_characteristics
  
  
  # section 0: initialize dummies data-set 
  individual_fyi_dummies <- datatable %>% copy()
  
  # section 1: create dummies for all characteristics, for all individual ---- 
  print("1/1: create dummies for all characteristics, for all individuals")
  # for all owners; for each characteristic, create full set of indicators 
  for(characteristic in vars_individual_characteristics){
    
    print(characteristic)  
    
    # place variable into vector for subsetting
    vars_characteristic <- c(characteristic)
    
    # get all of the unique characteristics
    unique_characteristics <- individual_fyi_dummies %>% .[, ..vars_characteristic] %>% unique() %>% unlist
    
    individual_fyi_dummies <- dummy_cols_for_big_data(
      datatable = individual_fyi_dummies,
      index = index_fyi, 
      col = characteristic, 
      vector_of_dummies = unique_characteristics,
      ignore_na = F)
    
  } 
  
  # section 2.1: keep dummies and firmid-year identifier ------
  vars_individual_fyi_dummies <- get_new_column_names(
    old_datatable = datatable,
    new_datatable = individual_fyi_dummies)
  
  vars_keep_these <- append(index_fyi, vars_individual_fyi_dummies)
  
  individual_fyi_dummies <- individual_fyi_dummies %>% 
    .[, ..vars_keep_these]
  
  # rename
  individual_fyi_dummies <- rename_columns(datatable = individual_fyi_dummies, 
                                           current_names = vars_individual_fyi_dummies, 
                                           new_names = paste0("i_", vars_individual_fyi_dummies))
  
  # section 4:  export
  return(individual_fyi_dummies)
  
}



# Section 1.4: convert all of the variables in a data-set to boolean excpet index ----
convert_to_boolean_gtz <- function(datatable, index, prefix="bool_"){
  
  print("converting numeric variables greater than zero to boolean")  
  
  # all columns except index will be converted
  convert_these <- names(datatable) %>%  .[!(.%in% index)]
  new_convert_these <- paste0(prefix, convert_these)
  
  out <- datatable %>% copy() %>% 
    .[, ..convert_these] %>% 
    .[, lapply(.SD, is_greater_than_zero)] %>%
    cbind(datatable[, ..index], .) %>% 
    rename_columns(
      .,
      current_names = convert_these,
      new_names = new_convert_these)
  
  return(out)
  
}

# Section 1.5: create create_alo_column_is_not_local function to indicate that at least one surname is not local (our paper) ----
create_alo_column_is_not_local <- function(datatable, index, not_local_name, omit_these_variables){
  
  # do not sum across the index nor the local origin variable
  omit_from_sum <-c(omit_these_variables) %>%  append(index, .)
  
  datatable <- datatable %>%
    # sum across the indicators for whether there is 
    create_variable_row_sum_greater_than_zero(
      datatable = .,
      variables_to_omit = omit_from_sum,
      column_name = not_local_name
    )
  
  return(datatable)
  
}

# Section 1.6: create a new column which indicates whether both columns are equal to one another ----
are_both_dummies_one <- function(datatable, dummyA, dummyB, new_dummy_name){
  
  # datatable = dyads_dt_light_all; 
  # dummyA = worker_dummy; 
  # dummyB = firm_dummy; 
  # new_dummy_name = var_share_column
  
  keep1 <- c(dummyA, dummyB) #%>% append(index, .)
  keep2 <- c(new_dummy_name) #%>% append(index,. )
  
  out <- datatable %>% 
    # subset columns 
    .[, ..keep1] %>% 
    # rename columns
    rename_columns(datatable = .,
                   current_names = c(dummyA, dummyB), 
                   new_names = c("dummyA", "dummyB")) %>% 
    # create new column for whether dummies are both equal to 1
    .[, new_dummy := ((dummyA==dummyB)&(dummyA==1))*1] %>% 
    # rename columns
    rename_columns(datatable = .,
                   current_names = c("new_dummy"), 
                   new_names = c(new_dummy_name)) %>% 
    .[, ..keep2]
  
  return(out)
  
}


# create variable that sums across rows, except specific rows ----
create_variable_row_sum_greater_than_zero <- function(datatable,
                                                      variables_to_omit,
                                                      column_name){
  
  
  # get columns which are not either the key or the specified column
  vars_to_sum <- datatable %>% names() %>% .[!(.%in%variables_to_omit)] #%>% .[!(.%in%index)]  
  
  series <- datatable %>% copy() %>%  
    .[,..vars_to_sum] %>% 
    apply(., 1, sum, na.rm=T) %>% 
    data.table()  %>% 
    dplyr::rename(., column_name_sum =".") %>% 
    .[, column_name_greater_than_zero := is_greater_than_zero(column_name_sum)] %>% 
    .[, .(column_name_greater_than_zero)] 
  
  names(series) <- column_name
  
  datatable <- datatable %>% copy() %>% 
    cbind(., series)
  
  return(datatable)
}

# create boolean using vector of categories -----
create_boolean_using_vector_of_categories <- 
  function(datatable, categories, variable){
    
    datatable <- datatable %>% copy() %>% 
      rename_columns(
        datatable = .,
        current_names = c(variable),
        new_names = c("variable"))
    
    datatable %>% copy() %>% 
      .[, variable] %>% 
      lapply(X = categories, FUN = str_detect, string = .) %>% 
      as.data.table() %>% 
      rename_columns(
        datatable = ., 
        current_names = names(.), 
        new_names = categories) %>% 
      return()
    
  }



# create dummies using vector of categories -----
create_dummy_if_any_category_present <- 
  function(datatable, categories, variable, indicator_name="contains_at_least_one_category"){
    
    
    datatable_original <- datatable %>% copy()
    
    # create a matrix indicating the presense of each category
    create_boolean_using_vector_of_categories(
      datatable=datatable,
      categories = categories, 
      variable=variable) %>% 
      # sum across all indicators 
      apply(X = .,MARGIN = 1, FUN =  sum, na.rm=T) %>% 
      #wrangle
      as.data.table() %>% 
      rename_columns(
        datatable = .,
        current_names = c("."), 
        new_names = c("SUM")) %>%
      .[, SUM := (SUM>0)*1] %>%
      # rename 
      rename_columns(
        datatable = .,
        current_names = c("SUM"), 
        new_names = c(indicator_name)) %>% 
      # bind back to original dataset
      cbind(datatable_original, .) %>% 
      # output
      return()
    
    
  }


# indicate years present: spit out matrix with the years available -----
create_dummy_matrix_to_indicate_years_present <- function(
    datatable, 
    year_min, 
    year_max, 
    variable_start_str, 
    variable_end_str){
  
  # delete later
  # parameters to be defined
  # datatable <- peppercat2 %>% copy()
  # year_min <- 1990
  # year_max <- 2022+3
  # variable_start_str = "year_start_str"
  # variable_end_str = "year_end_assumed_str"  
  
  message_with_lines("Notice: start & end year variables must be characters!")
  
  # define order
  year_order <- (c(year_min:year_max)-year_min)+1
  year_order_rev <- year_order %>% rev()
  available_years <- c(year_min:(year_max)) %>% as.character()
  available_years_rev <- available_years %>% rev()
  
  # Section 2: create an indicator for each year -----
  
  # Section 2.1: start year -----
  year_start_mat <- datatable %>% copy() %>%
    create_boolean_using_vector_of_categories(
      datatable = ., 
      # correct order
      categories = available_years,
      variable = variable_start_str
    ) %>% 
    as.matrix() *1
  
  # Section 2.2: end year -----
  year_end_mat_rev <- datatable %>% copy() %>%
    create_boolean_using_vector_of_categories(
      datatable = ., 
      # reversed order
      categories = available_years_rev,
      variable = variable_end_str
    ) %>% 
    as.matrix()  *1
  
  # Section 3: sum across years -----
  for(j in 1:(ncol(year_start_mat)-1)){
    print(j)
    year_start_mat[,j+1] <- year_start_mat[,j+1] + year_start_mat[,j]
    year_end_mat_rev[,j+1] <- year_end_mat_rev[,j+1] + year_end_mat_rev[,j]
  }
  
  # reverse end matrix order to correct order
  year_end_mat <- year_end_mat_rev[, year_order_rev]
  
  # their intersection sums to 2 
  years_present <- ((year_start_mat + year_end_mat) == 2)*1 
  
  return(years_present)
  
}



################################################################################
# Section 2: Merging ###########################################################
################################################################################

# Section 2.1: this function takes in a data-table indexed by an ID variable & outputs two data.tables as a list: (used to be unique_by_column_with_threshold_ids) -----
## once which has more than 'threshold' id's per 'by_column' & another with the wide data & 'threshold columns'
reshape_unique_bycol_with_t_id_cols <- function(datatable, id, by_column, threshold, dir_save, wide_summary_name, unique_by_column_id_prefix = "i", fill=""){
  
  # DELETE LATER
  # datatable = datatable_y_alt %>% copy()
  # id = "id_y"
  # by_column = "by_column"
  # threshold = threshold1
  # dir_save = dir_save
  # wide_summary_name =  paste0("bvd", "_", COUNTRY, "_") 
  # unique_by_column_id_prefix = "y", 
  # fill = ""
  
  var_name <- id
  
  # section 1: preliminary cleaning of data 
  datatable_alt <- datatable %>%  copy() %>% 
    rename_columns(
      datatable = .,
      current_names = c(id, by_column), 
      new_names = c("id", "by_column") ) %>% 
    # clean by_column by removing empty strings
    .[by_column!=""] %>% 
    # create a unique id
    .[, unique_by_column_id := .GRP, by_column] %>% 
    .[, unique_by_column_id := paste0(unique_by_column_id_prefix, unique_by_column_id)] %>% 
    .[, per_by_column := 1:.N ,by_column] 
  
  # export this 
  filename <- paste0(dir_save, wide_summary_name, "wide_summary.csv")
  paste0("Saving wide summary in: ", filename) %>% print()
  datatable_alt %>% copy() %>% .[, .N, .(per_by_column)] %>% 
    fwrite(., filename)
  
  # display 
  paste0("Displaying CMF up til threshold for id: (",id, ") and by_column: (", by_column, ")") %>% print()
  stats <- datatable_alt %>% copy() %>% .[, .N, .(per_by_column)] %>% 
    .[, cumsum := cumsum(N)] %>%
    .[, cumsum_perc := cumsum/sum(N)] %>%
    .[1:threshold] 
  
  stats %>% print() # 98% of cases occur below 10
  
  # cases where we have more than 10 pepids, are left for later;  -----
  # identify names with many linked IDs
  datatable_alt_try_later <- datatable_alt %>% copy() %>% 
    .[, max_per_by_column := max(per_by_column),by_column] %>% 
    .[max_per_by_column>threshold]
  
  datatable_wide <- datatable_alt %>% copy() %>% 
    .[, max_per_by_column := max(per_by_column),by_column] %>% 
    .[max_per_by_column<=threshold] %>% 
    .[, .(id, by_column, unique_by_column_id, per_by_column)] %>% 
    .[, per_by_column := paste0(var_name, "_", per_by_column)] %>% 
    dcast(., by_column + unique_by_column_id ~per_by_column, value.var = "id", fill = fill)
  
  out <- list()
  out$stats <- stats %>% copy()
  out$wide <- datatable_wide %>% copy()
  out$try_later <- datatable_alt_try_later %>% copy()
  
  return(out)
  
}


# Section 2.2: split pattern into threshold+1 columns then compute the pairwise combination of these threshold+1 columns ----------
split_pattern_into_tplus1_cols_pairwise_combinations <- function(
    datatable, 
    id,string,
    pattern, 
    threshold){
  
  # section 1: rename columns & comupute # of occurances of pattern ----
  datatable <- datatable %>% copy() %>% 
    rename_columns(
      datatable = ., 
      current_names = c(id, string),
      new_names = c("id", "string")) %>% 
    .[, n_pattern := str_count(string=string, pattern = pattern)] 
  
  # section 2: share statistics ----  
  paste0("Printing number of pattern: (", pattern, ") occurances in the data.") %>% print()
  stats <- datatable %>% 
    .[, .N, n_pattern] %>% 
    .[order(N)]
  stats %>% print()
  paste0("Threshold is currently set to: (", threshold, "); resulting in (", threshold*(threshold+1), ") columns.") %>% print()
  
  # section 3: split the columns  ----  
  datatable_try_later <- datatable %>% copy() %>% 
    .[n_pattern>threshold]
  
  datatable_names <- names(datatable)
  
  datatable_split <- datatable %>% copy() %>% 
    .[n_pattern<=threshold] %>% 
    .[, paste0("s_",c(1:(threshold+1))) := tstrsplit(string, pattern, fixed=T, fill = "")]
  
  # section 4.1: create a datatable with all column combinations  ----  
  possible_columns <- create_AB_combinations_dt(max=threshold+1) %>% 
    .[, A := paste0("_", A)] %>% 
    .[, B := paste0("_", B)]
  
  # section 4.2: create all possible combinations in the data ----  
  for(row in 1:nrow(possible_columns)){
    
    print(row)
    
    stringA <- possible_columns[row] %>% .[, A] %>% paste0("s", .)
    stringB <- possible_columns[row] %>% .[, B] %>% paste0("s", .)
    stringAB <- possible_columns[row] %>% .[, column]
    
    datatable_split <- datatable_split %>% 
      rename_columns(
        datatable = ., 
        current_names = c(stringA, stringB), 
        new_names = c("stringA", "stringB") 
      ) %>% 
      .[stringA!=""&stringB!="", stringAB := paste(stringA, stringB, sep=" ")] %>% 
      .[stringA==""|stringB=="", stringAB := ""] %>% 
      rename_columns(
        datatable = ., 
        current_names = c("stringA", "stringB", "stringAB"), 
        new_names = c(stringA, stringB, stringAB) 
      )
    
  }
  
  # section 5: final details to export ----  
  # keep these columns
  keep_these <- c(id, string, "n_pattern") %>% append(possible_columns[, unique(column)])
  
  # rename columns back 
  datatable_split <- datatable_split %>% copy() %>% 
    rename_columns(
      datatable = ., 
      current_names = c("id", "string"),
      new_names = c(id, string)) %>% 
    .[, ..keep_these] 
  
  # return 
  out <- list()
  out$stats <- stats %>% copy()
  out$try_later <- datatable_try_later %>% copy()
  out$pairwise_split <-  datatable_split %>% copy() 
  
  return(out)
  
}


# Section 2.3 perform the pairwise merge & get appropriate indices ------------
pairwise_merge_indices <- function(datatable_x, id_x, col_x, 
                                   datatable_y, id_y, col_y,
                                   by_columns, max_index_cutoff=15){
  
  paste0("This pair-wise merge function (pairwise_merge) does the following:", 
         "1) It sequentially merges two data-sets datatable_x & datatable_y individually on each one of their columns specified in: by_columns (both data-sets must share the columns).", 
         "2) For each column pair (e.g. column s_1_2 in x & s_2_1 in y), the function reshapes the datatables into wide format.", 
         "3) If a specific row has a (max_index_cutoff) > (",max_index_cutoff,"), we disconsider that row to avoid over-matching.", 
         "4) The final output is in wide format and indicates the indices of both data-sets.")
  
  for(col_x in by_columns) {
    
    paste0("Column from data.table x: (",col_x,")") %>% print()
    print_line() #() #() #() #() # %>% print()
    
    datatable_x_tmp <- datatable_x %>% copy() %>% 
      rename_columns(
        datatable = .,
        current_names = c(id_x, col_x),
        new_names = c("id_x", "by_col")
      ) %>%
      .[, .(id_x, by_col)] %>%
      # drop empty rows
      .[!((by_col == "") | (by_col == " ") | (by_col == "  "))] %>%   
      .[, index := 1:.N, by_col] %>% 
      .[, max_index := max(index, na.rm=T), by_col] %>% 
      .[, index := paste0("x_",index)] 
    
    datatable_x_tmp %>% .[, .N, max_index] %>% .[order(max_index)] %>% print()
    paste0("max_index cut-off is: (", max_index_cutoff, ")") %>% print()
    
    datatable_x_tmp_wide <- datatable_x_tmp %>% copy() %>% 
      # variable can't show up to more than 15 unique ids
      .[max_index<max_index_cutoff] %>%
      .[, max_index := NULL] %>% 
      dcast(., by_col~index, value.var="id_x", fill = "")
    
    for (col_y in by_columns) {
      
      paste0("Column from data.table y: (",col_y,")") %>% print()
      print_line() #() #() #() #() # %>% print()
      
      datatable_y_tmp <- datatable_y %>%
        rename_columns(
          datatable = .,
          current_names = c(id_y, col_y),
          new_names = c("id_y", "by_col")
        ) %>%
        .[, .(id_y, by_col)] %>%
        # drop empty rows
        .[!((by_col == "") | (by_col == " ") | (by_col == "  "))] %>% 
        .[, index := 1:.N, by_col] %>% 
        .[, max_index := max(index, na.rm=T), by_col] %>% 
        .[, index := paste0("y_",index)] 
      
      datatable_y_tmp %>% .[, .N, max_index] %>% .[order(max_index)] %>% print()
      paste0("max_index cut-off is: (", max_index_cutoff, ")") %>% print()
      
      datatable_y_tmp_wide <- datatable_y_tmp %>% copy() %>% 
        # variable can't show up to more than 15 unique ids
        .[max_index<max_index_cutoff] %>%
        .[, max_index := NULL] %>% 
        dcast(., by_col~index, value.var="id_y", fill = "")
      
      merge_tmp <- merge(x = datatable_x_tmp_wide,
                         y = datatable_y_tmp_wide,
                         by = "by_col",
                         all = F) %>%
        .[, `:=`(by_x = col_x, by_y = col_y)]
      
      if (col_y == by_columns[1]) {
        merge_tmp_out <- merge_tmp %>% copy()
        
      } else{
        merge_tmp_out <- merge_tmp %>% copy() %>% rbind(merge_tmp_out, ., fill=T)
        
      }
      
    }
    
    if (col_x == by_columns[1]) {
      merge_out <- merge_tmp_out %>% copy()
      
    } else{
      merge_out <- merge_tmp_out %>% copy() %>% rbind(merge_out, ., fill=T)
      
    }
    
  }
  
  return(merge_out)
  
}

# create column pairwise combinations ---------

create_unordered_nonrepeating_combinations_dt <- function(max){
  
  strings <- c(1:(max))
  
  CJ(A= strings, B= strings, sorted = T, unique = T) %>% 
    .[A!=B] %>% 
    .[, column := paste0("s","_", A,"_", B)] %>% 
    .[, sum := A + B] %>% 
    .[order(sum, A)] %>% 
    return()
  
}


# split pattern into threshold+1 columns then compute the pairwise combination of these threshold+1 columns
split_pattern_into_tplus1_cols_pairwise_combinations <- function(
    datatable, id,string, pattern, threshold_split){
  
  
  # delete later
  # datatable <- datatable_y_wide_step2 %>% copy() %>%
  #   .[, .(by_column, unique_by_column_id_y)]
  # id = "unique_by_column_id_y"
  # string = "by_column"
  # pattern = " "
  # threshold_split = threshold2
  
  
  # section 1: rename columns & comupute # of occurances of pattern ----
  datatable <- datatable %>% copy() %>% 
    rename_columns(
      datatable = ., 
      current_names = c(id, string),
      new_names = c("id", "string")) %>% 
    .[, string := str_trim(string, side = c("both"))] %>% 
    .[, n_pattern := str_count(string=string, pattern = pattern)] 
  
  # section 2: share statistics ----  
  paste0("Printing number of pattern: (", pattern, ") occurances in the data.") %>% print()
  stats <- datatable %>% 
    .[, .N, n_pattern] %>% 
    .[order(N)]
  stats %>% print()
  paste0("threshold_split is currently set to: (", threshold_split, "). threshold_split==n-1. Resulting in (n)*(n-1) (", threshold_split*(threshold_split+1), ") columns.") %>% print()
  
  print(threshold_split)
  
  
  # in case threshold_split > n_pattern; create completely empty columns
  max_n_pattern <- stats[, max(n_pattern)]
  if(max_n_pattern<threshold_split){
    
    print_line %>% print()
    
    paste0("threshold_split (", threshold_split,") exceeds number of occurances of pattern: (",pattern,"). ", 
           "Resetting threshold_split to the maximum number of the pattern. ",
           "Original threshold_split: (",threshold_split,"). New threshold_split: (",max_n_pattern,")") %>% print()
    
    print_line %>% print()
    
    threshold_split <- max_n_pattern
  } 
  
  # in case threshold_split < n_pattern; create completely empty columns
  min_n_pattern <- stats[, min(n_pattern)]
  if(min_n_pattern>threshold_split){
    
    print_line %>% print()
    
    paste0("threshold_split (", threshold_split,") is always smaller then the minimum number of occurances of pattern: (",pattern,"). ",
           "Resetting threshold_split to the maximum number of the pattern. ",
           "Original threshold_split: (",threshold_split,"). New threshold_split: (",max_n_pattern,")") %>% print()
    
    print_line %>% print()
    
    
    
    threshold_split <- min_n_pattern
  }
  
  # section 3: split the columns  ----  
  datatable_try_later <- datatable %>% copy() %>% 
    .[n_pattern>threshold_split]
  
  datatable_names <- names(datatable)
  string_split_colnames <- paste0("s_",c(1:(threshold_split+1)))
  
  datatable_split <- datatable %>% copy() %>% 
    .[n_pattern<=threshold_split] %>% 
    dt_separate(
      dt_ = .,
      col = string, 
      into = string_split_colnames,
      sep = pattern,
      fill = "", fixed = T, remove = F)
  
  # section 4.1: create a datatable with all column combinations  ----  
  possible_columns <- create_unordered_nonrepeating_combinations_dt(max=threshold_split+1) %>% 
    .[, A := paste0("_", A)] %>% 
    .[, B := paste0("_", B)]
  
  # section 4.2: create all possible combinations in the data ----  
  for(row in 1:nrow(possible_columns)){
    
    print(row)
    
    stringA <- possible_columns[row] %>% .[, A] %>% paste0("s", .)
    stringB <- possible_columns[row] %>% .[, B] %>% paste0("s", .)
    stringAB <- possible_columns[row] %>% .[, column]
    
    datatable_split <- datatable_split %>% 
      rename_columns(
        datatable = ., 
        current_names = c(stringA, stringB), 
        new_names = c("stringA", "stringB") 
      ) %>% 
      .[stringA!=""&stringB!="", stringAB := paste(stringA, stringB, sep=" ")] %>% 
      .[stringA==""|stringB=="", stringAB := ""] %>% 
      rename_columns(
        datatable = ., 
        current_names = c("stringA", "stringB", "stringAB"), 
        new_names = c(stringA, stringB, stringAB) 
      )
    
  }
  
  # section 5: final details to export ----  
  # keep these columns
  keep_these <- c(id, string, "n_pattern") %>% append(possible_columns[, unique(column)])
  
  # rename columns back 
  datatable_split <- datatable_split %>% copy() %>% 
    rename_columns(
      datatable = ., 
      current_names = c("id", "string"),
      new_names = c(id, string)) %>% 
    .[, ..keep_these] 
  
  # return 
  out <- list()
  out$threshold_split_used <- threshold_split
  out$stats <- stats %>% copy()
  out$try_later <- datatable_try_later %>% copy()
  out$pairwise_split <-  datatable_split %>% copy() 
  
  
  
  
  num <- out$stats %>% .[n_pattern<=threshold_split] %>% .[, sum(N)]
  den <- out$stats %>% .[, sum(N)] 
  paste0((num/den)*100, "% of the observations from the datatable were split due to the threshold_split (",threshold_split,") provided.") %>% print()
  
  return(out)
  
}

# perform the pairwise merge & get appropriate indices ------
merge_on_many_columns_and_produce_links <- function(
    datatable_x, id_x, col_x, by_columns_x,
    datatable_y, id_y, col_y, by_columns_y,
    max_index_cutoff=3){
  
  #  delete later
  #   datatable_x = datatable_x_pairs
  # datatable_y = datatable_y_pairs
  # id_x = "unique_by_column_id_x"
  # id_y = "unique_by_column_id_y"
  # col_x =  "by_column"
  # col_y = "by_column"
  # by_columns_x = by_columns_x
  # by_columns_y = by_columns_y
  # max_index_cutoff = threshold_match_cutoff
  
  print_line %>% print()
  
  print("Running function: (merge_on_many_columns_and_produce_links)")
  
  print_line %>% print()
  
  paste0("This pair-wise merge function (pairwise_merge) takes two data-sets & produces a data-set linking matched ids from both.", 
         "Essentially, it does the following:",
         "1) It sequentially merges two data-sets datatable_x & datatable_y individually ",
         "on each one of their columns specified in: by_columns_x & by_columns_y. ", 
         "Since there are (",length(by_columns_x),") columns for x and (",length(by_columns_y),") columns for y, ",
         "there will be (",length(by_columns_x)*length(by_columns_x),") merges in total. ",
         "2) For each column pair (e.g. column s_1_2 in x & s_2_1 in y), the function reshapes the datatables into wide format.", 
         "3) If a specific row has a (max_index_cutoff) > (",max_index_cutoff,"), we disconsider that row to avoid over-matching.", 
         "4) The final output indicates the linked indices for both data-sets & the pair that links them.") %>% print()
  print_line %>% print()
  
  merge_index <- 1
  merge_index_total <- length(by_columns_x)*length(by_columns_y)
  
  # section 1: for each pair in the x datatable; create a long data-set with all the links ------
  # delete later  # col_x <- by_columns_x[1]
  for(col_x in by_columns_x) {
    
    paste0("Column from data.table x: (",col_x,")") %>% print()
    print_line %>% print()
    
    # subset the datatable to that column + the by_column we will merge on 
    datatable_x_tmp <- datatable_x %>% copy() %>% 
      rename_columns(
        datatable = .,
        current_names = c(id_x, col_x),
        new_names = c("id_x", "by_col")
      ) %>%
      .[, .(id_x, by_col)] %>%
      # drop empty rows
      .[!((by_col == "") | (by_col == " ") | (by_col == "  "))] %>%   
      # create an index to reshape, to get one unique obs per name pair
      .[, index := 1:.N, by_col] %>% 
      .[, max_index := max(index, na.rm=T), by_col] %>% 
      .[, index := paste0("x_",index)] 
    
    # show some stats
    show <- max(10, max_index_cutoff)
    datatable_x_tmp %>% .[, .N, max_index] %>% .[order(max_index)] %>% .[1:show] %>% print()
    paste0("max_index cut-off is: (", max_index_cutoff, ")") %>% print()
    
    # do the reshaping for observations that have less than max_index_cutoff linked observations.
    datatable_x_tmp_wide <- datatable_x_tmp %>% copy() %>% 
      # variable can't show up to more than x unique ids
      .[max_index<max_index_cutoff] %>%
      .[, max_index := NULL] %>% 
      dcast(., by_col~index, value.var="id_x", fill = "")
    
    # do the same as above for the y columns (won`t comment exact same code)
    # delete later    # col_y <- by_columns_y[1]
    for (col_y in by_columns_y) {
      
      paste0("Column from data.table y: (",col_y,")") %>% print()
      print_line %>% print()
      
      datatable_y_tmp <- datatable_y %>%
        rename_columns(
          datatable = .,
          current_names = c(id_y, col_y),
          new_names = c("id_y", "by_col")
        ) %>%
        .[, .(id_y, by_col)] %>%
        # drop empty rows
        .[!((by_col == "") | (by_col == " ") | (by_col == "  "))] %>% 
        .[, index := 1:.N, by_col] %>% 
        .[, max_index := max(index, na.rm=T), by_col] %>% 
        .[, index := paste0("y_",index)] 
      
      
      
      datatable_y_tmp %>% .[, .N, max_index] %>% .[order(max_index)] %>% .[1:show] %>%  print()
      paste0("max_index cut-off is: (", max_index_cutoff, ")") %>% print()
      
      datatable_y_tmp_wide <- datatable_y_tmp %>% copy() %>% 
        # variable can't show up to more than 15 unique ids
        .[max_index<max_index_cutoff] %>%
        .[, max_index := NULL] %>% 
        dcast(., by_col~index, value.var="id_y", fill = "")
      
      # now is the time to merge/ we are merging on the word pairs, if the word pair exists
      merge_tmp <- merge(x = datatable_x_tmp_wide,
                         y = datatable_y_tmp_wide,
                         by = "by_col",
                         all = F) %>%
        .[, `:=`(by_x = col_x, by_y = col_y)]
      
      print_line %>% print()
      paste0("Merge ", merge_index, "/", merge_index_total, " done!") %>% print()
      merge_index <- merge_index + 1
      print_line %>% print()
      
      if (col_y == by_columns_y[1]) {
        merge_tmp_out <- merge_tmp %>% copy()
        
      } else{
        merge_tmp_out <- merge_tmp %>% copy() %>% rbind(merge_tmp_out, ., fill=T)
        
      }
      
    }
    
    if (col_x == by_columns_x[1]) {
      merge_out <- merge_tmp_out %>% copy()
      
    } else{
      merge_out <- merge_tmp_out %>% copy() %>% rbind(merge_out, ., fill=T)
      
    }
    
  }
  
  print_line %>% print()
  paste0("Merging concluded. Reshaping and outputting index crosswalk.") %>% print()
  print_line %>% print()
  
  # return 
  merge_out <- merge_out %>% copy() %>% 
    rename_columns(
      datatable = ., 
      current_names = c("by_col"), 
      new_names = c("pairwise_by_column")) 
  
  x_reshape_columns <-  names(merge_out) %>% .[startsWith(prefix = "x_",x = . )] %>% append(., c("pairwise_by_column"))
  y_reshape_columns <-  names(merge_out) %>% .[startsWith(prefix = "y_",x = . )] %>% append(., c("pairwise_by_column"))
  
  merge_out_x <- merge_out %>% copy() %>% 
    # subset data to by_column & all x name links
    .[, ..x_reshape_columns] %>% 
    # reshape 
    melt(., id=c("pairwise_by_column"))  %>% 
    # drop missing/unmatched observations
    .[!(is.na(value)|value=="")] %>% 
    rename_columns(
      datatable = .,
      current_names = c("variable", "value"), 
      new_names = c("index", "unique_by_column_id_x") 
    ) %>% 
    .[, .(pairwise_by_column, unique_by_column_id_x)] %>% 
    # drop duplicated observations, if they exist
    .[, agg_level := paste0(pairwise_by_column, unique_by_column_id_x)] %>% 
    .[!duplicated(agg_level)] %>% 
    .[, agg_level := NULL]  
  
  merge_out_y <- merge_out %>% copy() %>% 
    # subset data to by_column & all y name links
    .[, ..y_reshape_columns] %>% 
    # reshape 
    melt(., id=c("pairwise_by_column"))  %>% 
    # drop missing/unmatched observations
    .[!(is.na(value)|value=="")] %>% 
    rename_columns(
      datatable = .,
      current_names = c("variable", "value"), 
      new_names = c("index", "unique_by_column_id_y") 
    ) %>% 
    .[, .(pairwise_by_column, unique_by_column_id_y)] %>% 
    # drop duplicated observations, if they exist
    .[, agg_level := paste0(pairwise_by_column, unique_by_column_id_y)] %>% 
    .[!duplicated(agg_level)] %>% 
    .[, agg_level := NULL]  
  
  
  
  # merge both data-sets once more to get all possible links
  merge(merge_out_y, merge_out_x, by='pairwise_by_column', all=T, allow.cartesian=TRUE)  %>% 
    return()
  
}

# perform the entire pairwise merge process -----
pairwise_merge_links <- function(
    datatable_x, unique_by_column_id_x,
    datatable_y, unique_by_column_id_y,
    by_column,
    threshold_split,
    threshold_match_cutoff
){
  
  # delete later
  # datatable_x = datatable_x_wide_step2 %>% copy() %>% .[, .(by_column, unique_by_column_id_x)]
  # datatable_y = datatable_y_wide_step2 %>% copy() %>% .[, .(by_column, unique_by_column_id_y)]
  # unique_by_column_id_x = "unique_by_column_id_x"
  # unique_by_column_id_y = "unique_by_column_id_y"
  # by_column = "by_column"
  # threshold_split = threshold2
  # threshold_match_cutoff  = threshold3
  
  print_line %>% print()
  print("Running function: (pairwise_merge_links)") 
  print_line %>% print()
  
  # section 1: set-up ----
  datatable_x <- datatable_x %>% copy() %>% 
    rename_columns(
      datatable = ., 
      current_names = c(by_column, unique_by_column_id_x), 
      new_names = c("by_column", "unique_by_column_id_x")
    )
  
  datatable_y <- datatable_y %>% copy() %>% 
    rename_columns(
      datatable = ., 
      current_names = c(by_column, unique_by_column_id_y), 
      new_names = c("by_column", "unique_by_column_id_y")
    )
  
  # section 2: split by_column into pair-wise combinations ----
  
  split_x <- datatable_x %>% copy() %>%
    .[, .(by_column, unique_by_column_id_x)] %>%
    split_pattern_into_tplus1_cols_pairwise_combinations(
      datatable = .,
      id = "unique_by_column_id_x",
      string = "by_column",
      pattern = " ",
      threshold_split = threshold_split
    )
  
  split_y <- datatable_y %>% copy() %>%
    .[, .(by_column, unique_by_column_id_y)] %>%
    split_pattern_into_tplus1_cols_pairwise_combinations(
      datatable = .,
      id = "unique_by_column_id_y",
      string = "by_column",
      pattern = " ",
      threshold_split = threshold_split
    )
  
  # section 3: prliminaries for the pairwise merge ------
  
  # section 3.1: get the data-sets with the spit columns
  datatable_x_pairs <- split_x$pairwise_split %>% copy()
  datatable_y_pairs <- split_y$pairwise_split %>% copy()
  
  # Section 3.2): for the threshold used (in case of no obs that meet the threshold), get the column names using the same method as split_pattern_into_tplus1_cols_pairwise_combinations ----
  by_columns_x <- create_unordered_nonrepeating_combinations_dt(max = split_x$threshold_split_used+1) %>% .[, column]
  by_columns_y <- create_unordered_nonrepeating_combinations_dt(max = split_y$threshold_split_used+1) %>% .[, column]
  
  # section 4: perform the pairwise merge ------
  indices_pairwise_merge <- merge_on_many_columns_and_produce_links(
    datatable_x = datatable_x_pairs,
    datatable_y = datatable_y_pairs,
    id_x = "unique_by_column_id_x",
    id_y = "unique_by_column_id_y",
    col_x =  "by_column",
    col_y = "by_column",
    by_columns_x = by_columns_x,
    by_columns_y = by_columns_y,
    max_index_cutoff = threshold_match_cutoff
  )
  
  print_line %>% print()
  print("Successfully ran function: (pairwise_merge_links)")
  print_line %>% print()
  
  # section 5: export -------
  indices_pairwise_merge %>% 
    rename_columns(
      datatable = .,
      c("unique_by_column_id_y", "unique_by_column_id_x"),
      c("unique_by_column_id_y", "unique_by_column_id_x")
    ) %>%
    return()
  
}

################################################################################
# Section 3: SIMULATION ########################################################
################################################################################

# create a reproducible ID for the group of variables -----
create_reproducible_id <- function(datatable, variables, id_name){
  
  # delete later
  # datatable <- raw_heat_data %>% copy()
  # variables <- c("year", "unique_event_id", "round", "heat")
  # id_name <- "heat_id"
  
  stopifnot(!("rowINDEX" %in% names(datatable)))
  
  # temporary index
  datatable[, rowINDEX := .I]
  
  # rename variables 
  new_variables <- length(variables) %>% seq(1,.) %>% paste0("V", .)
  select_these <- c("rowINDEX") %>%  append(., new_variables ) # order matters 
  
  datatable %>% 
    rename_columns(
      current_names = variables, 
      new_names = new_variables
    ) %>% 
    .[, ..select_these] %>%
    .[, concat_id:=do.call(paste, .SD), .SDcols=-1] %>% 
    .[, concat_id:=str_replace_all(concat_id, " ", "_")] %>% 
    .[order(concat_id)] %>% 
    .[, concat_id_num := .GRP, concat_id] %>% 
    .[, .(concat_id, concat_id_num, rowINDEX)] %>% 
    merge(x = datatable,  y= ., by="rowINDEX", all=T ) %>% 
    rename_columns(
      current_names = c("concat_id", "concat_id_num"),
      new_names = c(id_name, paste0(id_name, "_num"))
    ) %>% 
    .[, rowINDEX:=NULL] %>% 
    return()
  
}

# create an empty data table with these nrows and columns  -------
create_datatable <- function(nrow, columns){
  
  
  matrix(nrow = nrow,
         ncol = length(columns)) %>% 
    as.data.table() %>% 
    rename_columns(
      current_names = paste0("V", c(1:length(columns))), 
      new_names =  columns
    )  %>% 
    return()
  
} 

# create all possible combinations given a max value -------
create_AB_combinations_dt <- function(max){
  
  strings <- c(1:(max))
  
  CJ(A= strings, B= strings, sorted = T, unique = T) %>% 
    .[A!=B] %>% 
    .[, column := paste0("s","_", A,"_", B)] %>% 
    .[, sum := A + B] %>% 
    .[order(sum, A)] %>% 
    return()
  
}


################################################################################
# Section 4: quick analysis  
################################################################################

# function to sum data-table columns ----
sum_datatable_columns <- function(datatable, columns){
  
  out <- datatable %>% copy() %>% 
    .[, ..columns ] %>% 
    .[, lapply(.SD, sum, na.rm=T)]
  
  return(out)
  
}


# function to answer question:  do indicators sum to number of rows ? 
columns_sum_to_nrow <- function(datatable, columns){
  
  
  # sum data.table columns 
  col_sum <- datatable %>% copy() %>% 
    sum_datatable_columns(
      datatable = ., 
      columns = columns )
  
  (sum(col_sum/nrow(datatable)) * 100) %>% 
    paste0("Column sum is equivalent to ", ., "% of the rows in the data-set") %>% 
    return()
  
}



# row sum for specific column vector
rowsum_for_columns <- function(datatable, columns){
  
  out <- datatable %>% copy() %>% 
    .[, ..columns ] %>% 
    .[, apply(.SD, MARGIN = 1, sum, na.rm=T)] %>% 
    data.table() %>% 
    dplyr::rename(., SUM = ".") 
  
  return(out)
  
}

# function to answer question:  do all rows for a given set of columns (indicators) sum to 1 ? -----

rowsum_for_columns_equals_one <- function(datatable, columns){
  
  rowsum_eq_one <- datatable %>% copy() %>% rowsum_for_columns(datatable = ., columns = columns) %>% .[SUM==1] %>% nrow()
  
  (rowsum_eq_one/nrow(datatable) * 100) %>% 
    paste0(., "% of the rows in the specified columns sum to 1.") %>% 
    return()
  
  
}

# define function to get average of each column ------
get_column_average <- function(datatable, y_vector, colname){
  
  
  # datatable <- dyads_dt_light_out %>% copy()
  # vars <- y_vector %>% .[!duplicated(.)]
  
  out <- datatable %>%
    copy() %>% 
    .[, ..y_vector] %>% 
    .[, lapply(.SD, mean, na.rm=T)] %>% 
    t() %>% 
    as.data.table(keep.rownames = T) 
  
  names(out) <- names(out) %>% 
    str_replace(., pattern = "rn", replacement = colname) %>% 
    str_replace(., pattern = "V1", replacement = "mean")
  
  
  return(out)
  
}


# the the maximum of a column ----
get_max <- function(datatable, column_count_column){
  
  
  # get max number of split columns
  max_name_count <- datatable %>% copy() %>% 
    rename_columns(
      datatable = .,
      current_names = c(column_count_column),
      new_names = c("column_count_column")
    ) %>%
    .[, max(column_count_column)]
  
  return(max_name_count)
  
}


# get vector of column names which follow: variable_root1, variable_root2, ... sequence ----
get_sequential_variables <- function(datatable, column_count_column, variable_root){
  
  N <- get_max(datatable=datatable, column_count_column=column_count_column)
  
  paste0(variable_root, c(1:N)) %>% return()
  
}

# matches a crosswalk with a data-set which contains datatable_key_rootX; where X corresponds to 1, 2, ... ----
match_split_columns_with_crosswalk <- function(datatable,
                                               column_count_column="name_count",
                                               datatable_key_root="name_clean",
                                               crosswalk, 
                                               crosswalk_key="surname"){
  
  # delete later  
  # datatable <- copy(main2)
  # column_count_column <- "name_count"
  # datatable_key_root <- "name_clean"
  # 
  # crosswalk <- copy(surnames_light)
  # crosswalk_key <- "surname"
  
  # 1)  prepare datatable for merge -----
  
  
  max_name_count <- get_max(
    datatable=datatable,
    column_count_column=column_count_column)
  
  # create the output data-set
  out <- datatable %>% copy()
  
  # for each column number 
  for(i in 1:max_name_count){
    
    # 2) setting up naming conventions 
    
    # rename crosswalk variables
    vars_crosswalk_original <- names(crosswalk)
    names(crosswalk) <- names(crosswalk) %>% paste0(., i)
    
    # get the name of the split columns
    name_split_column <- paste0(datatable_key_root, i)  
    # get the name of the crosswalk key variable
    crosswalk_key_i <- paste0(crosswalk_key, i)
    
    # 3) matching 
    paste("Matching column: ", name_split_column, " with crosswalk") %>% print()
    
    out <- out %>% copy() %>% 
      merge(x = ., y = crosswalk, 
            by.x = name_split_column, by.y=crosswalk_key_i, all.x=T, all.y=F)
    
    # revert crosswalk names
    names(crosswalk) <- vars_crosswalk_original
    
  }
  
  # detect the new columns 
  new_columns <- get_new_column_names(old_datatable = datatable, 
                                      new_datatable = out)
  
  # set missings equal to zero 
  out <- out %>% setnafill(x = ., fill = 0, cols = new_columns)
  
  return(out)
  
}



# sum up variables with the same suffix ----
sum_by_variable_suffix <- function(datatable, suffixes, index,prefix) {
  
  for (suffix in suffixes) {
    
    paste0("sum across variables with the following suffix: ", suffix) %>% print()
    
    underscore_suffix <- paste0("_", suffix)
    
    variables_with_suffix <-
      names(datatable) %>% .[endsWith(x = ., suffix = underscore_suffix)]
    
    sum_column <- datatable %>%
      .[, ..variables_with_suffix] %>%
      apply(., 1, sum, na.rm = T) %>%
      data.table()
    
    
    names(sum_column) <- paste0(prefix, suffix)
    
    if (suffix == suffixes[1]) {
      out <- cbind(datatable[, ..index], sum_column)
    } else{
      out <- cbind(out, sum_column)
      
    }
  }  
  return(out)
  
}


################################################################################
# Section 5: Regression analysis 
################################################################################





# get t-stat of linear combination: B1 - B2 ----
# using: two vectors of coefficient names to be B1 & B2. e.g. B1_vector <- c("SO"_cpf_region); B2_vector <- c("DO"_cpf_region)
# the scaled covariance matrix
# vector of coefficients fom the model; if the coef was dropped, it will reflect in the output
# number of parameters in the model and number of observations 

get_linear_combinations_B1_min_B2 <- 
  function(B2_averages_dt, B1_vector, B2_vector, coef_vector, scaled_cov_matrix, N, n_params){
    
    # determine number of 
    # K <- nrow(scaled_cov_matrix)
    K <- n_params
    
    if(K!=nrow(scaled_cov_matrix)){
      
      warning("n_params (", n_params ,") is not equal to nrow(scaled_cov_matrix)(", nrow(scaled_cov_matrix), ").
            Setting K (for df = N - K - 1) to n_params.")
      
    }
    
    # perform linear combination tests of the type: \hat{B}1-\hat{B}2/se(\hat{B}1-\hat{B}2) -----
    
    # if the corresponding vectors have the same length, then we proceed
    if (length(B1_vector) == length(B2_vector)) {
      
      # for each coefficient in the x vector
      for (i in 1:length(B1_vector)) {
        
        # assign the B1 and B2 coefficients
        B1 <- B1_vector[i]
        B2 <- B2_vector[i]
        
        # create variable name
        var_B1_B2 <-  paste0(B1, " - ", B2)
        # and vector with both variables 
        B1_B2 <- c(B1, B2)
        
        # https://statmath.wu.ac.at/~fruehwirth/Oekonometrie_I/Folien_Econometrics_I_teil5.pdf
        
        # first, use the scaled covariance matrix to compute se(Bx-By) ----
        # get the L vector to determine which covariance matrix rows to indicate. e.g. (0,0,1,0,1)
        L_base <- (row.names(scaled_cov_matrix) %in% B1_B2) * 1
        # convert to matrix
        L <- L_base %>% matrix() %>% t()
        # transpose for matrix multiplication 
        Lt <- L %>% t()
        # perform L cov(\hat{B}1-\hat{B}2) L' (scaled covariance matrix)  == L cov L'
        cov_B1_B2 <- (L %*% scaled_cov_matrix) %*% Lt %>% as.numeric()
        # compute se = sqrt( cov_B1_B2 / N)
        se_B1_B2 <- sqrt(cov_B1_B2 / N) %>% as.numeric()
        # second, compute the difference for the variables ----
        # compute difference 
        diff_B1_B2 <-
          (coef_vector[B1_B2][1] - coef_vector[B1_B2][2]) %>% as.numeric()
        # third, compute the t-statistic & p-value ----
        # t statistic = diff_B1_B2 / se_B1_B2
        t_statistic <- (diff_B1_B2 / se_B1_B2)
        # df (should df be total controls or include FE? )
        df <- N - K - 1
        # compute the upper tail  
        p_value <- pt(
          q = abs(t_statistic),
          df = df,
          lower.tail = F)
        # compute upper and lower bounds
        z <- qt(p = .975, df = df, lower.tail = T)
        upper_bound <- diff_B1_B2 + z*se_B1_B2
        lower_bound <- diff_B1_B2 - z*se_B1_B2
        
        
        # construct a data.table with all of the relevant information. 
        tmp <- data.table(
          B1minB2 = c(var_B1_B2),
          B1 = c(B1),
          B2 = c(B2),
          coef = c(diff_B1_B2),
          cov = c(cov_B1_B2),
          se = c(se_B1_B2),
          N = c(N),
          K = c(K),
          df = c(df),
          t_statistic = c(t_statistic),
          p_value = c(p_value), 
          lower_bound = c(lower_bound),
          upper_bound = c(upper_bound)
        ) %>% 
          # merge B2_avergaes with tmp 
          merge(., B2_averages_dt, "B2", all=F) %>% 
          # normalize coefficient
          .[, coef_norm := coef/mean] %>% 
          .[, lower_bound_norm := lower_bound/mean] %>% 
          .[, upper_bound_norm := upper_bound/mean] 
        
        if (i == 1) {
          out <- tmp %>% copy()
        } else{
          out <- tmp %>% copy() %>% rbind(out, .)
        }
        
      }
      
    }else{
      # otherwise, print out alert that something went wrong 
      warning("The B1_vector and B2_vector lengths are not equal. Please re-evaluate vectors.")
    }
    
    order <- c("B1minB2", "B1", "B2", "coef", "cov", "se", "N", "K", "df", "t_statistic", "p_value", "lower_bound", "upper_bound", "mean") 
    new_names <- c("linear_combination", "B1", "B2", "coef", "cov", "se", "N", "K", "df", "t_statistic", "p_value", "lower_bound", "upper_bound", "E[y|B2==1]") 
    
    out <- out %>% .[, ..order]
    names(out) <- new_names
    
    return(out)
    
  }

# computes the expectation of y given x for a datatable ----- 
expectation_y_given_x <- function(datatable, y, x_vector){
  
  x_vector <- x_vector %>% .[!duplicated(.)]
  
  keep_these <- append(y, x_vector) 
  
  datatable_reduced <-  datatable %>% copy() %>% .[, ..keep_these]
  
  # rename y column 
  names(datatable_reduced) <- names(datatable_reduced) %>% 
    str_replace(., pattern = y, replacement = "y")
  
  for(var in x_vector){
    
    #var <- x_vector[2]
    
    var_str <- var
    
    # rename y column 
    names(datatable_reduced) <- names(datatable_reduced) %>% 
      str_replace(., pattern = var, replacement = "var___")
    
    # take data table
    tmp <- datatable_reduced %>% 
      copy() %>% 
      # compute mean of y conditional on var
      .[, mean(y, na.rm=T), var___] %>%
      .[, variable_name := var_str] %>% 
      dplyr::rename(., x=var___, "E[y|x]" = V1) 
    
    # rename y column 
    names(datatable_reduced) <- names(datatable_reduced) %>% 
      str_replace(., pattern = "var___", replacement = var)
    
    if(var==x_vector[1]){
      out <- tmp %>% copy()
      
    }else{
      out <- tmp %>% copy() %>% rbind(out, .)
    }
  }
  
  out %>%
    .[, agg_level := paste0(x, variable_name)] %>% 
    .[!duplicated(agg_level)] %>% 
    .[, agg_level:=NULL] %>% 
    return(.)
  
}

# get estimation sample using obsRemoved & the input datatable -----
get_estimation_sample <- function(datatable, obsRemoved){
  
  # get all of the rows
  rows <- datatable[, .I]
  
  if(length(obsRemoved)>0){
    
    estimation_sample <- rows %>% .[!(. %in% obsRemoved)]
    
  }else{
    estimation_sample <- rows
  }
  
  return(estimation_sample)
  
}

# get the lightwright elements from model summary so we can use it later ----
get_lightweight_summary <- function(fixest_model_summary){
  
  out<-list()
  
  out$coeftable<-fixest_model_summary$coeftable
  out$nparams<-fixest_model_summary$nparams
  out$cov.scaled<-fixest_model_summary$cov.scaled
  out$sigma2<-fixest_model_summary$sigma2
  out$hessianout$hessian
  out$se<-fixest_model_summary$se
  out$fixef_sizes<-fixest_model_summary$fixef_sizes
  out$nobs<-fixest_model_summary$nobs
  out$nobs_origin<-fixest_model_summary$nobs_origin
  out$nparams<-fixest_model_summary$nparams
  out$fml_no_xpd<-fixest_model_summary$fml_no_xpd
  out$fixef_vars<-fixest_model_summary$fixef_vars
  out$obsRemoved<-fixest_model_summary$obsRemoved
  out$coefficients<-fixest_model_summary$coefficients
  out$method<-fixest_model_summary$method
  out$method_type<-fixest_model_summary$method_type
  out$multicol<-fixest_model_summary$multicol
  out$collin.min_norm<-fixest_model_summary$collin.min_norm
  out$ll_fe_only<-fixest_model_summary$ll_fe_only
  out$ssr_fe_only<-fixest_model_summary$ssr_fe_only
  out$ll_null<-fixest_model_summary$ll_null
  out$ssr<-fixest_model_summary$ssr
  out$ssr_null<-fixest_model_summary$ssr_null
  out$sq.cor<-fixest_model_summary$sq.cor
  out$iterations<-fixest_model_summary$iterations
  
  # ols_all_occup_summary$fitted.values
  # ols_all_occup_summary$scores
  # ols_all_occup_summary$sumFE
  # ols_all_occup_summary$residuals
  
  out$tex <- etable(fixest_model_summary, tex = TRUE)
  
  return(out)
  
}

# save the lightwright elements from model summary so we can use it later ----
save_lightweight_summary <- function(fixest_model_summary, filename=NULL, directory){
  
  out<-list()
  
  out$coeftable<-fixest_model_summary$coeftable
  out$nparams<-fixest_model_summary$nparams
  out$cov.scaled<-fixest_model_summary$cov.scaled
  out$sigma2<-fixest_model_summary$sigma2
  out$hessianout$hessian
  out$se<-fixest_model_summary$se
  out$fixef_sizes<-fixest_model_summary$fixef_sizes
  out$nobs<-fixest_model_summary$nobs
  out$nobs_origin<-fixest_model_summary$nobs_origin
  out$nparams<-fixest_model_summary$nparams
  out$fml_no_xpd<-fixest_model_summary$fml_no_xpd
  out$fixef_vars<-fixest_model_summary$fixef_vars
  out$obsRemoved<-fixest_model_summary$obsRemoved
  out$coefficients<-fixest_model_summary$coefficients
  out$method<-fixest_model_summary$method
  out$method_type<-fixest_model_summary$method_type
  out$multicol<-fixest_model_summary$multicol
  out$collin.min_norm<-fixest_model_summary$collin.min_norm
  out$ll_fe_only<-fixest_model_summary$ll_fe_only
  out$ssr_fe_only<-fixest_model_summary$ssr_fe_only
  out$ll_null<-fixest_model_summary$ll_null
  out$ssr<-fixest_model_summary$ssr
  out$ssr_null<-fixest_model_summary$ssr_null
  out$sq.cor<-fixest_model_summary$sq.cor
  out$iterations<-fixest_model_summary$iterations
  
  # ols_all_occup_summary$fitted.values
  # ols_all_occup_summary$scores
  # ols_all_occup_summary$sumFE
  # ols_all_occup_summary$residuals
  
  out$tex <- etable(fixest_model_summary, tex = TRUE)
  
  # save
  if(length(filename)>0){
    
    filename_full <-  paste0(directory, filename, ".rds")
    
    
    paste0("Saving summary in: ", filename_full) %>% print()
    
    saveRDS(
      object = out,
      file = filename_full)
    
  }
  
  
}


