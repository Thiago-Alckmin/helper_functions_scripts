# #######################################
# filename: helper_functions_brazil.R 
# author: Thiago Jan/2022
# purpose: collection of functions which help with Brazil specific research. 
# #######################################

print("Executing helper_functions_GPC.R")

require(magrittr)

# Section 0: CNPJ helper functions --------------------

print_line<- print("------------------------------------------------------------")

# function takes in dataframe/data.table with dirty 14 digit cnpj and
# outputs the same df/dt with a clean cnpj column
clean_cnpj <- function(df, cnpj_column){
  
  # set data frame  
  setDF(df)
  # rename to work with datatable
  df$cnpj_dirty <- df[, cnpj_column]
  # set datatable
  setDT(df)
  
  # clean the cnpjs
  out <- df %>% # cpf as character
    .[, cnpj_clean := as.character(cnpj_dirty)] %>% 
    # cpf should have 11 digits
    .[, n_char := nchar(cnpj_clean)] %>%
    .[n_char == 0, cnpj_clean := paste0("00000000000000", cnpj_clean)] %>%
    .[n_char == 1, cnpj_clean := paste0("0000000000000", cnpj_clean)] %>%
    .[n_char == 2, cnpj_clean := paste0("000000000000", cnpj_clean)] %>%
    .[n_char == 3, cnpj_clean := paste0("00000000000", cnpj_clean)] %>%
    .[n_char == 4, cnpj_clean := paste0("0000000000", cnpj_clean)] %>%
    .[n_char == 5, cnpj_clean := paste0("000000000", cnpj_clean)] %>%
    .[n_char == 6, cnpj_clean := paste0("00000000", cnpj_clean)] %>%
    .[n_char == 7, cnpj_clean := paste0("0000000", cnpj_clean)] %>%
    .[n_char == 8, cnpj_clean := paste0("000000", cnpj_clean)] %>%
    .[n_char == 9, cnpj_clean := paste0("00000", cnpj_clean)] %>%
    .[n_char == 10, cnpj_clean := paste0("0000", cnpj_clean)] %>% 
    .[n_char == 11, cnpj_clean := paste0("000", cnpj_clean)] %>% 
    .[n_char == 12, cnpj_clean := paste0("00", cnpj_clean)] %>% 
    .[n_char == 13, cnpj_clean := paste0("0", cnpj_clean)] %>% 
    .[, n_char := NULL] %>% 
    # add cnpj_clean structure as well to avoid errors
    .[, cnpj_clean_char :=  paste0(
      stri_sub(cnpj_clean, 1, 2), ".",
      stri_sub(cnpj_clean, 3, 5), ".",
      stri_sub(cnpj_clean, 6, 8), "/",
      stri_sub(cnpj_clean, 9, 12), "-", 
      stri_sub(cnpj_clean, 13, 14))]
  
  return(out)
  
}

# clean incomplete cnpj (i.e. force 12 digits) ----
clean_incomplete_cnpj <- function(data, incomplete_cnpj_col = "firmplantid"){
  
  # data <- cnpjs %>% setDT() %>%copy()
  # firmplantid_col <- "firmplantid"
  
  # set firmplantid 
  names(data)[names(data) == incomplete_cnpj_col] <- "incomplete_cnpj"
  
  # copy & split columns
  out <- data %>%
    .[, incomplete_cnpj := as.character(incomplete_cnpj)] %>% 
    .[, incomplete_cnpj := str_remove_all(string = incomplete_cnpj, pattern = " ")] %>% 
    .[, n_char := nchar(incomplete_cnpj)] %>%
    .[n_char == 11, incomplete_cnpj := paste0("0", incomplete_cnpj)] %>%
    .[n_char == 10, incomplete_cnpj := paste0("00", incomplete_cnpj)] %>%
    .[n_char == 9, incomplete_cnpj := paste0("000", incomplete_cnpj)] %>%
    .[n_char == 8, incomplete_cnpj := paste0("0000", incomplete_cnpj)] %>%
    .[n_char == 7, incomplete_cnpj := paste0("00000", incomplete_cnpj)] %>%
    .[n_char == 6, incomplete_cnpj := paste0("000000", incomplete_cnpj)] %>%
    .[n_char == 5, incomplete_cnpj := paste0("0000000", incomplete_cnpj)] %>%
    .[n_char == 4, incomplete_cnpj := paste0("00000000", incomplete_cnpj)] %>%
    .[n_char == 3, incomplete_cnpj := paste0("000000000", incomplete_cnpj)] %>%
    .[n_char == 2, incomplete_cnpj := paste0("0000000000", incomplete_cnpj)] %>%
    .[n_char == 1, incomplete_cnpj := paste0("00000000000", incomplete_cnpj)] %>% 
    .[, n_char := NULL]
  
  return(out)
}

# clean complete cnpj (i.e. force 14 digits) ----
clean_complete_cnpj <- function(data, complete_cnpj_col = "firmplantid"){
  
  # data <- cnpjs %>% setDT() %>%copy()
  # firmplantid_col <- "firmplantid"
  
  # set firmplantid 
  names(data)[names(data) == complete_cnpj_col] <- "complete_cnpj"
  
  # copy & split columns
  out <- data %>%
    .[, complete_cnpj := as.character(complete_cnpj)] %>% 
    .[, complete_cnpj := str_remove_all(string = complete_cnpj, pattern = " ")] %>% 
    .[, n_char := nchar(complete_cnpj)] %>%
    .[n_char == 13, complete_cnpj := paste0("0", complete_cnpj)] %>%
    .[n_char == 12, complete_cnpj := paste0("00", complete_cnpj)] %>%
    .[n_char == 11, complete_cnpj := paste0("000", complete_cnpj)] %>%
    .[n_char == 10, complete_cnpj := paste0("0000", complete_cnpj)] %>%
    .[n_char == 9, complete_cnpj := paste0("00000", complete_cnpj)] %>%
    .[n_char == 8, complete_cnpj := paste0("000000", complete_cnpj)] %>%
    .[n_char == 7, complete_cnpj := paste0("0000000", complete_cnpj)] %>%
    .[n_char == 6, complete_cnpj := paste0("00000000", complete_cnpj)] %>%
    .[n_char == 5, complete_cnpj := paste0("000000000", complete_cnpj)] %>%
    .[n_char == 4, complete_cnpj := paste0("0000000000", complete_cnpj)] %>%
    .[n_char == 3, complete_cnpj := paste0("00000000000", complete_cnpj)] %>%
    .[n_char == 2, complete_cnpj := paste0("000000000000", complete_cnpj)] %>%
    .[n_char == 1, complete_cnpj := paste0("0000000000000", complete_cnpj)] %>% 
    .[, n_char := NULL]
  
  return(out)
}

# function creates additional columns with full cnpj ----
incomplete_to_full_cnpj <- function(data, incomplete_cnpj_col = "firmplantid"){
  
  # data <- cnpjs %>% setDT() %>%copy()
  # firmplantid_col <- "firmplantid"
  
  # set firmplantid 
  names(data)[names(data) == incomplete_cnpj_col] <- "incomplete_cnpj"
  
  # copy & split columns
  data2 <- data %>% copy() %>% setDT(.) %>%
    .[, str_split_fixed(incomplete_cnpj, pattern = "", n = 12) ] %>%
    as.data.table() %>%
    setDF(.)
  
  # Section 1.1: compute first number -----
  
  df <- copy(data2)
  
  # set cnpj verification key
  first_key <- c(5,4,3,2,9,8,7,6,5,4,3,2)
  
  # transform
  for(column in 1:length(names(df))){
    df[, column] <- as.numeric(df[, column]) * first_key[column]
  } 
  
  # row sum
  first_remainder <- rowSums(x = df)%%11
  
  # if first decimal larger than 2, subtract from 11, otherwise 0 
  first_digit <- (11 - first_remainder)*(first_remainder > 1)
  
  # append to original data
  data2$V13 <- first_digit
  
  # Section 1.2: compute second number -----
  
  df <- copy(data2)
  
  # set cnpj verification key
  second_key <- c(6,5,4,3,2,9,8,7,6,5,4,3,2)
  
  # transform
  for(column in 1:length(names(df))){
    df[, column] <- as.numeric(df[, column]) * second_key[column]
  } 
  
  # row sum
  ssecond_remainder <- rowSums(x = df)%%11
  
  # if second decimal larger than 2, subtract from 11, otherwise 0 
  second_digit <- (11 - ssecond_remainder)*(ssecond_remainder > 1)
  
  # append to original data
  data2$V14 <- second_digit
  
  # Section 1.3: join data into cnpj once more -----
  
  # transform
  for(column in 1:length(names(data2))){
    data2[, column] <- as.character(data2[, column])
  } 
  
  cnpjs <- data2 %>% 
    unite('cnpj_full', V1:V14, remove=TRUE, sep = "") %>%
    as.data.table()
  
  out <- cbind(data, cnpjs)
  
  return(out)
  
}

# function to disidentify cnpj ----
incomplete_cnpj_to_id_j <- function(data, incomplete_cnpj_col = "firmplantid"){
  
  # set firmplantid 
  names(data)[names(data) == incomplete_cnpj_col] <- "incomplete_cnpj"
  
  # copy & split columns
  data2 <- data %>% copy() %>% setDT(.) %>%
    .[, str_split_fixed(incomplete_cnpj, pattern = "", n = 12) ] %>%
    as.data.table() %>%
    setDF(.)
  
  # Section 1.1: compute first number -----
  
  df <- copy(data2)
  
  # set cnpj verification key
  first_key <- c(1,0,1,0,1,0,0,1,3, 1,0,1)
  
  # transform
  for(column in 1:length(names(df))){
    df[, column] <- as.numeric(df[, column]) + first_key[column]
  } 
  
  # if 10, then 0
  df <- (df!=10)*df
  
  id_j <- df %>% 
    unite('id_j', V1:V12, remove=TRUE, sep = "") %>%
    as.data.table() %>%
    .[, id_j := paste0(id_j, "@IRR.PAW")]
  
  out <- cbind(data, id_j)
  
  return(out)
  
}

# function to disidentify cnpj ----
complete_cnpj_to_id_j <- function(data, complete_cnpj_col = "complete_cnpj"){
  
  # data <- contact_info
  # complete_cnpj_col <- "complete_cnpj"
  
  
  # set firmplantid 
  names(data)[names(data) == complete_cnpj_col] <- "complete_cnpj"
  
  # copy & split columns; drops last two digits
  data2 <- data %>% copy() %>% setDT(.) %>%
    .[, str_split_fixed(complete_cnpj, pattern = "", n = 14) ] %>%
    as.data.table() %>%
    .[, `:=`(V13=NULL, V14 = NULL)] %>%
    setDF(.)
  
  # Section 1.1: compute first number -----
  
  df <- copy(data2)
  
  # set cnpj verification key
  first_key <- c(1,0,1,0,1,0,0,1,3, 1,0,1)
  
  # transform
  for(column in 1:length(names(df))){
    df[, column] <- as.numeric(df[, column]) + first_key[column]
  } 
  
  # if 10, then 0
  df <- (df!=10)*df
  
  id_j <- df %>% 
    unite('id_j', V1:V12, remove=TRUE, sep = "") %>%
    as.data.table() %>%
    .[, id_j := paste0(id_j, "@IRR.PAW")]
  
  out <- cbind(data, id_j)
  
  return(out)
  
}

# clean complete cpf (i.e. force 12 digits) ----
clean_complete_cpf <- function(data, complete_cpf_col = "cpf"){
  
  # data <- cpfs %>% setDT() %>%copy()
  # firmplantid_col <- "firmplantid"
  
  # set firmplantid 
  names(data)[names(data) == complete_cpf_col] <- "complete_cpf"
  
  # copy & split columns
  out <- data %>%
    .[, complete_cpf := as.character(complete_cpf)] %>% 
    .[, complete_cpf := str_remove_all(string = complete_cpf, pattern = " ")] %>% 
    .[, n_char := nchar(complete_cpf)] %>%
    .[n_char == 10, complete_cpf := paste0("0", complete_cpf)] %>%
    .[n_char == 9, complete_cpf := paste0("00", complete_cpf)] %>%
    .[n_char == 8, complete_cpf := paste0("000", complete_cpf)] %>%
    .[n_char == 7, complete_cpf := paste0("0000", complete_cpf)] %>%
    .[n_char == 6, complete_cpf := paste0("00000", complete_cpf)] %>%
    .[n_char == 5, complete_cpf := paste0("000000", complete_cpf)] %>%
    .[n_char == 4, complete_cpf := paste0("0000000", complete_cpf)] %>%
    .[n_char == 3, complete_cpf := paste0("00000000", complete_cpf)] %>%
    .[n_char == 2, complete_cpf := paste0("000000000", complete_cpf)] %>%
    .[n_char == 1, complete_cpf := paste0("0000000000", complete_cpf)] %>% 
    .[, n_char := NULL]
  
  return(out)
}

# clean part cpf (i.e. force 12 digits) ----
clean_part_cpf <- function(data, part_cpf_col = "part_cpf"){
  
  # data <- copy(main2)
  # part_cpf_col <- "part_cpf"
  
  # set firmplantid 
  names(data)[names(data) == part_cpf_col] <- "part_cpf"
  
  # copy & split columns
  out <- data %>%
    .[, clean_part_cpf := as.character(part_cpf)] %>% 
    .[, clean_part_cpf := str_remove_all(string = clean_part_cpf, pattern = " ")] %>% 
    .[, n_char := nchar(part_cpf)] %>%
    .[n_char == 6, clean_part_cpf := paste0("XXX", clean_part_cpf, "XX")] %>%
    .[n_char == 5, clean_part_cpf := paste0("XXX0", clean_part_cpf, "XX")] %>%
    .[n_char == 4, clean_part_cpf := paste0("XXX00", clean_part_cpf, "XX")] %>%
    .[n_char == 3, clean_part_cpf := paste0("XXX000", clean_part_cpf, "XX")] %>%
    .[n_char == 2, clean_part_cpf := paste0("XXX0000", clean_part_cpf, "XX")] %>%
    .[n_char == 1, clean_part_cpf := paste0("XXX00000", clean_part_cpf, "XX")] %>%
    .[, n_char := NULL]
  
  return(out)
}

# function to disidentify cpf ----
complete_cpf_to_id_j <- function(data, complete_cpf_col = "complete_cpf"){
  
  # data <- contact_info
  # complete_cpf_col <- "complete_cpf"
  
  
  # set firmplantid 
  names(data)[names(data) == complete_cpf_col] <- "complete_cpf"
  
  # copy & split columns; drops last two digits
  data2 <- data %>% copy() %>% setDT(.) %>%
    .[, str_split_fixed(complete_cpf, pattern = "", n = 11) ] %>%
    as.data.table() %>%
    .[, `:=`(V10=NULL, V11 = NULL)] %>%
    setDF(.)
  
  # Section 1.1: compute first number -----
  
  df <- copy(data2)
  
  # set key for disidentification of cpf 
  first_key <- c(1,0,1,0,1,0,0,1,0)
  
  # transform
  for(column in 1:length(names(df))){
    df[, column] <- as.numeric(df[, column]) + first_key[column]
  } 
  
  # if 10, then 0
  df <- (df!=10)*df
  
  id_j <- df %>% 
    unite('id_j', V1:V9, remove=TRUE, sep = "") %>%
    as.data.table() %>%
    .[, id_j := paste0(id_j, "@IRR.PAW")]
  
  out <- cbind(data, id_j)
  
  return(out)
  
}

# create cleaning function for all text variables that need cleaning ----
clean_politics_at_work_experiment_strings <- function(dt, col){ # clean_variable
  
  #dt <- survey_responses4
  #col <- "s_region"
  
  # convert to df
  df <- dt %>% copy() %>% as.data.frame()
  setDF(df)
  
  # convert just column to dt
  dt2 <- df[,col] %>% data.table(col_ = .)
  
  # clean column
  dt2 %>%
    .[, col_ := str_to_lower(string = col_) ] %>%
    # .[, col_ := str_replace_all(string = col_, pattern = "é", replacement = "e") ] %>%
    # .[, col_ := str_replace_all(string = col_, pattern = "í", replacement = "i") ] %>%
    .[, col_ := gsub(x = col_, pattern = " ", replacement = "")] %>% 
    .[, col_ := gsub(x = col_, pattern = "\\(", replacement = "")] %>% 
    .[, col_ := gsub(x = col_, pattern = "\\)", replacement = "")] %>% 
    .[, col_ := gsub(x = col_, pattern = "á", replacement = "a")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ã", replacement = "a")] %>% 
    .[, col_ := gsub(x = col_, pattern = "à", replacement = "a")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ç", replacement = "c")] %>% 
    .[, col_ := gsub(x = col_, pattern = "õ", replacement = "o")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ô", replacement = "o")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ó", replacement = "o")] %>% 
    .[, col_ := gsub(x = col_, pattern = "é", replacement = "e")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ê", replacement = "e")] %>% 
    .[, col_ := gsub(x = col_, pattern = "í", replacement = "i")] %>% 
    .[, col_ := gsub(x = col_, pattern = ",", replacement = "")] %>% 
    .[, col_ := gsub(x = col_, pattern = "-", replacement = "") ] %>%
    # clean some large names
    .[, col_ := gsub(x = col_, pattern = "negocioseconomiaecontabilidade", replacement = "negocios") ] %>%
    .[, col_ := gsub(x = col_, pattern = "outroshumanidadesoutrascienciassociaisecienciasnaturais", replacement = "outros") ] %>%
    .[, col_ := gsub(x = col_, pattern = "outroshumanasoutrascienciassociaisecienciasnaturais", replacement = "outros") ] %>%
    .[, col_ := gsub(x = col_, pattern = "engenhariacienciadacomputacãomatematicaeestatistica", replacement = "engenharia") ] %>%
    .[, col_ := gsub(x = col_, pattern = "engenhariacienciadacomputacaomatematicaeestatistica", replacement = "engenharia") ]
  
  # name column original name
  names(dt2) <- col
  
  # over-write original
  df[, col] <- dt2 %>% data.frame()
  
  # convert again
  out <- as.data.table(df)
  setDT(out)
  
  return(out)
  
}

# create cleaning function for all text variables that need cleaning ----
clean_politics_at_work_experiment_strings2 <- function(dt, col){ #clean_variable_additional
  
  #dt <- survey_responses4
  #col <- "s_region"
  
  # convert to df
  df <- dt %>% copy() %>% as.data.frame()
  setDF(df)
  
  # convert just column to dt
  dt2 <- df[,col] %>% data.table(col_ = .)
  
  # clean column
  dt2 %>%
    .[, col_ := str_to_lower(string = col_) ] %>%
    # .[, col_ := str_replace_all(string = col_, pattern = "é", replacement = "e") ] %>%
    # .[, col_ := str_replace_all(string = col_, pattern = "í", replacement = "i") ] %>%
    .[, col_ := gsub(x = col_, pattern = " ", replacement = "")] %>% 
    .[, col_ := gsub(x = col_, pattern = "\\(", replacement = "")] %>% 
    .[, col_ := gsub(x = col_, pattern = "\\)", replacement = "")] %>% 
    .[, col_ := gsub(x = col_, pattern = "á", replacement = "a")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ã", replacement = "a")] %>% 
    .[, col_ := gsub(x = col_, pattern = "à", replacement = "a")] %>% 
    .[, col_ := gsub(x = col_, pattern = "â", replacement = "a")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ç", replacement = "c")] %>% 
    .[, col_ := gsub(x = col_, pattern = "õ", replacement = "o")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ô", replacement = "o")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ó", replacement = "o")] %>% 
    .[, col_ := gsub(x = col_, pattern = "é", replacement = "e")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ê", replacement = "e")] %>% 
    .[, col_ := gsub(x = col_, pattern = "í", replacement = "i")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ú", replacement = "u")] %>% 
    .[, col_ := gsub(x = col_, pattern = "ü", replacement = "u")] %>% 
    .[, col_ := gsub(x = col_, pattern = ",", replacement = "")] %>% 
    .[, col_ := gsub(x = col_, pattern = "-", replacement = "") ] %>%
    # clean additional things
    .[, col_ := gsub(x = col_, pattern = "a©", replacement = "e") ]  %>%
    # clean some large names
    .[, col_ := gsub(x = col_, pattern = "negocioseconomiaecontabilidade", replacement = "negocios") ] %>%
    .[, col_ := gsub(x = col_, pattern = "outroshumanidadesoutrascienciassociaisecienciasnaturais", replacement = "outros") ] %>%
    .[, col_ := gsub(x = col_, pattern = "outroshumanasoutrascienciassociaisecienciasnaturais", replacement = "outros") ] %>%
    .[, col_ := gsub(x = col_, pattern = "engenhariacienciadacomputacãomatematicaeestatistica", replacement = "engenharia") ] %>%
    .[, col_ := gsub(x = col_, pattern = "engenhariacienciadacomputacaomatematicaeestatistica", replacement = "engenharia") ] %>%
    .[, col_ := gsub(x = col_, pattern = "'", replacement = "") ] 
  # name column original name
  names(dt2) <- col
  
  # over-write original
  df[, col] <- dt2 %>% data.frame()
  
  # convert again
  out <- as.data.table(df)
  setDT(out)
  
  return(out)
  
}


# reingex firmid, labor_market, personid datatable ----
reindex_m <- function(datatable, mcrosswalk){
  
  # delete later
  # datatable <- surname_fyi %>% copy()
  # mcrosswalk <-  labor_market_num_crosswalk %>% copy()
  
  datatable <- datatable %>% copy() %>% 
    merge(mcrosswalk, by = "labor_market", all.x = F, all.y=F) %>% 
    .[, labor_market:=NULL] 
  
  return(datatable)
  
  
}

# reindex at the person level ----
reindex_i <- function(datatable,
                      icrosswalk){
  
  #delete later
  # datatable <- surname_fyi %>% copy()
  # icrosswalk <-  personid_num_crosswalk %>% copy()
  
  
  datatable <- datatable %>% copy() %>% 
    merge(icrosswalk, by = "personid", all.x = F, all.y=F) %>% 
    .[, personid:=NULL] 
  
  return(datatable)
  
}
# reindex at the firm ----
reindex_f <- function(datatable, fcrosswalk){
  
  # delete later
  # datatable <- surname_fyi %>% copy()
  # fcrosswalk <-  firmid_num_crosswalk %>% copy()
  
  datatable <- datatable %>% copy() %>% 
    merge(fcrosswalk, by = "firmid", all.x = F, all.y=F) %>% 
    .[, firmid:=NULL]  
  
  return(datatable)
  
}



