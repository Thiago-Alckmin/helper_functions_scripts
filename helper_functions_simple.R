
# packages used 
# pacman::p_load(data.table, magrittr, dplyr, glue, stringr)

################################################################################
# Section 0: functions to use in the set up section  ###########################
################################################################################

save_global_env <- function(directory="/home/tresende/Global_Political_Connections/general_purpose/code/global_env/"){
  
  global_env_save <- get_timestamp() %>% 
    substr(start = 1, stop = 11) %>% 
    str_remove_all(., pattern = "-") %>% 
    paste0("global_env",., ".RData") %>% 
    paste0(directory, .)
  
  save.image(file=global_env_save)}

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


# Section 3.0: examples of names that need cleaning  -----
str_create_name_column <- function(dataset="CIA-CHIEFS"){
  
  data_set_options <- c("CIA-CHIEFS")
  
  if(is.null(dataset)){
    message("The names here are sourced from actual data-sets. Available data-set options are: ")
    message(glue::glue_collapse(data_set_options, sep = ", "))
  }

  if(dataset=="CIA-CHIEFS"){
    c(
      "Prifti, Dritan",
      "Baymyrat HOJAMUHAMMEDOW"
      ,
      "Francoise ASSOGBA",
      "Sarbu, Marian"
      ,
      "Jennifer WESTFORD, Doctor",
      "Hierro, Luis"
      ,
      "Ahmed Mohamed Mohamed AL-KAROURI",
      "Raja Pervaiz ASHRAF"
      ,
      "Bedouma Alain YODA",
      "Vasyl HRYTSAK"
      ,
      "Abdul Hadi ARGHANDIWAL",
      "Toprak, Erdogan"
      ,
      "Jose Carlos Lopes CORREIA",
      "Sheikh Hassan Ismail BILE"
      ,
      "MENG Jianzhu",
      "Metogho, Emmanuel Ondo"
      ,
      "Ivan ZAMBRANO",
      "Tatoul MARKARIAN"
      ,
      "Mariano Gago, Jose",
      "Maumoon Abdul GAYOOM"
      ,
      "Sabine LARUELLE",
      "Denis SASSOU-Nguesso"
      ,
      "Jeffrey, Henry, Doctor",
      "Abdoulaye BALDE"
      ,
      "Sergey MASKEVICH",
      "Masahiko KOMURA"
      ,
      "Tsedevdamba OYUNGEREL",
      "Tokon MAMYTOV"
      ,
      "MOHAMMAD bin Abd Rahman",
      "Evode UWIZEYIMANA"
      ,
      "Maria KIWANUKA",
      "Selma Aliye KAVAF"
      ,
      "Djombo, Henri",
      "Petre TSISKARISHVILI"
      ,
      "Gilles NOGHES",
      "Adechi, Joel"
      ,
      "Clayton BURGIN",
      "Mikhail KHVOSTOV"
      ,
      "Abdiweli Ibrahim Sheikh MUUDEEY",
      "M. Veerappa MOILY"
      ,
      "Ngandagina, Joao Baptista",
      "Babanyyaz ITALMAZOW"
      ,
      "Adil SAFIR",
      "Andrea LEADSOM"
      ,
      "Tabare Ramon VAZQUEZ Rosas",
      "Peri Vaevae PARE"
      ,
      "Milosavljevic, Slobodan",
      "Besir ATALAY"
      ,
      "Naomi Mataafa FIAME",
      "SUWIT Yotmanee"
      ,
      "Shuala, Abd al-Nabi al-",
      "Reginald AUSTRIE"
      ,
      "Elyse RATSIRAKA",
      "Janez PODOBNIK"
      ,
      "Henry PUNA",
      "Khatib, Mohammed Seif"
      ,
      "Gudmundur Arni STEFANSSON",
      "Obaid Humaid al-TAYER"
      ,
      "YOO Young-sook",
      "Aananda Prasad POKHERAL"
      ,
      "Timothy TONG Hin-ming",
      "Janis MAZEIKS"
      ,
      "Fernando \"\"Lasama\"\" de ARAUJO",
      "Thomas Motsoahae THABANE"
      ,
      "Lee, Howard Chin",
      "Christian NOYER"
      ,
      "Ramiro VALDES Menendez",
      "Guillermo RISHCHYNSKI"
      ,
      "Abd al-Wahid al-AWADI",
      "Qaranful, Sami"
      ,
      "Mahen JHUGROO",
      "Ruslan KAZAKBAEV"
      ,
      "Daniel SCIOLI",
      "Iuga, Mircea"
      ,
      "KYAW SAN, Colonel",
      "Georgette KOKO"
      ,
      "Adoum GARGOUM",
      "Adao do NASCIMENTO"
      ,
      "Deborah Mae LOVELL",
      "Muhammad bin Abd al-Malik AL AL-SHAYKH"
      ,
      "Julio VELARDE",
      "Yuli EDELSTEIN"
      ,
      "Anna BIJLEVELD",
      "Banoita Tourab SALEH, Doctor"
      ,
      "Aiyaz SAYED-KHAIYUM",
      "Asia Muhammad Ali IDRISS"
      ,
      "Paulo Sergio PASSOS",
      "RYU Mi Yong"
      ,
      "Lohani, Prakash Chandra",
      "ALI MUHSIN al-Ahmar, Lieutenant General"
      ,
      "Kamana, Jean",
      "LETSIE III"
      ,
      "Petr PROKOPOVICH",
      "Paul SWAIN"
      ,
      "HU Jintao",
      "Bogdan KLICH"
      ,
      "Lygia KRAAG-KETELDIJK",
      "Mahesh BASNET"
      ,
      "NUT NINDOEUN (F)",
      "Karl-Heinz GRASSER"
      ,
      "Amadou Boubacar CISSE",
      "Truong My HOA"
      ,
      "Siniora, Fuad",
      "JUNEDIN Sado"
      ,
      "De Mauro, Tullio",
      "Philippe RICHERT"
      ,
      "Sam IDURI",
      "Kanda SIPTEY"
      ,
      "Eyegue Obama Asue, Francisco Pascual",
      "El Fassi, Abbas"
      ,
      "OH Myung",
      "Alfheidur INGADOTTIR"
      ,
      "Waena, Nathaniel",
      "Delano Frank BART"
      ,
      "Geldymukhammed ASHIRMUKHAMEDOV",
      "Jose Eduardo DOS SANTOS"
      ,
      "Ilham ALIYEV",
      "Fillon, Francois"
      ,
      "Rasmussen, Lars Loekke",
      "Steve BLACKETT"
      ,
      "Carlos Alberto AMARANTE BARET",
      "AHMAD bin Jumat, Doctor"
      ,
      "Dewael, Patrick",
      "Paride ANDREOLI"
      ,
      "Henry KAJURA",
      "Phiwayinkhosi MABUZA"
      ,
      "SUN CHANTHOL",
      "Homayoun RASA"
      ,
      "Nkaku KABI",
      "Rajoy, Mariano Brey"
      ,
      "Danilo ASTORI Saragoza",
      "Hassanein, Muhammad Medhat"
      ,
      "Qarase, Laisenia",
      "Mendez Pinelo, Cesar Augusto, Brigadier General"
      ,
      "Qarase, Laisenia",
      "PERNG Fai-nan"
      ,
      "Anerood JUGNAUTH, Sir",
      "KHALID bin Abdallah Al Khalifa"
      ,
      "Yar Muhammad RIND",
      "MSWATI III"
      ,
      "TEA BANH, General",
      "Sirojidin ASLOV"
      ,
      "Jose Antonio GARCIA BELAUNo Diplomatic Exchange",
      "NUTH SOKHOM"
      ,
      "James BABA",
      "Etienne SCHNEIDER"
      ,
      "Muhsin BILAL, Doctor",
      "Adriano MALEIANE"
      ,
      "James MUSONI",
      "Helgi AGUSTSSON",
      'Afful, John Edward',
      'Yasser REDA',
      'Ersumer, Cumhur',
      'Igwe AJA-NWACHUKWU',
      'Ghoul, Omar',
      'Saud NASEIRAT',
      'Adou ASSOA',
      'Benoit OUTTARA',
      'Sultan, Sultan Hamid',
      'PHAN PHIN (CPP)',
      'Ilona JURSEVSKA',
      'Polataivao, Fosi',
      'Adnan BADRAN',
      'SONG Soo-keun',
      'Soccoh KABIA',
      'Naot, Yehudit',
      'Patrick Saidu CONTEH, Doctor',
      'Rosalia CORTE-REAL',
      'Mohammed LOULICHKI',
      'TUNG Hsiang-lung',
      'Yien TUT',
      'Joe OLIVER',
      'Ben Abdallah, Moncef',
      'Zaha WAHEED',
      'Philip BYARUHANGA',
      'El Hossein EL OUARDI',
      'Marto, Michel',
      'Dupont, Christian',
      'PHISIT Li-atham',
      'Lubica LASSAKOVA',
      'Frick, Mario',
      'Ergash SHOISMATOV',
      'Tomka, Peter',
      'Essomba ETOUNDI',
      'Musa, Hamid Majid',
      'ABDALLAH bin Zayid al-Nuhayyan',
      'Muci, Mustafa',
      'CHALEUAN Yapaoher',
      'Latifa AKHERBACH',
      'Marian LUPU',
      'Dsir ADADJA',
      'Henry CHIMUNTHU-BANDA',
      'Houssen Hassan IBRAHIM Minister, Ministry of Justice, Civil Service, Administrative, Administration Reform, Human Rights, &',
      'Celestin NIYONGABO',
      'Mujahid al-QAHALA',
      'Nkongo, Maximin Paul N\'Koue', 'Muhammad Nidal al - SHA’AR', 'Ivan FOSCHI', 'Pelisge HARRISON', 'Nguyen Manh Kiem', 'Jean', 'Fahey,
      John', 'Kuzvart,
      Milos', 'Quliyev,
      Vilayat', 'LIM Swee Say', 'Guillaume LONG', 'LIO Chao - hsuan', 'Alvarado Downing,
      Guillermo', 'Lawan Gana BUBA', 'Guy Mikulu POMBO', 'Clarke,
      Gline', 'Senaviratne,
      Athauda', 'Mohamed EL OUAFA', 'Isch,
      Edgar', 'Steve MAHAREY', 'Oleh PROSKURYAKOV', 'Aghvan VARDANYAN', 'Rosa Bautista,
      Leonidas', 'Augusto DOS SANTOS', 'Maria de Fatima Monteiro JARodger Dodger !
        IM', 'Luis Enrique MONTERROSO', 'Hubert OULAYE', 'Michael KEENAN', 'Latpov,
      Ural', 'Anwar Muhammad al - GARGASH', 'Kazem VAZIRI - Hamaneh', 'Wellington SANDOVAL', 'Mangoaela,
      Percy Metsing', 'Omar MANSOUR', 'Temirbek KURMANBEKOV', 'Hamlaoui,
      Yahia', 'Saleem MANDVIWALLA', 'Hassan HARUNA', 'Hemida Ould Ahmed TALEB', 'Horacio SEVILLA Borja', 'PRASERT Boonchaisuk', 'Souley,
      Hassane', 'Andre Ringui LE GAILLARodger Dodger ! ', 'Bahr Idris ABU GARodger Dodger !
        A', 'Fouad Ali EL - HIMMA', 'Bessie Reen KACHERE', 'Jacques Ulrich RANDRIANTIANA', 'Djibril Yipene BASSOLE', 'John MUTORWA', 'Jovanovic,
      Vladislav', 'Saud,
      ABD AL - AZIZ bin Fahd bin Abd', 'Tokyo SEXWALE', 'Amina EL - GUINDI', 'Boubaker EL -
        AKHZOURI', 'Rill,
      Anton', 'Amos KIMUNYA', 'Imendia,
      Francisco', 'Hamud Muhammad ABAD', 'Flores Facusse,
      Carlos Roberto', 'Peter Paire O\'NEILL',
      'Dias, Guilherme Gomes',
      'Tonis LUKAS',
      'Casali, Augusto',
      'MAENG Hyung-kyu',
      'Matteo FIORINI',
      'Babamyrat TAGANOW',
      'Webb, Maurine',
      'Dayasritha TISSERA',
      'Cristobal Menana ELA',
      'Brasseur, Anne',
      'Muhammetguly OGSHUKOV',
      'Matuq, Abdallah al-',
      'Godfridah Nsenduluka SUMAILI',
      'Mountaga TALL',
      'Yondo, Maurice',
      'Levai, Katalin',
      'Serra, Joao',
      'Bornito De Sousa Baltazar DIOGO',
      'Ravil SAFIULLIN',
      'Satya Veyash FAUGOO',
      'Svetozar MAROVIC',
      'Ancil ANTOINE',
      'Abdi Ibrahim Absieh',
      'Karamatov, Hamidulla',
      'Imbert, Colm',
      'CHAN NYEIN',
      'Nancy BAKIR',
      'Alvear Valenzuela, Maria Soledad',
      'Patricia GORodger Dodger!ON-PAMPLIN',
      'MONGKHON Na Songkhla, Doctor',
      'Georgios BABINIOTIS',
      'Mendes, Luis Olundo',
      'David HARUTYUNYAN',
      'Milutinovic, Milan',
      'Maris KUCINSKIS',
      'Nabil Mohamed AHMED, Doctor',
      'Krishna Bahadur MAHARA',
      'Capoulas Santos, Luis Manuel',
      'Ibrahim al-JAZI',
      'Pecek, Zeljko',
      'Tshipasa, Venant',
      'Octavio SANCHEZ',
      'ABDUL RAHMAN bin Mohamed Taib',
      'Wisdom, Neville',
      'Abdullah al-THINI',
      'Cidalia CHAUQUE',
      'Cheikh Bamba DIEYE',
      'Snjezana SOLDAT',
      'Dona Jean-Claude HOUSSOU',
      'Jorge Alberto MOLINA Contreras',
      'Mohamed Ould Mohamed Abderrahmane Ould MOINE',
      'Te Ururoa FLAVELL',
      'Maggie BARRY',
      'Rakam CHEMJONG',
      'Amr EZZAT SALAMA',
      'Stepan KUBIV',
      'Ichinkhorloo ERDENEBAATAR',
      'Oscar MARTINEZ Doldan',
      'Salehuddin AHMED',
      'Lina Dolores POHL Alfaro',
      'Gabriel Mosima “Tokyo” SEXWALE',
      'Babatune OSOTIMEHIN',
      'Nacer MEHAL',
      'Christopher Kajoro CHIZA',
      'Jose Luis CANCELA',
      'Khushiram, Khushhal',
      'Kimmo TILLIKAINEN',
      'Spartak SEYRANIAN',
      'Son Chong-ho',
      'Weerawanni, Samaraweera',
      'Jagmohan',
      'Ronell GILES',
      'Adama BICTOGO',
      'Marino MURILLO Jorge',
      'Hamadou MUSTAPHA',
      'Eliseo RIO, Jr.',
      'Malan, Pedro',
      'Magtymguly BAYRAMDURDYYEW',
      'Lang, Jack',
      'Antonio de Aguiar PATRIOTA',
      'Abdul Razaq WAHIDI',
      'Rita, Cosme Afonso Da Trindade',
      ' Obiang, Rene Ndemezo',
      'Rebeca SANTOS',
      'Arpad ERSEK',
      'Mabandla, Bridgette',
      'al-Fayez, Faisal',
      'Anwar Muhammad GARGASH',
      'Rup JYOTI',
      'Gerry RITZ',
      'Solange Pagonendji NDACKALA',
      'CHUNG Dong-chea',
      'Nunzia DE GIROLAMO',
      'Rashid Hamad Muhammad al-HAMAD',
      'Spatafora, Marcello',
      'Hery RAJAONARIMAMPIANINA',
      'Shatwan, Ahmad Fathi ibn',
      'Van Dunem, Oswaldo de Jesus Serra',
      'Mba, Fernando Mabale',
      'Jabulani MABUZA',
      'Mwakwere, Chirau Ali',
      'Alain Guillaumme BUNYONI',
      'Khalid TOUQAN',
      'Mikael DAMBERG',
      'Stagno, Bruno',
      'PAK Song-ch\'ol', 'Abdallah Awabil MANTHUQ', 'Jose HERNANo Diplomatic ExchangeZ Bernardez', 'Halima Tayo ALAO', 'Chaves Bolanos,
      Javier', 'Carlos Alberto DUBOY Sierra', 'Artur SILVA', 'Song Chong - ho', 'Jadranka KOSOR', 'ANSARI,
      Majid,
      Hojjat ol - Eslam', 'Petre TSISKARISHVILI', 'Adoum GARGOUM', 'Ollanta Moises HUMALA Tasso', 'Nduwimana,
      Onesime', 'Mark WOYONGO', 'Salwai,
      Charlot', 'Rania ABDEL MONIM,
      Doctor', 'Andrzej CZUMA', 'Diego FUENTES Acosta', 'Rasit MEREDOW', 'YEO Yong Boon,
      George,
      Brigadier General', 'Limam Ould TEGUEDI', 'Bicakcic,
      Edhem', 'Stuart Rowland ROBERT', 'Kumbakor,
      Andrew', 'Patil,
      Shivraj', 'Salamat AZIMI', 'Fio Selafi Joseph Purcell LAUTAFI', 'FELIPE VI', 'Sheila TLOU', 'Gusmao,
      Jose Alexander', 'Zypries,
      Brigitte', 'Dithny Joan RATON', 'Bassam AWADALLAH', 'Othom Rago AJAK', 'Xavier CASAL Rodriguez', 'Dube,
      Alfred', 'Lucian Puiu GEORGESCU', 'Provoste,
      Yasta', 'Djigui CAMARA', 'Kazhmurat NAGMANOV', 'SUWAPHAN Tanyuwattana', 'Michael MOROSKY,
      Sir', 'Agni Prasad KHAREL', 'Johnnie K. SWARTZ', 'Koimdodov,
      Kozidavlat', 'Lodhi,
      Maleeha', 'Ali OSOBLE', 'Frederique VIDAL', 'Pape Gorgui NDONG', 'Nikolaos PAPPAS', 'Edita HRodger Dodger !
        A', 'Justin NDIORO', 'Persis NAMUGANZA', 'Dinesh TRIVEDI', 'Salah JARRAR', 'Antoni JASZCZUK', 'Amadou SOUMAHORO', 'Enrique MENDOZA Ramirez', 'Mandandi,
      Godden', 'Ibrahima KOUROUMA', 'Martins ROZE', 'Daniele BODINI', 'Shamsi,
      Abd al - Aziz bin Nasir al - ', 'Atef OBEIDAT', 'Ali Abd al - Aziz al -
        ISSAWI', 'Arun SINGH', 'Orlando Celso GARCIA Ramirez', 'Benedikt JOHANNESSON', 'Kahinda OTAFIIRE,
      Colonel', 'Sharaf,
      Ali Hamid al - ,
      Captain', 'Viktor TOPOLOV', 'Kalinic,
      Dragan', 'Salah Al Sayyed YOUSUF FARAG', 'Henry OKELLO ORYEM', 'CHU Ching -
        yi (a.k.a. Cyrus CHU)', 'Aksenenko,
      Nikolay Yemelyanovich', 'Margaret Mhango MWANAKATWE', 'Mamadou SIDIBE', 'Samuel SANTOS Lopez', 'Saad Al KHARABSHEH', 'O Kuk Ryol,
      General', 'Kouyate,
      Oumare', 'Sanoussy Bantama SOW', 'Rawhani,
      Abd al - Wahhab al - ', 'Lagos Pizzati,
      Victor Manuel', 'LEE Ju - ho', 'Michael MISKIN', 'Eila,
      Mohammed Tahir', 'Jean - Pierre DARUWEZI Mokombe', 'Rawdhan Abd al - Aziz al -
        RAWDHAN', 'Suat KILIC', 'Abdallah al - RABYA', 'Lu,
      Annette', 'Abdallah Sulayman Abdallah Sulayman,
      Professor', 'Hasan QAZIZADEH - Hashemi', 'Christabel NJIMBU', 'ABD al -
        Aziz bin Atiyatallah al - Khalifa', 'Frafjord Johnson,
      Hilde', 'Rock,
      Allan', 'Henrik Sass LARSEN', 'Bao,
      Yuntuvi', 'Naftali BENNETT', 'Jelena PIA - COMELLA', 'Mercedes JUAN LOPEZ', 'KIM Dong -
        yeon', 'Alberto RIGAIL Arosemena', 'Gil,
      Rosalia', 'Alfredo GOMEZ Urcuyo', 'Dalibor STYS', 'RI Kwang Gon', 'Rene FIGUEROA', 'Danilo TONINELLI', 'Hector DADA Hirezi', 'Claudio BISOGNIERO', 'Kabir HASHIM', 'Lin,
      Feng - mei', 'Diakite Aissata TRAORE', 'Linda Amalia Sari GUMELAR', 'Velasquez,
      Alfonso', 'Mohammad Jakir HUSSEIN', 'Ferrero - Waldner,
      Benita', 'Miguel,
      Girlyn', 'Michel BONGONGO', 'Masumeh EBTEKAR', 'Abdullah GUL', 'Greg CLARK', 'Mansour FAYE', 'Abd al -
        Rahman Muhammad SHALGHAM', 'Abd al - Rahman Muhammad al - OWAIS', 'Ashton GRANEAU', 'Algirdas BUTKEVICIUS', 'Manohar PARRIKAR', 'Benjamin,
      Charlie', 'Georgievski,
      Ljubco', 'MAH Bow Tan', 'Bergen,
      Ernst', 'M. Hatta RAJASA', 'Mohamed GHAZI', 'El - Mursi HEGAZY', 'Reem bin Ibrahim al -
        HASHIMI', 'Serhiy TIHIPKO', 'Nezdet MUSTAFA', 'Vincent SSESMPIJJA', 'Joaquin ZEVALLOS', 'Tinatin KHIDASHELI', 'Gallardo,
      Jorge', 'John Luk JOK', 'Merete RIISAGER', 'Nanan,
      Adesh', 'Narayanan,
      Kocheril Raman', 'Dharmendra PRADHAN', 'Cristian LARROULET Vignau', 'Paulo KASSOMA', 'Jawad Karim al -
        BULANI', 'Cao Duc PHAT', 'Marcos JORGE de Lima', 'Alfonso DASTIS Quecedo', 'Hmeyda,
      Zeidane Ould', 'Do Trung Ta', 'Eliud Ulises AYALA Zamora', 'Zdena ABAZAGIC', 'Ronald JUMEAU', 'Rodolfo MEDINA', 'Philip Bruce GOFF', 'Haiman EL TROUDI', 'Marisol ARGUETA de Barillas', 'Herrera Tello,
      Maria Teresa', 'Ricardo Alberto ARIAS Arias', 'William Ni’i HAOMAE', 'Petsalnikos,
      Filippos', 'Nelu Ioan BOTIS', 'Toledo,
      Alejandro', 'Elvia Violeta MENJIVAR Escalante', 'Neil PARSAN', 'Beyshenaliyeva,
      Neliya', 'Sarr,
      Oumar', 'Aniceto EBIAKA Mohote', 'Manuel GONZALEZ Sanz', 'Falah Hasan al -
        ZAYDAN', 'Mavroyiannis,
      Andreas', 'Jeannette SANCHEZ', 'Karl - Theodor zu GUTTENBERG', 'Kuzmuk,
      Oleksandr', 'Lovden,
      Lars - Erik', 'Ylli MANJANI', 'Ganoo,
      Alan', 'Navarrete Lopez,
      Jorge', 'Carolina RENTERIA', 'Sadun Farhan al - DULAYMI', 'Betty TOLA', 'Sarah bint Yousef al -
        AMIRI', 'Gayibov,
      Charymammed', 'RASHID bin Abdallah Al Nuaymi', 'David LITTLEPROUD', 'Eduard GRAMA', 'Ze’ev BOIM', 'Pavlopoulos,
      Prokopis', 'Kelly O\'DWYER',
      'Kerekou, Mathieu',
      'Mali Malie, Mpho') %>% as.data.table() %>% dplyr::rename(., "name" = ".") %>% return()
  }
  
  
  
}

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
    .[str_detect(column2, "\\´"), column2 := stringr::str_remove(column2, pattern = "\\´")] %>%
    .[str_detect(column2, "\\ʿ"), column2 := stringr::str_remove(column2, pattern = "\\ʿ")] %>%
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
str_trim_ws_iterate <- function(string, whitespace=" "){
  
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
get_common_tites <- function(type = "educ_period") {

  possible_titles <-c(
    "educ_all",
    "educ_unambiguous",
    "educ_period",
    "military",
    "military_unambiguous",
    "poli",
    "oth", 
    "unambiguous")
  
  if (!(type %in% possible_titles)) {
    
    warning <- possible_titles %>%
      glue::glue_collapse(x = ., sep = ", ") %>%
      paste0("Possible types are: ", .)  
    warning(warning)
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
  if (type == "educ_unambiguous"|type == "unambiguous") {
    out <- c(
      "DOCTOR",
      "ESQUIRE", 
      "PROFESSOR"
    ) 
    
    if(type == "unambiguous"){
      unambiguous <- out
    }
    
    }
  
  if (type == "educ_period") {
    out <- c(
      "B\\.A\\.",
      "MR\\." ,
      "MSR\\." ,
      "MS\\.",
      "BSC\\.",
      "MSC\\.",
      "MBA\\.",
      "MA\\." ,
      "MD\\." ,
      "PH\\.D\\.",
      "PHD\\." ,
      "DR\\." ,
      "LLM\\." ,
      "LLB\\.",
      "PROF\\.",
      "ENG\\."
    ) 
    
    
  }
  
  # military
  if (type == "military") {
    
    out <-
      c(
        'COMMAND CHIEF MASTER SERGEANT',
        'MASTER CHIEF PETTY OFFICER',
        'SENIOR CHIEF PETTY OFFICER',
        'COMMAND CHIEF MASTER SGT\\.',
        'LIEUTENANT JUNIOR GRADE',
        'MASTER GUNNERY SERGEANT',
        'SENIOR MASTER SERGEANT',
        'COMMAND SERGEANT MAJOR',
        'CHIEF WARRANT OFFICER',
        'CHIEF MASTER SERGEANT',
        'SERGEANT FIRST CLASS',
        'LIEUTENANT COMMANDER',
        'PRIVATE FIRST CLASS',
        'CHIEF PETTY OFFICER',
        'LIEUTENANT GENERAL',
        'LIEUTENANT COLONEL',
        'OF THE MARINE CORP',
        'SECOND LIEUTENANT',
        'BRIGADIER GENERAL',
        'COMMAND SGT\\. MAJ\\.',
        'SEAMAN APPRENTICE',
        'FIRST LIEUTENANT',
        'OF THE AIR FORCE',
        'MASTER SGT\\. MAJ\\.',
        'GUNNERY SERGEANT',
        'WARRANT OFFICER',
        'MASTER SERGEANT',
        'FIRST SERGEANT',
        'SERGEANT MAJOR',
        'SGT\\. 1ST CLASS',
        'STAFF SERGEANT',
        'SEAMAN RECRUIT',
        'LANCE CORPORAL',
        'SENIOR AIRMAN',
        'MAJOR GENERAL',
        'PETTY OFFICER',
        'AIRMAN BASIC',
        'VICE ADMIRAL',
        'REAR ADMIRAL',
        'SECOND CLASS',
        'GUNNERY SGT\\.',
        'MASTER SGT\\.',
        'OF THE ARMY',
        'FIRST CLASS',
        'THIRD CLASS',
        'OF THE NAVY',
        'SECOND LT\\.',
        'CHIEF SGT\\.',
        'BRIG\\. GEN\\.',
        'STAFF SGT\\.',
        'SPECIALIST',
        'LIEUTENANT',
        'LANCE CPL\\.',
        'TECHNICAL',
        'MAJ\\. GEN\\.',
        'SGT\\. MAJ\\.',
        'VICE ADM\\.',
        'REAR ADM\\.',
        'COMMANDER',
        'LT\\. CMDR\\.',
        '1ST CLASS',
        '2ND CLASS',
        '3RD CLASS',
        '(RETIRED)',
        'AIR CHIEF',
        'RESERVIST',
        'SERGEANT',
        'LT\\. GEN\\.',
        'LT\\. COL\\.',
        '1ST SGT\\.',
        'CORPORAL',
        'LT\\. J\\.G\\.',
        'GENERAL',
        '1ST LT\\.',
        'COLONEL',
        'CAPTAIN',
        'ADMIRAL',
        'PRIVATE',
        'RESERVE',
        'AIRMAN',
        'ENSIGN',
        'SEAMAN',
        '(RET\\.)',
        'STAFF',
        'MAJOR',
        'CAPT\\.',
        'CMDR\\.',
        'BRIG\\.',
        'RADM\\.',
        'CORPS',
        'SGT\\.',
        'MAJ\\.',
        'GEN\\.',
        'COL\\.',
        'CPL\\.',
        'SPC\\.',
        'ADM\\.',
        'PFC\\.',
        'PVT\\.',
        'DIV\\.',
        'MAR\\.',
        'RES\\.',
        'LT\\.',
        'FD\\.'
      )
    
  }
  
  # military
  if (type == "military_unambiguous"|type == "unambiguous") {
    
    out <-
      c(
        'COMMAND CHIEF MASTER SERGEANT',
        'MASTER CHIEF PETTY OFFICER',
        'SENIOR CHIEF PETTY OFFICER',
        'LIEUTENANT JUNIOR GRADE',
        'MASTER GUNNERY SERGEANT',
        'SENIOR MASTER SERGEANT',
        'COMMAND SERGEANT MAJOR',
        'CHIEF WARRANT OFFICER',
        'CHIEF MASTER SERGEANT',
        'SERGEANT FIRST CLASS',
        'LIEUTENANT COMMANDER',
        'PRIVATE FIRST CLASS',
        'CHIEF PETTY OFFICER',
        'LIEUTENANT GENERAL',
        'LIEUTENANT COLONEL',
        'OF THE MARINE CORP',
        'SECOND LIEUTENANT',
        'BRIGADIER GENERAL',
        'SEAMAN APPRENTICE',
        'FIRST LIEUTENANT',
        'OF THE AIR FORCE',
        'GUNNERY SERGEANT',
        'WARRANT OFFICER',
        'MASTER SERGEANT',
        'FIRST SERGEANT',
        'SERGEANT MAJOR',
        'STAFF SERGEANT',
        'SEAMAN RECRUIT',
        'LANCE CORPORAL',
        'SENIOR AIRMAN',
        'MAJOR GENERAL',
        'PETTY OFFICER',
        'AIRMAN BASIC',
        'VICE ADMIRAL',
        'REAR ADMIRAL',
        'SECOND CLASS',
        'OF THE ARMY',
        'FIRST CLASS',
        'THIRD CLASS',
        'OF THE NAVY',
        'SPECIALIST',
        'LIEUTENANT',
        'TECHNICAL',
        'COMMANDER',
        '1ST CLASS',
        '2ND CLASS',
        '3RD CLASS',
        '(RETIRED)',
        'AIR CHIEF',
        'RESERVIST',
        'SERGEANT',
        'CORPORAL',
        'GENERAL',
        'COLONEL',
        'CAPTAIN',
        'ADMIRAL',
        'PRIVATE',
        'RESERVE'#,
        #'MAJOR'
      )
    
    if(type == "unambiguous"){
      unambiguous <- append(unambiguous, out)
    }
    
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
      "MIN\\.",
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
  
  if(type == "unambiguous"){
    out <- unambiguous  
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
str_remove_all_common_titles <- function(vector, prefix="", suffix="", specific=NULL){
  
  if(is.null(specific)){
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
  }else{
    common_titles <- specific
  }
  
  for(title in common_titles){
    
    title <- paste0(prefix, title, suffix)
    
    paste0("Removing common title in parenthesis: (", title,"). This includes the specified prefix & suffix.") %>% 
      message_with_lines()
    
    vector <- str_remove_all(string = vector, pattern = title) 
    
  }
  
  vector %>% 
    # DROP double commas again
    str_replace_all(., ",,", ",")   %>% 
    str_replace_all(., ", ,", ",")   %>% 
    return()
}

# Section 3.12.2: drop common titles from strings: helper functions -------
str_remove_all_common_titles_startswith <- function(vector, prefix="", suffix="", specific=NULL){
  
  if(is.null(specific)){
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
  }else{
    common_titles <- specific
  }
  
  # prepare vector as datatable
  vector_dt <- vector %>% data.table(vector=.) %>% 
    .[, internal_order := 1:.N] 
  
  # for each title, remove it from the data 
  for(title in common_titles){
    
    title <- paste0(prefix, title, suffix)
    
    vector_dt[startsWith(vector, title),  vector := str_remove_all(string = vector, pattern = title)]
    
  }
  
  vector_dt %>% 
    .[order(internal_order)] %>% 
    .[, vector] %>% 
    return()
  
}

# Section 3.12.3: drop common titles from strings: helper functions -------
str_remove_all_common_titles_endswith <- function(vector, prefix="", suffix="", specific=NULL){
  
  if(is.null(specific)){
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
  }else{
    common_titles <- specific
  }
  
  # prepare vector as datatable
  vector_dt <- vector %>% data.table(vector=.) %>% 
    .[, internal_order := 1:.N] 
  
  # for each title, remove it from the data 
  for(title in common_titles){
    
    title <- paste0(prefix, title, suffix)
    
    vector_dt[endsWith(x = vector, suffix = title),  vector := str_remove_all(string = vector, pattern = title)]
    
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

################################################################################
# Section 6: print things  ###########################
################################################################################

# Section 6.1.1: simply print out a line ----
print_line <- function(){
  print("---------------------------------------------------")
}

# Section 6.1.2: print out a line, a message, then another line ----
message_with_lines <- function(message_text){
  message("---------------------------------------------------")
  message(message_text)
  message("---------------------------------------------------")
  
}

# Section 6.2: return output in the style of a vector input: incredibly useful when working with R ----
return_in_vector_format <- function(x){
  
  # if its a character vector
  if(is.character(x)){
    
    out <- x %>% 
      paste(., collapse = "', '") %>% 
      paste0("'", . , "'") %>%
      paste0("c(", . , ")")
    
  }
  
  # if its a numeric vector
  if(is.numeric(x)){
    
    out <- x %>% 
      paste(., collapse = ", ") %>% 
      paste0("c(",. , ")")
    
  }
  
  return(out)
  
  
}
