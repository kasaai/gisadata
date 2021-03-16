library(readxl)
library(ChainLadder)
library(openxlsx)
options(scipen = 999)


print_cl_results <- function(filename){
  
  cl_results <- function(amount){
    
    ### this function is fed either with the dataframe of paid or incurred from the excel files in the folder
    
    amount %>% select(accident_half_year) %>% bind_cols(
      (amount %>% select(-accident_half_year) %>%
         as.matrix %>% as.triangle() %>% 
         MackChainLadder() %>% summary)[[1]][,c(1,3,4)]
    ) %>% return()
    
    
  }
  
  amounts_vector <- c("paid_amounts","incurred_amounts")
  
  amounts_vector %>% map(~read_excel(filename, 
                                     sheet = .)
  ) %>% map(~cl_results(.)) %>% set_names(amounts_vector) %>% 
    write.xlsx(paste0(str_remove(filename,("\\.xlsx$")),"_Chain_Ladder.xlsx"))
  
}

list.files()[3] %>% print_cl_results()


print_cape_cod_results <- function(filename){

  cape_cod <- function(amount,exposure){
    
    pattern <- (amount %>% select(-accident_half_year) %>%
                  as.matrix %>% as.triangle() %>% MackChainLadder() %>% summary())[[1]][,2]
    
    uup <- (pattern * exposure) %>% sum
    
    latest <- (amount %>% select(-accident_half_year) %>%
                 as.matrix %>% as.triangle() %>% MackChainLadder() %>% summary())[[1]][,1]
    
    lr_implied <- (latest %>% sum) / uup
    
    cape_cod_ultimate <- latest + (1 - pattern) * lr_implied * exposure
    
    tibble(
      
      amount[,1],
      Latest = latest,
      Ultimate = cape_cod_ultimate,
      IBNR = Ultimate - Latest
    ) %>% return()
    
    
  }
    
  amounts_vector <- c("paid_amounts","incurred_amounts")
  
  amounts_list <- amounts_vector %>% map(~read_excel(filename, 
                                                     sheet = .)
                                         )
  
  premium_list <- read_excel(filename, sheet = "premiums_exposures")
  
  amounts_list %>% map(~cape_cod(.,premium_list %>% pull(premium_earned))) %>% set_names(amounts_vector) %>% 
  write.xlsx(paste0(str_remove(filename,("\\.xlsx$")),"_Cape_cod.xlsx"))
  
  
}

print_cape_cod_results(list.files()[3])










