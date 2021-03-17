library(readxl)
library(ChainLadder)
library(openxlsx)
library(tidyverse)

print_cl_results <- function(filename){

  cl_results <- function(amount){

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

  amounts_list <- amounts_vector %>% map(~read_excel(filename, sheet = .))

  premium_list <- read_excel(filename, sheet = "premiums_exposures")

  amounts_list %>% map(~cape_cod(.,premium_list %>% pull(premium_earned))) %>% set_names(amounts_vector) %>%
  write.xlsx(paste0(str_remove(filename,("\\.xlsx$")),"_Cape_Cod.xlsx"))


}



print_cl_results("Alberta_Collision.xlsx")
print_cl_results("Alberta_TPL_BI.xlsx")
print_cl_results("Atlantic_Canada_Collision.xlsx")
print_cl_results("Atlantic_Canada_TPL_BI.xlsx")
print_cl_results("Nova_Scotia_Collision.xlsx")
print_cl_results("Nova_Scotia_TPL_BI.xlsx")
print_cl_results("Ontario_AB_DI.xlsx")
print_cl_results("Ontario_AB_MR.xlsx")
print_cl_results("Ontario_Collision.xlsx")
print_cl_results("Ontario_TPL_BI.xlsx")

print_cape_cod_results("Alberta_Collision.xlsx")
print_cape_cod_results("Alberta_TPL_BI.xlsx")
print_cape_cod_results("Atlantic_Canada_Collision.xlsx")
print_cape_cod_results("Atlantic_Canada_TPL_BI.xlsx")
print_cape_cod_results("Nova_Scotia_Collision.xlsx")
print_cape_cod_results("Nova_Scotia_TPL_BI.xlsx")
print_cape_cod_results("Ontario_AB_DI.xlsx")
print_cape_cod_results("Ontario_AB_MR.xlsx")
print_cape_cod_results("Ontario_Collision.xlsx")
print_cape_cod_results("Ontario_TPL_BI.xlsx")


