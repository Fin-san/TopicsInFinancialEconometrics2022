# Seminar - Topics in Financial Econometrics

# Libraries

library(dplyr)
library(RSQLite)
library(tidyverse)
library(zoo)
library(urca)
library(vars)
library(VARtests)
library(tsDyn)
library(forecast)
library(tseries)
library(Rfast)
library(spacetime)
library(lubridate)
library(itertools)

## Data collection and manipulation
tidy_finance <- dbConnect(SQLite(),"C:/Users/jeope/OneDrive/Skrivebord/tidy_finance.sqlite",extended_types=TRUE)

# Loading data
crsp_daily <- tbl(tidy_finance, 'crsp_daily') %>% collect() %>%
  dplyr::select(permno, date, ret_excess)
crsp_monthly <- tbl(tidy_finance, 'crsp_monthly') %>% collect()
crsp_d <- crsp_daily %>% filter(date > '2005-01-01'& date < '2015-01-01')
{
  
crsp_m <- crsp_monthly %>% 
  filter(date > '2005-10-01', date < '2008-01-01') %>%
  dplyr::select(permno, industry)

# Joining tables to add 'industry'
crsp <- crsp_d %>%
  left_join(crsp_m, by = "permno")

# Dropping columns with NA values and sorting crsp by date 
crsp <- crsp %>% distinct(permno, date, .keep_all = TRUE)

# Checking correlation to find the largest correlation within industries
corr_crsp <- crsp %>%
 group_by(industry) %>%
  summarize(cor=cor(permno, ret_excess))
}

crsp_filtered <- crsp_d %>%
   #filter(industry %in% c('Public', "Construction", "Utilities")) %>%
  pivot_wider(., id_cols = date, names_from = permno, values_from = ret_excess)
crsp_filtered <- crsp_filtered[, 1:1000]

wild_coint <- function(df) {
  crsp_nodate <- subset(df, select = -c(date))
  
  # Convert returns to prices
  crsp_modified <- crsp_nodate # Note: First column name should be changed according to data set
  crsp_modified_prices <- (cumprod(crsp_modified+1))*100 
  
  for (i in 1:length(crsp_modified_prices)) {
    crsp_modified_prices[ , i] <- (crsp_modified_prices[ , i]/crsp_modified_prices[1, i])*100 
  }
  
  crsp_modified_prices <- crsp_modified_prices[ , colSums(is.na(crsp_modified_prices)) == 0]

print(crsp_modified_prices)
# Finding all combinations of pairs
pairs <- as.data.frame(combn(1:(dim(crsp_modified_prices)[2]), 2))

# Misspecification tests
misspecification_autocorrelation_detected <- numeric(ncol(pairs))
misspecification_arch_detected <- numeric(ncol(pairs))
misspecification_normality_detected <- numeric(ncol(pairs))
no_cointegration_rejected <- numeric(ncol(pairs)) 
ca_jo_trace_test_stats <- numeric(ncol(pairs))
ca_jo_trace_crit_vals <- numeric(ncol(pairs))
selected_lag <- numeric(ncol(pairs))
used_lag <- numeric(ncol(pairs))
i = 0 
# Looping over pairs

pvalues <- list()
for (n_pair in 1:ncol(pairs)) {
  
  # Declare Time Series Objects
  pair <- pairs[, n_pair]

  ts1 <- ts(crsp_modified_prices[,pair[1]], frequency = 1) 
  ts2 <- ts(crsp_modified_prices[,pair[2]], frequency = 1)
  
  # Bind into a system
  dset <- cbind(ts1,ts2)
  
  # Test lag length for pairs
  lagselect <- VARselect(dset, lag.max = 20, type = "trend")
  selected_lag[n_pair] <- lagselect$selection[1]
  if(selected_lag[n_pair] == 1){
    used_lag[n_pair] <- which.min(lagselect$criteria[1, -1]) + 1
  } else {
    used_lag[n_pair] <- selected_lag[n_pair]
  }
  
  
  fitted_var_model <- VAR(dset, p = used_lag[n_pair], type ="trend")
  
  # Misspecifications tests (only testing for serial correlation)
  misspecification_autocorrelation_detected[n_pair] <- serial.test(fitted_var_model, lags.bg = used_lag[n_pair], type = "BG")$serial$p.value
  misspecification_arch_detected[n_pair] <- 1 # arch.test(fitted_var_model)$arch$p.value 
  misspecification_normality_detected[n_pair] <- 1 # normality.test(fitted_var_model)$jb.mul$JB$p.value
  
  # If the residuals suffer from serial correlation discard and move on to the next model
  
  if(min(misspecification_autocorrelation_detected[n_pair], 
         misspecification_arch_detected[n_pair],
         misspecification_normality_detected[n_pair])  < 0.05) {
    next
  }
  
# Bootstrap testing the co-integration rank
i = i +1

ctrace <- cointBootTest(dset, r = "sequence", p = used_lag[n_pair], model = 3, signif = 0.05, B = 399, boot_type = "WB", WB_dist = "rademacher")

ca_jo_trace_test_stats[n_pair] <- ctrace$WB.pv[1] 
no_cointegration_rejected[n_pair] <- (ca_jo_trace_test_stats[n_pair] < 0.05 )

if (ca_jo_trace_test_stats[n_pair] < 0.05) {
  pvalues <- append(pvalues, ctrace$WB.pv[1] )
  }

}


pairs_with_cointegration <- pairs[,which(no_cointegration_rejected==1)]

named_pairs_with_cointegration <- apply(pairs_with_cointegration, 2, function(r) colnames(crsp_modified_prices)[r])

  
  #FINDSENS PLAN B
 # pairs_df = data.frame(stocka=c('første'), stockb=c('anden'))
  
#  for (value in colnames(pairs_with_cointegration)) {
#    
#    pairs <- as.list(t(pairs_with_cointegration[value]))
#    
#    first_value <- as.integer(pairs[1])
#    second_value <- as.integer(pairs[2])
#    
#    name1 <- colnames(crsp_modified_prices)[first_value]
#    name2 <- colnames(crsp_modified_prices)[second_value]
    
    
#    pairs_df[nrow(pairs_df) + 1,] = c(name1,name2)
#  }
  
  
  
  
  
  
# Getting summaries for CVAR models

#var_with_cointegration <- list()



#for (n_pair in 1:ncol(pairs_with_cointegration)) {
  #pair <- pairs_with_cointegration[, n_pair]
  
  #name_1 <- named_pairs_with_cointegration[1, n_pair]
  #name_2 <- named_pairs_with_cointegration[2, n_pair]
  
  #beta <- var_with_cointegration_boot[[n_pair]]$beta[2,1]
  
  #rho <- var_with_cointegration_boot[[n_pair]]$rho[1,1]
  
  #alpha <- var_with_cointegration_boot[[n_pair]]$alpha[1,1]
  
  #roots <- Rfast::nth(Re(sort(var_with_cointegration_boot[[n_pair]]$companion_eigen[[2]])),2, descending = T)
  
  # Removing beta-values not between -0.5 and 2. 
  
  #if (!between(beta, -0.5, 2)) {
   # next
  #}
  # Finding the second largest unrestricted eigenvalue in the companion matrix. (Speed of convergence to equilibrium)
  #if ((roots > 0.97)  ) {
   # next
  #}
  
  #cointegrated_residual <- crsp_modified_prices[,pair[1]] - crsp_modified_prices[,pair[2]] - rho
  
  #cointegrated_residual_mean <- mean(cointegrated_residual[1:2265])
  #cointegrated_residual_sd <- sd(cointegrated_residual[1:2265])
  
  #z_scores_selected[[n_pair]] <- (cointegrated_residual[2265:2513] - cointegrated_residual_mean)/cointegrated_residual_sd
  #roots_selected[[n_pair]] <- Rfast::nth(Re(sort(var_with_cointegration_boot[[n_pair]]$companion_eigen[[2]])),2, descending = T)
  
  #count_number_of_pairs_after_beta_adjust <- count_number_of_pairs_after_beta_adjust+1
  
  #eq <- paste(n_pair, ":", name_1, "=", round(beta, 3), "*", name_2, "+", round(rho, 3))
  
  #equation_of_selected_pairs[count_number_of_pairs_after_beta_adjust] <- eq
  #pairs_selected[count_number_of_pairs_after_beta_adjust] <- n_pair
  
  #cat(eq, file=summary_file, append=TRUE, sep = "\n")
  

#cat(paste("Number of pairs:", count_number_of_pairs_after_beta_adjust), file = summary_file, append = TRUE, sep ="\n")
return(list(named_pairs_with_cointegration,pvalues))
}

start_date <- as.Date('2011-01-01')
end_date <- as.Date('2014-01-01')

rebalance <- seq(start_date,end_date, "months")

stocka <- list()
stockb <- list()
date_py <- list()
pvalues <- list()

for (tid in rebalance) {
  df <- crsp_filtered %>% filter(date <= as.Date(tid) & date >= as.Date(tid) - years(5))
  coint_wild_results <- wild_coint(df)
  coint_wild <- data.frame(coint_wild_results[1]) 
  stocka <- append(stocka,unname(t(coint_wild)[,1]))
  stockb<- append(stockb,unname(t(coint_wild)[,2]))
  date_py <- append(date_py,as.list(rep(as.Date(tid),times=length(unname(t(coint_wild)[,1])))))
  pvalues <- append(pvalues,unname(data.frame(coint_wild_results[2])))
} 

main_data <- cbind(
  date_py,
  stocka,
  stockb,
  pvalues
)
