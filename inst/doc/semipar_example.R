## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(healthcare.antitrust)
data(discharge_data, package = "healthcare.antitrust")

## -----------------------------------------------------------------------------
list1 <- c("drg","age","zip5")
layers <- list(list1)

## -----------------------------------------------------------------------------
th <- 15

## -----------------------------------------------------------------------------
discharge_data$count <- 1
outList <- cell_defn(discharge_data,th,layers)

## -----------------------------------------------------------------------------
D0 <- outList$assigned
print(nrow(D0))
print(nrow(discharge_data))

## -----------------------------------------------------------------------------
discharge_data$zip3 <- floor(discharge_data$zip5/100)
list1 <- c("drg","age","zip5")
list2 <- c("drg","age","zip3")
list3 <- c("zip3")
layers <- list(list1, list2, list3)

## -----------------------------------------------------------------------------
outList <- cell_defn(discharge_data,th,layers)
D0 <- outList$assigned
print(nrow(D0))
print(nrow(discharge_data))

## -----------------------------------------------------------------------------
D0$party_ind <- 0
D0$party_ind[D0$hosp_id==1] <- 1
D0$party_ind[D0$hosp_id==2] <- 1
D0$party_ind[D0$hosp_id==5] <- 1

## -----------------------------------------------------------------------------
out <- div_calc(D0)

## -----------------------------------------------------------------------------
divratio_hosp <- out$hosp_level
sum(divratio_hosp$div_from_1, na.rm = TRUE)

## -----------------------------------------------------------------------------
print(out$hosp_level)
print(out$sys_level)

## -----------------------------------------------------------------------------
out <- wtp_calc(D0)
y_pre <- subset(out, party == 1)

D0_post <- D0
D0_post$sys_id[D0_post$party_ind == 1] <- 1

out <- wtp_calc(D0_post)
y_post <- subset(out, party == 1)

y_pre <- aggregate(list(WTP_s = y_pre$WTP_s, WTP_s_wt = y_pre$WTP_s_wt, N_s=y_pre$N_s),by=list(y_party=y_pre$party),sum)

print("% Change in WTP")
print((y_post$WTP_s-y_pre$WTP_s)/(y_pre$WTP_s)*100)

