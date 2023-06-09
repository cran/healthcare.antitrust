#' Diversion Ratio Calculator
#'
#' Calculates hospital-level diversion ratios, once cells have been
#' defined.
#'
#' @param data Dataset of hospital discharges, with required variables:
#'   \code{cell}, \code{hosp_id}, \code{hospital}, \code{sys_id},
#'   \code{party_ind}, \code{count}. Use other function arguments to
#'   indicate alternative variable names to the default names.
#' @param cell Name of variable specifying cell to which each observation
#'   has been allocated. Default variable name is \code{cell}. Can be
#'   created by \code{cell_defn} function.
#' @param hosp_id Name of variable specifying (numeric) hospital
#'   identifier. Default variable name is \code{hosp_id}.
#' @param hospital Name of variable specifying (string) hospital name.
#'   Default variable name is \code{hospital}.
#' @param sys_id Name of variable specifying (numeric) system identifier.
#'   Default variable name is \code{sys_id}.
#' @param party_ind Name of indicator variable for whether hospital is a
#'   merging party from which diversions should be calculated. Default
#'   variable name is \code{party_ind}.
#' @param count Name of variable indicating the number of admissions
#'   represented by the observation. Set = 1 for every row if each
#'   observation represents one admission.
#' @param dropDegenerateCell logical; specifies how to treat cells with a
#'   100 percent within-system share. If TRUE, observations in degenerate, 100
#'   percent share cells will be ignored in the diversion ratio calculation.
#'   If FALSE, any such individuals will be assigned to the outside option,
#'   but still included in the denominator, so that the inside-option diversion
#'   will total less than 100  percent.
#'
#' @returns A list with two components. The first component, `hosp_level`,
#'  is a matrix giving hospital-level diversions from party hospitals to
#'  all other hospitals. The second object, `sys_level`, is a matrix that
#'  aggregates party hospitals to systems, thus giving diversions from party
#'  systems to all other hospitals.
#'
#' @details For system-to-system diversions, set \code{hosp_id} and
#'  \code{hospital} equal to corresponding system-level identifiers.
#'  Patients are not allowed to divert to within-system alternative
#'  hospitals.
#'
#' For more details see the example vignette by typing:
#' \code{vignette("semipar_example", package = "healthcare.antitrust")}
#'
#' @examples
#' data(discharge_data, package = "healthcare.antitrust")
#'
#' list1 <- c("drg","age","zip5")
#' layers <- list(list1)
#' th <- 15
#' discharge_data$count <- 1
#'
#' outList <- cell_defn(discharge_data,th,layers)
#' D0 <- outList$assigned
#'
#' D0$party_ind <- 0
#' D0$party_ind[D0$hosp_id==1] <- 1
#' D0$party_ind[D0$hosp_id==2] <- 1
#' D0$party_ind[D0$hosp_id==5] <- 1
#'
#' out <- div_calc(D0)
#'
#' @importFrom stats aggregate ave
#' @importFrom methods is
#' @export



##################################################################
# Diversion Ratio Calculator
##################################################################
# Required inputs: cell, hosp_id, hospital, sys_id, system, party_ind, count
# where party_ind is 1 party hospitals, zero otherwise
# and count is the number of admissions represented by the observation. =1 for all if
# each observation is one admissions

# Could be nice to pass-through system name string variable to output
# if it is supplied.

div_calc <- function(data,
                     cell = "cell",
                     hosp_id = "hosp_id",
                     hospital = "hospital",
                     sys_id = "sys_id",
                     party_ind = "party_ind",
                     count = "count",
                     dropDegenerateCell = TRUE) {

  # allow for generic variable names - old. Now use [] instead of $
  #names(data)[names(data) == cell] <- "cell"
  #names(data)[names(data) == hosp_id] <- "hosp_id"
  #names(data)[names(data) == hospital] <- "hospital"
  #names(data)[names(data) == sys_id] <- "sys_id"
  #names(data)[names(data) == party_ind] <- "party_ind"
  #names(data)[names(data) == count] <- "count"


  # To address check() NOTEs
  #N_h <- hosp_id <- hospital <- party_sys_id <- sys_id <- NULL
  N_h <- NULL
  party_sys_id <- NULL

  check <- unique(data[c(hosp_id,hospital)])
  if (length(unique(check$hosp_id)) != length(check$hosp_id)) {warning('Error: hosp_id associated with multiple hospital names')}
  #if (length(unique(check$hospital)) != length(check$hospital)) {warning('Error: hospital name associated with multiple hosp_ids')}

  # Error checks
  if (!is(data,"data.frame")) {warning('Input needs to be a dataframe'); stop()}
  if (!is(dropDegenerateCell,"logical")) {warning('Input dropDegenerateCell needs to be a logical'); stop()}

  # old Var name checks
  #if (!"cell" %in% names(data)) {warning('Variable "cell" required in input dataset'); stop()}
  #if (!"hosp_id" %in% names(data)) {warning('Variable "hosp_id" required in input dataset'); stop()}
  #if (!"hospital" %in% names(data)) {warning('Variable "hospital" required in input dataset'); stop()}
  #if (!"sys_id" %in% names(data)) {warning('Variable "sys_id" required in input dataset'); stop()}
  #if (!"party_ind" %in% names(data)) {warning('Variable "party_ind" required in input dataset'); stop()}
  #if (!"count" %in% names(data)) {warning('Variable "count" required in input dataset'); stop()}

  # Updated var checks
  if (! cell %in% names(data)) {warning('Variable "cell" required in input dataset'); stop()}
  if (! hosp_id %in% names(data)) {warning('Variable "hosp_id" required in input dataset'); stop()}
  if (! hospital %in% names(data)) {warning('Variable "hospital" required in input dataset'); stop()}
  if (! sys_id %in% names(data)) {warning('Variable "sys_id" required in input dataset'); stop()}
  if (! party_ind %in% names(data)) {warning('Variable "party_ind" required in input dataset'); stop()}
  if (! count %in% names(data)) {warning('Variable "count" required in input dataset'); stop()}

  iter <- 0
  data$party_sys_id <- data[[party_ind]]*data[[sys_id]]
  party_sys_list <- sort(unique(data$party_sys_id[data$party_sys_id > 0]))

  for (m in party_sys_list) {
    # Calculate cell-specific hospital diversion ratios
    y_hosp_cell = aggregate(data[[count]],by=list(data[[cell]],data[[hosp_id]],data[[hospital]],data$party_sys_id),sum)
    names(y_hosp_cell) <- c("cell","hosp_id","hospital","party_sys_id","N_h")

    y_hosp_cell$N <- ave(y_hosp_cell$N_h,y_hosp_cell$cell, FUN = sum)
    y_hosp_cell$share_h <- y_hosp_cell$N_h/y_hosp_cell$N
    y_hosp_cell$share_m <- ave(y_hosp_cell$share_h,y_hosp_cell$cell, y_hosp_cell$party_sys_id, FUN = sum)
    y_hosp_cell$share_m[y_hosp_cell$party_sys_id != m] <- 0
    y_hosp_cell$share_m <- ave(y_hosp_cell$share_m,y_hosp_cell$cell, FUN = max)

    y_hosp_cell$share_h[y_hosp_cell$party_sys_id == m] <- 0 # set share to zero for system hospitals

    y_hosp_cell$div <- y_hosp_cell$share_h/(1-y_hosp_cell$share_m)


    # Calculate predicted hospital-cell admissions after hospital k exclusion
    system_hosp <- sort(unique(y_hosp_cell$hosp_id[y_hosp_cell$party_sys_id == m]))

    for (k in system_hosp) {
      #print(paste0("Hosp Id: ", k))  # removed for CRAN, just was status update
      iter <- iter + 1

      y_hosp_cell$N_k <- 0
      y_hosp_cell$N_k[y_hosp_cell$hosp_id == k] <- y_hosp_cell$N_h[y_hosp_cell$hosp_id == k]
      y_hosp_cell$N_k <- ave(y_hosp_cell$N_k,y_hosp_cell$cell, FUN = max)

      y_hosp_cell$N_h_predict <- y_hosp_cell$N_h + y_hosp_cell$N_k*y_hosp_cell$div
      y_hosp_cell$N_h_predict[y_hosp_cell$party_sys_id == m] <- 0

      # Sum across cells
      y_hosp = aggregate(data[[count]],by=list(data[[hosp_id]],data[[hospital]],data$party_sys_id,data[[sys_id]]),sum)
      names(y_hosp) <- c("hosp_id","hospital","party_sys_id","sys_id","N_h")

      y_hosp$N_k <- 0
      y_hosp$N_k[y_hosp$hosp_id == k] <- y_hosp$N_h[y_hosp$hosp_id == k]
      y_hosp$N_k <- max(y_hosp$N_k)

      temp <- aggregate(y_hosp_cell$N_h_predict,by=list(y_hosp_cell$hosp_id),sum)
      names(temp) <- c("hosp_id","N_h_predict")

      y_hosp <- merge(y_hosp,temp)

      # Calculate hospital diversion ratios - two options for denom
      if (dropDegenerateCell == FALSE) {
        y_hosp$div <- (y_hosp$N_h_predict-y_hosp$N_h)/y_hosp$N_k
      }
      if (dropDegenerateCell == TRUE) {
        y_hosp$movers <- y_hosp$N_h_predict - y_hosp$N_h
        y_hosp$N_k_alt <- sum(y_hosp$movers[y_hosp$movers>0])
        y_hosp$div <- (y_hosp$N_h_predict-y_hosp$N_h)/y_hosp$N_k_alt
      }


      y_hosp$div[y_hosp$party_sys_id == m] <- NA

      # Print flag if degenerate cells
      degenlist <- y_hosp_cell$cell[is.na(y_hosp_cell$div) & y_hosp_cell$hosp_id == k]
      if (length(degenlist) > 0) {
        message("Note the following cells are degenerate: ",paste(degenlist, collapse = ", "))

        totdiv <- sum(y_hosp$div, na.rm = TRUE)
        #print(paste0("Total Diversion: ",totdiv))  # removed for CRAN, not really necessary info
      }

      if (iter == 1) {out <- subset(y_hosp, select=c(hosp_id,hospital,party_sys_id,sys_id,N_h))}

      #out[,paste0("div_",m,"_",k)] <- y_hosp$div
      out[,paste0("div_from_",k)] <- y_hosp$div

    }

  }

  # sort for return of hospital-level diversions
  out$party_sys_id[out$party_sys_id == 0] <- NA
  out <- out[order(out$party_sys_id,out$sys_id,out$hosp_id),]

  # also calculate system-level diversion
  out2 <- out
  party_sys_list <- sort(unique(out$party_sys_id[!is.na(out$party_sys_id)]))
  for (m in party_sys_list) {
    party_hosp_list <- sort(unique(out$hosp_id[out$party_sys_id==m]))
    ct <- out$N_h[out2$party_sys_id==m & !is.na(out2$party_sys_id)]
    varnames <- paste("div_from_", party_hosp_list, sep="")

    out2[,paste0("div_from_sys_",m)] <- (rowSums(as.matrix(out[varnames]) %*% diag(ct, nrow = length(ct))))  / (sum(ct))
    out2[varnames] <- NULL
  }


  # Return List of Outputs
  #names(out)[names(out) == "cell"] <- cell
  #names(out2)[names(out2) == "cell"] <- cell
  #names(out)[names(out) == "hosp_id"] <- hosp_id
  #names(out2)[names(out2) == "hosp_id"] <- hosp_id
  #names(out)[names(out) == "hospital"] <- hospital
  #names(out2)[names(out2) == "hospital"] <- hospital
  #names(out)[names(out) == "sys_id"] <- sys_id
  #names(out2)[names(out2) == "sys_id"] <- sys_id
  #names(out)[names(out) == "party_ind"] <- party_ind
  #names(out2)[names(out2) == "party_ind"] <- party_ind
  #names(out)[names(out) == "count"] <- count
  #names(out2)[names(out2) == "count"] <- count

  newList <- list("hosp_level" = out, "sys_level" = out2)
  #newList <- list("hosp_level" = out)
  return(newList)
}


