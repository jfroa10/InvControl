#' QR with Poisson Distribution Demand
#'
#' This function gets the optimal order quantity Q and the reorder point R with a Poission Distribution Demand in a Continuous Review System.
#'
#' @param m double - mean of the demand (u/time).
#' @param k double - fixed cost for each order ($/order).
#' @param c double - purchase cost for each unit ($/u).
#' @param h double - holding cost for each unit per time ($/(u*time)).
#' @param p double - cost for each unit shortage during Lead Time ($/(u missing))
#' @param t double - Lead Time, time an order takes to arrive once is placed (time)
#' @param b boolean - TRUE if the system manages Backorders, FALSE if the system manages Lost Sales
#'
#' @details All time units must be the same (year, month, day, etc.).
#'
#' @return vector - c(Q,R)
#' @export
#' @examples
#' qrPoisson(100,32,5,4,10,0.5,TRUE)

qrPoisson <- function(m, k, c, h, p, t, b){
  Q = round(sqrt(2*k*m/h))
  Q_ref = -1
  mt = m*t
  R = 0
  R_ref = -1
  while (((Q - Q_ref) > 0.001)||((R - R_ref) > 0.001)) {
    Q_ref = Q
    R_ref = R
    if(b == TRUE){
      R = qpois(1-(Q*h)/(p*m),lambda = mt)
    }else{
      R = qpois(1-(Q*h)/(Q*h + p*m),lambda = mt)
    }
    nR = 0
    nR_ref = -1
    x = R
    while ((nR - nR_ref) > 0.001){
      nR_ref = nR
      x = x + 1
      nR = nR + (x - R)*dpois(x,lambda = mt)
    }
    Q = round(sqrt((2*m*(k+p*nR))/h))
  }
  print(paste("Q = ",Q))
  print(paste("R = ",R))
  c(Q,R)
}
