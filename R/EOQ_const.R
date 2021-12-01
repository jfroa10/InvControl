#' EOQ Constant Demand
#'
#' This function gets the optimal EOQ orders with constant demand in one cycle T.
#'
#' @param d double - constant demand in a cycle T (units/time).
#' @param k double - fixed cost for each order ($/order).
#' @param h double - holding cost for each unit per time ($/(unit*time)).
#' @param c double - purchase cost for each unit ($/unit).
#'
#' All time units must be the same (year, month, day, etc.).
#'
#' @return double - Economic Order Quantity Q*
#' @export
#' @examples
#' EOQ_const(400,100,0.4,200)

EOQ_const <- function(d, k, h, c){
  Q = sqrt(2*d*k/h)
  G = k*d/Q + c*d + h*Q/2
  print(paste("Economic Order Quantity - Q* = ", round(Q,2)))
  print(paste("Cycle Time - T =", round(Q/d,2)))
  print(paste("Total Average Cost (per time unit) - G(Q) = $", round(G,2)))
  x = c(0,0)
  y = c(0,Q)
  for(i in 1:5){
    x = append(x,tail(x,n = 1) + Q/d)
    y = append(y,0)
    x = append(x,tail(x,n = 1))
    y = append(y,Q)
  }
  plot(x,y,type = "l",main = "Inventory Level vs Time",xlab="Time [time unit]", ylab= "Inventory Level [units]")
  return(Q)
}
