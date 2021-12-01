#' EOQ no Constant Demand
#'
#' This function gets the optimal EOQ orders with different deterministic demand rates in one cycle T.
#'
#' @param n int - number of different demand rates in a cycle T.
#' @param d vector - vector of different demand rates in a cycle T (u/time). The vector length must be equal to @param n.
#' @param t vector - vector of percentages of cycle for each demand rate in vector d. The sum of this vector should be 1. The vector length must be equal to @param n.
#' @param k double - fixed cost for each order ($/order).
#' @param h double - holding cost for each unit per time ($/(u*time)).
#' @param c double - purchase cost for each unit ($/u).
#'
#' @details All time units must be the same (year, month, day, etc.).
#'
#' @return double
#' @export
#' @examples
#' EOQ_ncd(3, c(6.75, 0, 9), c(0.5, 0.3, 0.2), 1700, 4.5, 35)

EOQ_ncd <- function(n, d, t, k, h, c){
  if(sum(t) != 1)
  {
    message("The sum of vector t must be equal to 1.")
  }
  else if((length(d) != n)||(length(t) != n)){
    message("The lengths of vectors d and t must be equal to n")
  }
  else{
    a = 0
    b = 0
    l = 0
    for (i in 1:n){
      a = a + d[i]*t[i]
    }
    a = a^2

    for (i in 1:n){
      b = b + d[i]*t[i]^2
    }

    for (i in 2:n){
      p = 0
      for (j in 1:(i-1)){
        p = p + t[j]
      }
      l = l + d[i]*t[i]
    }
    Q = sqrt(a*k/(h*(b/2 + l)))
    G = k*sum(d*t)/Q + c*sum(d*t) + h*(b/2+l)*Q/sqrt(a)
    T = Q/sum(t*d)
    print(paste("Economic Order Quantity - Q* = ", round(Q,2)))
    print(paste("Cycle Time - T =", round(T,2)))
    print(paste("Total Average Cost (per time unit) - G(Q) = $", round(G,2)))
    x = c(0,0)
    y = c(0,Q)
    for (i in 1:5){
      for (j in 1:n){
        x = append(x,tail(x,n = 1) + T*t[j])
        y = append(y,tail(y,n = 1) - d[j]*T*t[j])
      }
      x = append(x, tail(x, n = 1))
      y = append(y, Q)
    }
    plot(x,y,type = "l",main = "Inventory Level vs Time",xlab="Time [time unit]", ylab= "Inventory Level [units]")
    return(Q)
  }
}


