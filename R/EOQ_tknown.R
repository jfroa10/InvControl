#' EOQ with Different Demand Rates in a Time of Cycle Known
#'
#' This function gets the optimal EOQ orders with different deterministic demand rates in one cycle T, where this time is known with different sub-times (seasonal).
#'
#' @param n int - number of different demand rates in a cycle T.
#' @param d vector - vector of different demand rates in a cycle T (u/time). The vector length must be equal to @param n.
#' @param t vector - vector of different times (time) in a cycle T for each demand rate in vector d. The sum of this vector should be equal to T. The vector length must be equal to @param n.
#' @param k double - fixed cost for each order ($/order).
#' @param h double - holding cost for each unit per time ($/(u*time)).
#' @param c double - purchase cost for each unit ($/u).
#'
#' @details All time units must be the same (year, month, day, etc.).
#'
#' @return double - Economic Order Quantity Q*
#' @export
#' @examples
#' EOQ_tknown(5,c(150,450,0,300,0),c(12,5,2,4,1),1500,5,17)

EOQ_tknown <- function(n,d,t,k,h,c){
  if((n == length(d))&&(n == length(t))&&(length(d) == length(t))){
    for (i in 1:n) {
      if(d[i]<0){
        message("The demand rates cannot be negative.")
        stop("")
      }
      if(t[i]<0){
        message("The times in vector t cannot be negative.")
        stop("")
      }
    }
    if(k<0||c<0||h<0){
      message("Costs cannot be negative.")
      stop("")
    }
    T = sum(t)
    if(d[1] == 0){
      A = "Q*t[1]"
    }else{
      A = "(Q*t[1]-floor(d[1]*t[1]/Q)*Q^2/(2*d[1])-(t[1]-floor(d[1]*t[1]/Q)*Q/d[1])^2*d[1]/2)"
    }
    if(n > 1){
      for(i in 2:n){
        suma = "(d[1]*t[1]"
        if(i > 2){
          for(j in 2:(i-1)){
            suma = paste(suma, "+ d[", j, "]*t[", j, "]")
          }
        }
        suma = paste(suma, ")")
        x = paste("(ceiling(", suma, "/Q)*Q - ", suma, ")")
        x = paste("(if(", x, "== 0){Q}else{", x, "})")
        if(d[i] == 0){
          A_j = paste("(", x, "*t[", i, "])")
        }else{
          mini = paste("(min(t[", i, "],(", x, "/d[", i, "])))")
          piso = paste("(floor((d[", i, "]*(t[", i, "]-", mini, "))/Q))")
          A_1 = paste("(", mini, "*", x, "-(", mini, "^2)*d[", i, "]/2)")
          A_2 = paste("(Q*(t[", i, "]-", mini, "))")
          A_3 = paste("((", piso, "*Q^2)/(2*d[", i, "]))")
          A_4 = paste("(((t[", i, "]-", mini, "-", piso, "*Q/d[", i, "])^2)*(d[", i, "]/2))")
          A_j = paste("(", A_1, "+", A_2, "-", A_3, "-", A_4, ")")
        }
        A = paste("(", A, ")+(", A_j, ")")
      }
    }
    I = paste("((", A, ")/", T, ")")
    D = sum(d*t)
    G = c()
    EOQ = c()
    for(i in 1:D){
      Q = i
      m = D/Q
      if(m%%1 == 0){
        cost = m*k/T + c*m*Q/T + h*eval(parse(text = I))
        G = append(G, cost)
        EOQ = append(EOQ, Q)
      }
    }
    index = which(G == min(G, na.rm = TRUE))
    Q = EOQ[index]
    print(paste("Economic Order Quantity - Q* = ", round(Q,2)))
    print(paste("Total Average Cost (per time unit) - G(Q) = $", round(G[index],2)))
    x = c(0,0)
    y = c(0,Q)
    for(i in 1:n){
      x_ref = 0
      while(tail(y, n = 1) - d[i]*(t[i] - x_ref) < 0){
        x_ref = x_ref + tail(y, n = 1)/d[i]
        x = append(x, tail(x, n = 1) + tail(y, n = 1)/d[i])
        y = append(y, 0)
        x = append(x, tail(x, n = 1))
        y = append(y, Q)
      }
      if(tail(y, n = 1) - d[i]*(t[i] - x_ref) > 0){
        x = append(x, tail(x, n = 1) + t[i] - x_ref)
        y = append(y, tail(y, n = 1) - d[i]*(t[i] - x_ref))
      }
      if(tail(y, n = 1) - d[i]*(t[i] - x_ref) == 0){
        x = append(x, tail(x, n = 1) + t[i] - x_ref)
        y = append(y, tail(y, n = 1) - d[i]*(t[i] - x_ref))
        x = append(x, tail(x, n = 1))
        y = append(y, Q)
      }
    }
    plot(x,y,type = "l",main = "Inventory Level vs Time",xlab="Time [time unit]", ylab= "Inventory Level [units]")
    return(Q)
  }else{
    message("n must be equal to the d and t vectors lengths.")
  }
}
