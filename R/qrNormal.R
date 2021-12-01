#' QR with Normal Distribution Demand
#'
#' This function gets the optimal order quantity Q and the reorder point R with a Normal Distribution Demand in a Continuous Review System.
#'
#' @param m double - mean of the demand (u/time).
#' @param s double - standard deviation of the demand (u/time).
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
#' qrNormal(10,2.83,32,5,4,10,0.5,TRUE)

qrNormal <- function(m, s, k, c, h, p, t, b){
  Q = sqrt(2*k*m/h)
  Q_ref = -1
  mt = m*t
  st = s*sqrt(t)
  R = 0
  R_ref = -1
  while (((Q - Q_ref) > 0.001)||((R - R_ref) > 0.001)) {
    Q_ref = Q
    R_ref = R
    if(b == TRUE){
      R = qnorm(1-(Q*h)/(p*m),mean = mt, sd = st)
    }else{
      R = qnorm(1-(Q*h)/(Q*h + p*m),mean = mt, sd = st)
    }
    z = (R-mt)/st
    Lz = dnorm(z) - z*(1-pnorm(z))
    nR = Lz*st
    Q = sqrt((2*m*(k+p*nR))/h)
  }
  if (b == TRUE){
    G = k*m/Q + c*m + h*(Q/2 + R - mt) + p*m*nR/Q
  }else{
    G = k*m/Q + c*m + h*(Q/2 + R - mt + nR) + p*m*nR/Q
  }
  print(paste("Optimal Order Quantity - Q* =", round(Q,2)))
  print(paste("Reorder Point - R =",round(R,2)))
  print(paste("Total Average Cost (per time unit) - G(Q) = $", round(G,2)))
  u = round((Q/m)*2/(t/200))
  x = rnorm(u, mean = m*t/200, sd = s*sqrt(t/200))
  for (i in 1:u){
    if(x[i] < 0){
      x[i] = 0
    }
  }
  I = c(0,Q)
  tiem = c(0,0)
  IP = c()
  tiemP = c()
  temp = -1
  for (i in 1:u){
    if ((tail(I, n = 1) < R) && (temp == -1)){
      if (length(IP) == 0){
        IP = append(IP, tail(I, n = 1))
        tiemP = append(tiemP, tail(tiem, n = 1))
      }
      temp = 0
      IP = append(IP, tail(I, n = 1) + Q)
      tiemP = append(tiemP, tail(tiem, n = 1))
    }
    if (temp == 200){
      I = append(I, tail(I, n = 1) + Q)
      tiem = append(tiem, tail(tiem, n = 1))
      temp = -1
    }
    if((b == FALSE) && ((tail(I, n = 1) - x[i]) < 0 )){
      ter = 0
    }else{
      ter = (tail(I, n = 1) - x[i])
    }
    I = append(I, ter)
    tiem = append(tiem, i)
    if (temp != -1){
      temp = temp + 1
    }
    if (length(IP) != 0){
      if((b == FALSE) && ((I[length(I)-1] - x[i]) < 0 )){
        ter = tail(IP, n = 1) - I[length(I)-1]
      }else{
        ter = tail(IP, n = 1) - x[i]
      }
      IP = append(IP, ter)
      tiemP = append(tiemP, tail(tiem, n = 1))
    }
  }

  plot(tiemP*t/200,IP,type="l", col="red", xlim = c(0, max(tiem*t/200)), ylim=c(min(I),max(IP)),lty = 2, main = "Inventory Level vs Time",xlab="Time [unit time]",ylab = "Inventory Level [units]")
  lines(tiem*t/200,I,col="black")
  legend("bottomright",legend = c("Real Inventory\nLevel in Stock", "Inventory Position"), col = c("black","red"), lty = 1:2, cex = 0.8, bg='white')
  result = c(Q,R)
  return(result)
}
