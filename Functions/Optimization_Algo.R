#Optimization Algorithm

ImpliedVolat.binary.search = function (a, b, target_value, S, K, T, r, epsilon = 1e-4)
{
  c = -1
  fc = -1
  fa = target_value - BS.OptionPricerEU(S, K, T, r, a)
  fb = target_value - BS.OptionPricerEU(S, K, T, r, b)
  #We check that fa and fb did not get out of Df
  if (!is.nan(fa) && !is.nan(fb))
  {
    if (fa * fb>=0)
    {
      cat("pas de racine entre ", a," et ", b)
    }
    else
    {
      while (b-a >= epsilon && fc != 0)
      {
        c=(a+b)/2
        fc = target_value - BS.OptionPricerEU(S, K, T, r, c)
        if (!is.nan(fc))
        {
          if (fa * fc <=0)
          {
            b=c
          }
          else
          {
            a=c
          }
        }
        #Si fc is NaN
        else 
        {
          c = NaN
          break
        }
      }
    }
  }
  else
  {
    c = NaN
  }
  return (c)
}

ImpliedVolat.NewtonRaphson <- function(target_value, volat, S, K, T, r, n = 1000, PRECISION = 1.0e-4) 
{
  for (i in 1:n) 
  {
    price <- BS.OptionPricerEU(S, K, T, r, volat)
    vega <- bs_vega(S, K, T, r, volat)
    diff <- target_value - price  # our root
    if (abs(diff) < PRECISION) 
    {
      #cat("La valeur de la volatilité implicite approximée est de : " , volat)
      return(volat)
    }
    if (vega < PRECISION)
    {
      print("Itérations stoppées car Vega est devenu trop faible")
      return (volat)
    }
    volat <- volat + diff/vega # f(x) / f'(x)
  }
  print('Too many iterations in method')
}    

#Relevant function to compute f'(x) from BS function C(volat)
bs_vega <- function(S,K,T,r,volat,q=0.0, type = "Call") 
{
  d1 <- (log(S/K)+(r+(volat*volat)/2.)*T)/(volat*sqrt(T))
  return(S * sqrt(T)*dnorm(d1))
}

Vector.bs_vega  <- function(stock_price, K, T_vector, d1_vector, q=0.0, type = "Call") 
{
  vector_vega = rep(NA, length(d1_vector))
  for (i in 1:length(d1_vector))
  {
    if (!is.na(d1_vector[i]))
    {
      vector_vega[i] = stock_price[i] * sqrt(T_vector[i])*dnorm(d1_vector[i])
    }
  }
  return(vector_vega)
}

display_func = function (func, a, b)
{
  curve (func, from = a, to = b, type = "l", xlab = "Valeur de x", ylab = "F(x)")
  abline(h=0, col="red", lty=1, lwd=2)
}

func = function (x)
{
  x^2 + 1
}

BS.parameters = function (S, K, T, r)
{
  list = list ("S" = S, "K" = K,"T" = T, "r" = r)
  return (list)
}

#f(a) * f(b) <= 0 is Necessary to start the Algorithm
binary.search = function (f, a, b, epsilon = 1e-5)
{
  c = -1
  fc = -1
  fa = f(a)
  fb = f(b)
  if (fa * fb>=0)
  {
    cat("pas de racine entre ", a," et ", b)
  }
  else
  {
    while (b-a >= epsilon && fc != 0)
    {
      c=(a+b)/2
      fc = f(c)
      if (fa * fc <=0)
      {
        b=c
      }
      else
      {
        a=c
      }
    }
    cat("Une racine possible est  : ", c)
    return (c)
  }
}

newton.raphson = function(f, a, tol = 1e-5, n = 1000) 
{
  require(numDeriv) # Package for computing f'(x)
  
  x0 = a # Set start value to supplied lower bound
  k = n # Initialize for iteration results
  res = 0
  # Check the upper and lower bounds to see if approximations result in 0
  fx0 = f(x0)
  
  if (fx0 == 0.0) 
  {
    return(x0)
  }
  
  for (i in 1:n) 
  {
    dx = genD(func = f, x = x0)$D[1] # First-order derivative f'(x0)
    x1 = x0 - (f(x0) / dx) # Calculate next value x1
    k[i] = x1 # Store x1
    # Once the difference between x0 and x1 becomes sufficiently small, output the results.
    if (!is.nan(x1))
    {
      if (abs(x1 - x0) < tol) 
      {
        root.approx = tail(k, n=1)
        res = list('root approximation' = root.approx, 'iterations' = k)
        return(res)
      }
      # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
      x0 = x1
    }
    #Si x1 est NaN --> on stoppe l'algo
    else
    {
      print("L'algorithme a été stoppé à cause d'une forme inderterminée")
      break
    }
  }
  #We display that only if there is no error and if the number of iterations  is N
  if (i == n)
  {
    print('Too many iterations in method')
  }
}

