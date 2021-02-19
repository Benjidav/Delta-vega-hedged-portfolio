
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

PriceCall.Volat = function (S0, K, date0, maturity, r, volat_vector, type = "Call")
{
  format = "%d/%m/%Y"
  spread_date = as.numeric(as.Date(maturity, format) - as.Date(date0, format))/365
  price_call = rep(NA, length(volat_vector))
  for (i in 1:length(volat_vector))
  {
    price_call[i] = BS.OptionPricerEU(S0, K, spread_date, 0, volat_vector[i], "Call")
  }
  return (price_call)
}

BS.parameters = function (S, K, T, r)
{
  list = list ("S" = S, "K" = K,"T" = T, "r" = r)
  return (list)
}

Vector.d1 = function (S, K, r, volat, T)
{
  vector_d1 <- rep(NA, length(K))
  for (i in 1:length(K))
  {
    vector_d1[i] = (log(S/K[i]) + (r + 0.5*volat[i]^2)*T) / (volat[i]*sqrt(T))
  }
  return (vector_d1)
}

Vector.DeltaOption = function (vector_d1)
{
  vector_delta <- rep(NA,length(vector_d1))
  for (i in 1:length(vector_d1))
  {
    #delta[i] = N(d1[i])
    vector_delta[i] = pnorm(vector_d1[i])
  }
  return (vector_delta)
}

Vector.PriceOption = function (stock_price, K, T_vector, r, volat, type = "Call")
{
  price_option = rep(NA, length(T_vector))
  for (i in 1:length(T_vector))
  {
    price_option[i] = BS.OptionPricerEU(stock_price[i], K, T_vector[i], 0, volat, "Call")
  }
  return (price_option)
}

BS.OptionPricerEU <- function(S, K, T, r,volat, type="Call")
{
  d1 <- (log(S/K) + (r + 0.5*volat^2)*T) / (volat*sqrt(T))
  d2 <- d1 - volat*sqrt(T)
  if(type=="Call")
  {
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  }
  if(type=="Put")
  {
    value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
  }
  return(value)
}
