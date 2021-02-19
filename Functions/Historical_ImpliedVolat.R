#---------------------------------Question 6 : Delta-vega hedging-------------------------------------

pf.deltaVega_hedging = function (price_option_1, price_option_2, stock_price, list_prm_vega)
{
  portfolio = rep(NA, length(price_option_1))
  j = 1
  #V0 = C0 en vérifiant que la premiere valeur est pas NA
  if (is.na(list_prm_vega[1, 1]))
  {
    while (is.na(list_prm_vega[j, 1]))
    {
      j = j + 1
    }
    #the last +1 is here because of q' : quantity of the second option
    portfolio[j] = price_option_1[j]
  }
  else 
  {
    portfolio[1] = price_option_1[1]
  }
  #the last +1 is here because of q' : quantity of the second option
  for (i in (j + 1):length(portfolio))
  {
    if (!is.na(list_prm_vega[i - 1, 1]))
    {
      portfolio[i] = portfolio[i - 1] + list_prm_vega[i - 1, 3] * (stock_price[i] - stock_price[i - 1]) + list_prm[i - 1, 1] * (price_option_2[i] - price_option_2[i - 1])
    }
  }
  return (portfolio)
}

Vector.PrmVega = function (vega_serie_1, vega_serie_2, delta_serie_1, delta_serie_2, option_price_Opts6, option_price_Opts6.5, crossed_vega_Opt2, stock_price)
{
  q_option_2 = rep(NA, length(option_price_Opts6))
  q_stock = rep(NA, length(option_price_Opts6))
  l_cash = rep(NA, length(option_price_Opts6))
  for (i in 1:length(option_price_Opts6))
  {
    if (!is.na(derived_sigma[i]))
    {
      #q'(i)= vega(i)/(vega'(i) * derived_sigma)
      q_option_2[i] = vega_serie_1[i] / (crossed_vega_Opt2[i])
      #q(i)= delta(i) - delta'(i) * (vega'(i) * derived_sigma)
      q_stock[i] = delta_serie_1[i] - delta_serie_2[i] * (vega_serie_1[i]/crossed_vega_Opt2[i])
      #q'(i) = C(i) - q''(i) * C''(i) - q(i) * stock (i)
      l_cash[i] = option_price_Opts6[i] - q_option_2[i] * option_price_Opts6.5[i] - q_stock[i] * stock_price[i]
    }
  }
  list_prm = list(q_option_2, l_cash, q_stock)
  names(list_prm) = c("q_option_2",  "l_cash", "q_stock")
  return (list_prm)
}

compute_crossed_vega = function (vega_option, derived_sigma)
{
  crossed_vega = rep(NA, length(vega_option))
  for (i in 1:length(crossed_vega))
  {
    if(!is.na(vega_option[i]) & !is.na(derived_sigma[i]))
    {
      crossed_vega[i] = vega_option[i] * derived_sigma[i]
    }
  }
  return (crossed_vega)
}

compute_derived_sigma = function (increment_hist_opt1, increment_hist_opt2)
{
  derived_sigma = rep(NA, length(increment_hist_opt1))
  for (i in 2:length(derived_sigma))
  {
    if(!is.na(increment_hist_opt1[i]) & !is.na(increment_hist_opt2[i]))
    {
      derived_sigma[i] = increment_hist_opt2[i]/increment_hist_opt1[i]
    }
  }
  return (derived_sigma)
}

compute_increments = function (hist_vector)
{
 increment_vector = rep(NA, length(hist_vector))
  for (i in 2:length(increment_vector))
  {
    if(!is.na(hist_vector[i - 1]))
    {
      increment_vector[i] = hist_vector[i] - hist_vector[i - 1]
    }
  }
  return (increment_vector)
}


#------------------------------Question 4 : Historical Implied Volatility of two options------------------------------- 

Historical.ImpliedVolat.NewtonRaphson = function (option_price, volat, stock_price, K, T_vector, r)
{
  hist_Impliedvolat = rep(NA, length(option_price))
  for (i in 1:length(option_price))
  {
    if(!is.na(option_price[i]))
    {
      hist_Impliedvolat[i] = ImpliedVolat.NewtonRaphson(option_price[i], volat, stock_price[i], K, T_vector[i], r)
    }
  }
  return (hist_Impliedvolat)
}

Historical.ImpliedVolat.binary.search = function (a, b, option_price, stock_price, K, T_vector, r)
{
  hist_Impliedvolat = rep(NA, length(option_price))
  for (i in 1:length(option_price))
  {
    if(!is.na(option_price[i]))
    {
      hist_Impliedvolat[i] = ImpliedVolat.binary.search(a, b, option_price[i], stock_price[i], K, T_vector[i], r)
    }
    if (is.nan(hist_Impliedvolat[i])) stop ("fa and fb got out of Df")
  }
  return (hist_Impliedvolat)
}

