# Perform a nonlinear (sine) fit on the tide data
tidal_fitting <- function(tides, A, B, C, D) {
   # Perform a nonlinear sine fit on the HdG tidal data
   HdG_fit <- nls(HdG ~ A * sin(B * time_hours + C) + D,
                  data = tides, 
                  start = list(A = A, B = B, C = C, D = D),
                  weights = tides$HdG_weights,
                  algorithm = 'port')
   
   # Perform a nonlinear sine fit on the Chesapeake City tidal data
   CCity_fit <- nls(CCity ~ A * sin(B * time_hours + C) + D,
                    data = tides, 
                    start = list(A = A, B = B, C = C, D = D),
                    weights = tides$CCity_weights,
                    algorithm = 'port')
   
   # Collect the fitted parameters
   HdG_params <- coef(HdG_fit)
   CCity_params <- coef(CCity_fit)
   
   # The fit can produce a negative amplitude and flip the sine curve, so control the parameters
   if (HdG_params['A'] < 0) {
      HdG_params['A'] = HdG_params['A'] * -1
      HdG_params['C'] = HdG_params['C'] + pi
   }
   
   if (CCity_params['A'] < 0) {
      CCity_params['A'] = CCity_params['A'] * -1
      CCity_params['C'] = CCity_params['C'] + pi
   }
   
   # Adjust the CCity parameters to match the HdG fit (i.e., create a new HdG tide time series)
   tides$new_HdG <- (HdG_params['A'] / CCity_params['A']) * (tides$CCity - CCity_params['D']) + HdG_params['D']
   
   return(tides)
}
