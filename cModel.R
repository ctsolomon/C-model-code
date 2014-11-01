#Function that calculates rates of change using differential equations for simple C model
#CTS 25 July 2014 building on SEJ version

cModel <- function(t,y,parms,otherPars) {
  with(as.list(c(y,parms,otherPars)),{
    
    #Use interpolation functions to calculate values of inputs at current time
    qIn <- qInFun(t)
    DICIn <- DICInFun(t)
    precip <- precipFun(t)
    DICPrecip <- DICPrecipFun(t)
    light <- lightFun(t)
    v <- vFun(t)
    qOut <- qOutFun(t)
    DICeq <- DICeqFun(t)
    zMix <- zMixFun(t)
    DOCIn <- DOCInFun(t)
    DOCPrecip <- DOCPrecipFun(t)
    
    #Calcuate rates of change
    dDICdt <- qIn*DICIn + precip*DICPrecip + r*DOC - iota*light*0.2*v - qOut*DIC/v - k*(DIC/v-DICeq)/zMix*v
    dDOCdt <- qIn*DOCIn + precip*DOCPrecip - r*DOC  - qOut*DOC/v
    dCdt <- c(dDICdt,dDOCdt)
    out <- list(dCdt)
    return(out)
  })
}
