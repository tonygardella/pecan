##-------------------------------------------------------------------------------------------------#
##' GERMAN Soil model
##'
##' @name german.model
##' @title German Soil Model code
##' @param input_met list of hourly temperature in degress C
##' @param input_params list of defaults to process
##' @return soil model output
##' @description GER is a model of soil decomposition developed from German et al. (2012) 
##' DOI: 10.1111/j.1365-2486.2011.02615.x with parameter values from Li et al. (2014) DOI:10.1007/s10533-013-9948-8.
##' @export
##' @author Rose Abramoff, Katerina Georgiou, Anthony Gardella
##-------------------------------------------------------------------------------------------------#

german_model <- function( input_met =NULL, input_params = NULL){
  
library(FME)

if(is.null(input_met)){
  PEcAn.logger::logger.info("No Input meteorological data, using default 2009 Harvard Forest data")
  # Option 1: Seasonally varying temperature from Harvard Forest EMS 2009
  inputdata <- read.csv(paste0(getwd(),"/InputTemp.csv"))
  RunTime <- length(inputdata$indexHour) # Hours
  if(!exists(inputdata)){
    # Option 2: Constant temperature
    PEcAn.logger::logger.info("Unable to find default temperature data. Using Constant temperature of 20 C")
    inputdata <- as.data.frame(cbind(1:RunTime, rep(20,RunTime)))
    names(inputdata) <- c("indexHour","temperatureC")
  }
}else if(!is.null(input_temp)){
  PEcAn.logger::logger.info("Using Input meteorological data processed through PEcAn")
  
  
} else{
  
}

if(is.null(input_params)){
PEcAn.logger::logger.info("No Input parameter, using default parameters")
  Tref=20 # Reference temperature
  Is=0.00016 # SOC input rate (mg/g/hr)
  Vmax0=0.01 # SOC reference Vmax (mg C (mg MBC)^-1 hr^-1)
  Km0=250 # SOC reference Km (mgC /g soil)
  Ea=47 # SOC Vmax activation energy (kJ/mol)
  KEa=30 # SOC Km activation energy (kJ/mol)
  rB=0.00028 # MBC turnover rate (mg C (mg C)^-1 hr^-1)
  CUE0=0.31 # Carbon use efficiency reference(mg C (mg C)^-1)
  CUEslope=-0.016 # Slope of CUE temperature sensitivity
  p = c(Tref,Vmax0,Km0,Ea,KEa,rB,CUE0,CUEslope)
}  
  
  
  # Option 1: Made a fake seasonal input based on reference input...just for fun
A1=0.0005       #seasonal amplitude
A2=0            #daily amplitude
w1=2*pi/RunTime
w2=2*pi
Iref=Is         #reference input
t=1:RunTime
Litter = Iref+A1*sin(w1*t-pi/2)+A2*sin(w2*t) # mgC g-1 soil hour-1
  
}

# Option 2: Constant Litter input
Litter = rep(Iref,RunTime)

derivs <- function(t,s,p) { #t = time, s = state, p = pars
        with(as.list(c(s,p)), {
              # Define parameters
          R        = 0.008314 # Gas constant (kJ/mol/degree)
          CtoK     = 273.15   # Convert Celcius to Kelvin
          Tref     <- p[1] # Reference temperature
          Vmax0    <- p[2] # SOC reference Vmax (mg C (mg MBC)^-1 hr^-1)
          Km0      <- p[3] # SOC reference Km (mgC /g soil)
          Ea       <- p[4] # SOC Vmax activation energy (kJ/mol)
          KEa      <- p[5] # SOC Km activation energy (kJ/mol)
          rB       <- p[6] # MBC turnover rate (mg C (mg C)^-1 hr^-1)
          CUE0     <- p[7] # Carbon use efficiency reference(mg C (mg C)^-1)
          CUEslope <- p[8] # Slope of CUE temperature sensitivity        

          # Arrhenius temperature dependence
          Vmax=Vmax0*exp(-(Ea/R)*(1/(Temp(t)+CtoK)-1/(Tref+CtoK)))
          Km=Km0*exp(-(KEa/R)*(1/(Temp(t)+CtoK)-1/(Tref+CtoK)))

          # Linear temperature dependence
          CUE=CUE0+CUEslope*(Temp(t)-Tref)

          # Update pools
          dsoc = Is(t) + rB*mic - Vmax*mic*soc/(Km + soc)
          dmic = CUE*Vmax*mic*soc/(Km + soc) - rB*mic
          dcout = (1-CUE)*Vmax*soc*mic/(Km + soc)
                    
              return(list(c(dmic, dsoc, dcout)))
        })
    }
    s <- c(mic = 0.26, soc = 24.82 , cout = 0) # Initial states are taken from solved steady state (mg C/g soil)
    Temp <- approxfun(input$indexHour, input$temperatureC) #temperature input function
    Is <- approxfun(1:RunTime,Litter) #SOC input function
    output <- ode(y = s, times=times, func=derivs, parms = p) #solve ode, return output
      return(as.data.frame(cbind(time = output[1:RunTime-1,1], cout = diff(output[1:RunTime,"cout"]), soc = output[1:RunTime-1,"soc"], mic = output[1:RunTime-1,"mic"] )))

out <- NULL                           # Initalize output matrix
input <- list(inputdata)[[1]]         # Define input data
out <- as.data.frame(Model(p))        # Run model
return(out)
}
