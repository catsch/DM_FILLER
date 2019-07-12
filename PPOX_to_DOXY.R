PPOX_to_DOXY <- function ( PRES_DOXY, TEMP_DOXY, PSAL_DOXY, PPOX_DOXY){

# Based on Henry Bittig work
# Here we change
# DOXY micromol/kg -> DOXY micromol/L -> PPOX
# dens_ss=sw_pden(pts.psal,pts.temp,pts.pres,0); % potential density at S, T, P with 0 dbar as reference
#% DOXY umol kg-1 -> molar O2 umol L-1 -> pO2 hPa
#O2molar=M.doxy.data.*(dens_ss./1000);

#Some constants

atm_press=1 # in atm
xO2 <- 0.20946 # mole fraction of O2 in dry air (Glueckauf 1951)
Vm  <- 0.317 # molar volume of O2 in m3 mol-1 Pa dbar-1 (Enns et al. 1965)
R   <- 8.314 # universal gas constant in J mol-1 K-1
a_pCoef2 <- 0.00022 # DO 4330
a_pCoef3 <- 0.0419  # DO 4330

#######################################################
## PPOX_DOXY en atm
#######################################################
PPOX=PPOX_DOXY/1013.25 # PPOX en atm, PPOX_DOXY en mbar

#######################################################
# Density conversion -> To check
#######################################################
swRho_DOXY=swRho(PSAL_DOXY, TEMP_DOXY, PRES_DOXY)

########################################################

pH2Osatsal =   exp(24.4543-(67.4509*(100/(TEMP_DOXY+273.15)))-(4.8489*log(((273.15+TEMP_DOXY)/100)))-0.000544*PSAL_DOXY) # in atm

sca_T=log((298.15-TEMP_DOXY)/(273.15+TEMP_DOXY))

# O2 solubility separated in Scorr and Tcorr (SCOR WG 142 coefficients: Garcia and Gordon 1992, Benson and Krause 1984 refit )
TCorr <- 44.6596*(exp(2.00907+3.22014*sca_T+4.05010*sca_T^2+4.94457*sca_T^3-2.56847e-1*sca_T^4+3.88767*sca_T^5))
Scorr <- exp(PSAL_DOXY*(-6.24523e-3-7.37614e-3*sca_T-1.03410e-2*sca_T^2-8.17083e-3*sca_T^3)-4.88682e-7*PSAL_DOXY^2)

DOXY_cp=PPOX*(TCorr*Scorr)/(exp(Vm*PRES_DOXY/(R*(TEMP_DOXY+273.15)))*(xO2*(atm_press-pH2Osatsal))) # (DOXY en micromol / l)

##########################################################
# Invert the Pressure effect
##########################################################

DOXY = DOXY_cp * (1 + ((a_pCoef2 *TEMP_DOXY ) + a_pCoef3) * PRES_DOXY /1000)

DOXY_DOXY=DOXY*1000/swRho_DOXY  # DOXY en micromol / kg

return(DOXY_DOXY)

}

