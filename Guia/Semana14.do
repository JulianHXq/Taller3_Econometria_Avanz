* Econometría Avanzada
* Clase Complementaria
* 2025-1

clear all
cap log close
set more off
cls

if "`c(username)'"=="macbookair2" {
	
	global main "/Users/lucia_mr/Dropbox/5. Complementarias/2025-1/Econ 4301/Complementaria/Clase 14/"
	cd "$main"
 }
	
else if "`c(username)'"=="mora"{
	
	global main ""
	cd "$main"
 } 
 
 else //cd "ESTUDIANTE: COPIE AQUÍ LA RUTA DE SU DIRECTORIO."

*********************************************************************************
** CLASE 14 - CONTROL SINTÉTICO					  		    		  		   **
*********************************************************************************

*-------------------------------------------------------------------------------
* 1. Control Sintético
* ------------------------------------------------------------------------------
*ssc install synth, replace
*findit synth_runner

use "texas.dta", replace
describe
xtdes

	* Tratamiento : Expansión de los establecimientos carcelarios.
	* Outcome de interés (Y): Tasa de encarcelamiento de los hombres 
	* afroamericanos.
	* Año del tratamiento: 1993.

*) Estimación (pesos de regresión) ---------------------------------------------

	/* En esta parte buscamos los pesos tal que la sumatoria entre períodos
	de la diferencia en la tasa de encarcelamiento en los hombres entre el 
	estado tratado y todos los demás estados no tratados sea mínima
	
	Solo consideramos los años pre-tratamiento */
	preserve
	keep bmprison year statefip
	
	reshape wide bmprison, j(statefip) i(year)
	
	rename 	bmprison48 treated_bmprison
	reg treated_bmprison bmprison* if year < 1993
	
		*Algunas pesos negativos, otras positivos, otras iguales a cero.
		
	mat define synthetic = J(51,3,.)
	forvalues j = 1/50{
		mat synthetic[`j', 1] = r(table)[1,`j']  //Pesos
	}
	
	//Calculemos el Y sintético
	gen y_synth = .
	
	
	local y = 0
	foreach x of varlist bmprison*{ //valores ponderados
		local ++y
		gen `x'_ponderado = `x' * synthetic[`y', 1] 
		drop `x'
	}
	
	gen y_predicted = 0
	foreach var of varlist bmprison* {
		replace y_predicted = y_predicted + `var'
	}
	
	twoway (line treated_bmprison year) (line y_predicted year), 				///
	xline(1993, lpattern(dash)) legend(order(1 "Treated" 2 "Synthetic"))		///
	xlab(, nogrid) ylab(, nogrid) ytitle("Black male incarceration per 100,000")
	
	restore
	
	
*) Estimación (Abadie et. al) ---------------------------------------------------

synth bmprison bmprison(1990) bmprison(1991) bmprison(1992) 					///
		bmprison(1988) alcohol(1990) aidscapita(1990) aidscapita(1991)			///
		income ur poverty black(1990) black(1991) black(1992) 					///
		perc1519(1990), trunit(48) trperiod(1993) unitnames(state) 				///
		mspeperiod(1985(1)1992)
			
		* Constantes (no negativas) de cada variable independiente
		mat list e(V_matrix)
		
		* Vector de pesos 
		mat list e(W_weights)
		
*) Presentación gráfica del efecto del tratamiento
	
	* Base temporal
	tempfile synth
	
	* Estimación en una base
	qui synth bmprison bmprison(1990) bmprison(1991) bmprison(1992) 			///
		bmprison(1988)	alcohol(1990) aidscapita(1990) aidscapita(1991)			///
		income ur poverty black(1990) black(1991) black(1992) 					///
		perc1519(1990), trunit(48) trperiod(1993) unitnames(state) 				///
		mspeperiod(1985(1)1992) keep(`synth')
	
	* Ajuste de la base
	use `synth', replace
	drop _Co_Number _W_Weight
	rename (_time _Y_treated _Y_synthetic) (year treat counterfact)
	
	* Gráfica
	twoway (line treat year,lp(solid)lw(vthin)lcolor(black))					///
			(line counterfact year,lp(solid)lw(vthin)lcolor(navy)),				///
			xline(1993, lpattern(shortdash) lcolor(black)) 						///
			xtitle("Año",si(medsmall)) xlabel(#10) 								///
			ytitle("Tasa de encarcelamiento", size(medsmall)) ///
			graphregion(fcolor(white))
	
	
	*Efecto
	gen effect=treat-counterfact
	
	twoway (line effect year,lp(solid)lw(vthin)lcolor(black)),				///
		xline(1993, lpattern(shortdash) lcolor(black)) 						///
		xtitle("Año",si(medsmall)) xlabel(#10) 								///
		ytitle("Efecto en la tasa de encarcelamiento", size(medsmall)) legend(off)		///
		graphregion(fcolor(white))


	
	
*) Placebo espacial 
use "texas.dta", replace
		
synth_runner bmprison bmprison(1990) bmprison(1992) bmprison(1991) 				///
	bmprison(1988) alcohol(1990) aidscapita(1990) aidscapita(1991) 				///
	income ur poverty black(1990) black(1991) black(1992) 						///
	perc1519(1990), trunit(48) trperiod(1993) unitnames(state) 					///
	mspeperiod(1985(1)1992) gen_vars
	
	* P-valor estandarizado de H0: No hay efecto del tratamiento
	di e(pval_joint_post_std)
	
	* P-valores estandarizados para año t de H0: No hay efecto del tratamiento 
	* en t
	mat l e(pvals_std)
	
	
	
	* Ajuste de la base
	keep effect year statefip
	reshape wide effect, i(year) j(statefip)
	rename effect48 texas 
		
	* Gráfica
	twoway (line effect1 -effect20 year, lc(gray*0.75 ...) lw(vthin ...))		///
			(line effect21 -effect35 year, lc(gray*0.75 ...) lw(vthin ...))		///
			(line effect36 -effect56 year, lc(gray*0.75 ...) lw(vthin ...))		///
			(line texas year, lc(black)),										///
			legend(off) graphregion(fcolor(white)) 								///
			xline(1993, lpattern(shortdash) lcolor(black))						///
			yline(0, lpattern(shortdash) lcolor(black)) 						///
			xtitle("Año",si(medsmall))
			
