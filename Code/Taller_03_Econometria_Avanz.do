  *==========================================================================*
  *                      Taller 2: Econometría avanzada 				     *
  *==========================================================================*
  cls  
  clear all
  
**# Integrantes
  *Julian Herrera - 202120246
  *Nicolas Torres - 201822010

  /*
  Fecha de entrega: 16 de mayo del 2025
  Profesor: Manuel Fernandez
  */
clear all
cap log close
set more off
cls

**# Directorio		

if "`c(username)'"=="macbookair2" {
	
	global main "/Users/lucia_mr/Dropbox/5. Complementarias/2025-1/Econ 4301/Complementaria/Clase 14/"
	cd "$main"
 }
	
else if "`c(username)'"=="mora"{
	
	global main ""
	cd "$main"
 } 
 
 else cd "C:\Users\User\Desktop\EconoAvanz\Taller_03"
  
  * Definir donde estan las bases y donde se quiere guardar los resultados 
  * (Se pone el nombre de cada carpeta dentro de comillas Ej: "Bases")
  
  global dir_bases "Bases" /* Directorio para traer las bases */
  global dir_resultados "Resultados" /*Directorio para guardar resultados */
    
**# 1. Exploración de la base 


   /*
  *==========================================================================*
  * Taller 1: Econometría avanzada - Efecto de que una mujer gane las elecciones
									 generales dado que gana en primera vuelta*
  *==========================================================================*
  Primer ejercicio
    Descripción: Se desarrolla el primer ejercicio del taller mediante la 
				 aplicacion de una regresion discontinua para poner en 
				 evidencia si existen limitaciones dado que se pone a una 
				 mujer en las elecciones generales dado componentes no 
				 observables de discriminacion*/
	
   use "${dir_bases}\Cuba_RC.dta", clear /* Abrir base */

use "Cuba_RC.dta", clear
describe
xtdes

* Gráfica 1: evolución en Cuba
twoway (line imr year if country=="cuba"), ///
    title("Mortalidad Infantil en Cuba (1950-1975)") ///
    ytitle("Muertes por cada 1,000 nacidos vivos") ///
    xtitle("Año")
	graph export "${dir_resultados}\Mortalidad_Infantil_Cuba.png", as(png) replace


	
* Gráfica 2: América Latina
twoway (line imr year if country=="cuba", lcolor(blue)) ///
       (line imr year if country=="honduras", lcolor(gs10)) ///
       (line imr year if country=="peru", lcolor(gs12)) ///
       (line imr year if country=="elsalvador", lcolor(gs14)) ///
       (line imr year if country=="costarica", lcolor(gs16)) ///
       (line imr year if country=="uruguay", lcolor(gs8)) ///
       (line imr year if country=="venezuela", lcolor(gs6)), ///
    legend(order(1 "Cuba" 2 "Honduras" 3 "Peru" 4 "El Salvador" 5 "Costa Rica" 6 "Uruguay" 7 "Venezuela")) ///
    title("Comparación de Mortalidad Infantil: Cuba y países LATAM")
	graph export "${dir_resultados}\Mortalidad_Infantil_Cuba_Latam.png", as(png) replace

	
* Gráfica 3: países desarrollados
twoway (line imr year if country=="cuba", lcolor(blue)) ///
       (line imr year if country=="sweden", lcolor(gs10)) ///
       (line imr year if country=="belgium", lcolor(gs12)) ///
       (line imr year if country=="germany", lcolor(gs14)) ///
       (line imr year if country=="italy", lcolor(gs16)) ///
       (line imr year if country=="unitedstates", lcolor(gs8)), ///
    legend(order(1 "Cuba" 2 "Sweden" 3 "Belgium" 4 "Germany" 5 "Italy" 6 "USA")) ///
    title("Comparación de Mortalidad Infantil: Cuba y países desarrollados")
	graph export "${dir_resultados}\Mortalidad_Infantil_Cuba_Euro.png", as(png) replace


*-------------------------------------------------------------------------------
* 1. Control Sintético Punto b
* ------------------------------------------------------------------------------
*ssc install synth, replace
*findit synth_runner
*net install synth_runner, from("https://raw.githubusercontent.com/bquistorff/synth_runner/master/") replace

	* Tratamiento : Expansión de los establecimientos carcelarios.
	* Outcome de interés (Y): Tasa de encarcelamiento de los hombres 
	* afroamericanos.
	* Año del tratamiento: 1993.

*) Estimación (pesos de regresión) ---------------------------------------------

	/* En esta parte buscamos los pesos tal que la sumatoria entre períodos
	de la diferencia en la tasa de encarcelamiento en los hombres entre el 
	estado tratado y todos los demás estados no tratados sea mínima
	
	Solo consideramos los años pre-tratamiento */
	
	**use "C:\Users\User\Desktop\EconoAvanz\Taller_03\Bases\Cuba_RC.dta", clear


*ssc uninstall synth
*ssc install synth, replace

use "C:\Users\User\Desktop\EconoAvanz\Taller_03\Bases\Cuba_RC.dta", clear

// 1. Crear indicador de país LATAM
gen latam = ///
    inlist(country, "argentina", "chile", "colombia", "costarica", "elsalvador", "guatemala", "honduras", "mexico", "peru") | ///
    inlist(country, "uruguay", "venezuela", "cuba")

// 2. Lista de variables en formato ancho
local vars imr gdppc urban leo(1950) leo(1955) leo(1958) schoolalt(1950) school(1950) school(1955)

* 1. Crear un archivo de resultados
tempname memhold
tempfile resultados
postfile `memhold' str20 variable int year float latam_mean notlatam_mean using `resultados', replace

* 2. Variables dependientes del año
foreach var in leo school schoolalt {
    foreach y in 1950 1955 1958 {
        quietly summarize `var' if latam == 1 & year == `y', meanonly
        local mean_latam = r(mean)
        
        quietly summarize `var' if latam == 0 & year == `y', meanonly
        local mean_nolatam = r(mean)
        
        post `memhold' ("`var'") (`y') (`mean_latam') (`mean_nolatam')
    }
}
// Variables sin año
foreach var in imr gdppc urban {
    quietly summarize `var' if latam == 1, meanonly
    local mean_latam = r(mean)
    
    quietly summarize `var' if latam == 0, meanonly
    local mean_nolatam = r(mean)
    
    post `memhold' ("`var'") (.) (`mean_latam') (`mean_nolatam')
}


postclose `memhold'

use `resultados', clear
list, sepby(year)

putexcel set "$dir_resultados\promedios_latam.xlsx", replace

// Encabezados
putexcel A1 = ("Variable") B1 = ("Año") C1 = ("LATAM Mean") D1 = ("No LATAM Mean")

// Escribir fila por fila
local row = 2
quietly {
    gen rownum = _n
    foreach r of numlist 1/`=_N' {
        putexcel A`row' = variable[`r'] ///
                 B`row' = year[`r'] ///
                 C`row' = latam_mean[`r'] ///
                 D`row' = notlatam_mean[`r']
        local row = `row' + 1
    }
}

*------------------------Synthetic control----------------------------------
use "C:\Users\User\Desktop\EconoAvanz\Taller_03\Bases\Cuba_RC.dta", clear

* Ejecutar método de control sintético
synth imr imr gdppc urban ///                                 
      leo(1950) leo(1955) leo(1958) ///                         
      schoolalt(1950) school(1950)  school(1955), ///
      trunit(9) trperiod(1959) unitnames(country) mspeperiod(1950(1)1958)///
	  
		* Constantes (no negativas) de cada variable independiente
		mat list e(V_matrix)
		
		* Vector de pesos 
		mat list e(W_weights)


		// 1. Guardar la matriz de pesos en una nueva matriz
		matrix W = e(W_weights)

		clear
		set obs `=rowsof(W)'

		gen peso = .
		forvalues i = 1/`=_N' {
			replace peso = W[`i',2] in `i'
		}


		local countries : rownames W
		gen str20 country = ""
		local i = 1
		foreach name of local countries {
			replace country = "`name'" in `i'
			local ++i
		}
		keep if peso > 0

		*----------------------Weights Synthetic control-----------------------
		graph bar peso, over(country, sort(1) descending) ///
			bar(1, color(dknavy)) ///
			title("Pesos en el país sintético de Cuba") ///
			ytitle("Peso") ///
			ylabel(, format(%4.2f))
		graph export "${dir_resultados}\Weight_Cuba_Synthetic.png", as(png) replace

		
use "C:\Users\User\Desktop\EconoAvanz\Taller_03\Bases\Cuba_RC.dta", clear

*) Presentación gráfica del efecto del tratamiento
	
	* Base temporal
	tempfile synth
	
	* Estimación en una base
	qui synth imr imr gdppc urban ///                                 
		  leo(1950) leo(1955) leo(1958) ///                         
		  schoolalt(1950) school(1950)  school(1955), ///
		  trunit(9) trperiod(1959) unitnames(country) 				///
		  mspeperiod(1950(1)1958) keep(`synth')
		
	* Ajuste de la base
	use `synth', replace
	drop _Co_Number _W_Weight
	rename (_time _Y_treated _Y_synthetic) (year treat counterfact)
	
	* Gráfica
	twoway (line treat year,lp(solid)lw(vthin)lcolor(black))					///
			(line counterfact year,lp(solid)lw(vthin)lcolor(navy)),				///
			xline(1959, lpattern(shortdash) lcolor(black)) 						///
			xtitle("Año",si(medsmall)) xlabel(#10) 								///
			ytitle("Tasa de mortalidad infantil por cada 1000 habitantes", size(medsmall)) ///
			graphregion(fcolor(white))
			graph export "${dir_resultados}\Mortalidad_Infantil_Cuba_Synthetic.png", as(png) replace
//list year treat counterfact, sep(0)

	*Efecto
		gen effect=treat-counterfact
		
		twoway (line effect year,lp(solid)lw(vthin)lcolor(black)),				///
			xline(1959, lpattern(shortdash) lcolor(black)) 						///
			xtitle("Año",si(medsmall)) xlabel(#10) 								///
			ytitle("Efecto del tratamiento en el contrafactual", size(medsmall)) legend(off)		///
			graphregion(fcolor(white))
			graph export "${dir_resultados}\Mortalidad_Infantil_Cuba_Contrafactual_effect.png", as(png) replace

//list year effect counterfact, sep(0)

	
*-------------------- Placebo espacial Synthetic control----------------------
use "C:\Users\User\Desktop\EconoAvanz\Taller_03\Bases\Cuba_RC.dta", clear

	synth_runner imr imr gdppc urban ///
		leo(1950) leo(1955) leo(1958) school(1950) school(1955) schoolalt(1950), ///
		trunit(9) trperiod(1959) unitvariable(id) timevariable(year) ///
		mspeperiod(1950(1)1960) gen_vars
		


		di e(pval_joint_post_std)
		
		* en t
		mat l e(pvals_std)
		
		* Ajuste de la base
		keep effect year id
		reshape wide effect, i(year) j(id)
		rename effect9 cuba 
		
		* Gráfica
	twoway ///
		(line effect1-effect12 year, lc(gray*0.75 ...) lw(vthin)) ///
		(line effect13-effect33 year, lc(gray*0.75 ...) lw(vthin)) ///
		(line cuba year, lc(blue)), ///
		legend(off) graphregion(fcolor(white)) ///
		xline(1959, lpattern(shortdash) lcolor(black)) ///
		yline(0, lpattern(shortdash) lcolor(black)) ///
		xtitle("Año",si(medsmall))
		graph export "${dir_resultados}\Mortalidad_Infantil_Cuba_Spacial_Placebo.png", as(png) 	
  
*-------------------- Proof Temporal Placebo----------------------
use "C:\Users\User\Desktop\EconoAvanz\Taller_03\Bases\Cuba_RC.dta", clear

* Ejecutar método de control sintético
synth imr imr gdppc urban ///                                 
      leo(1950) leo(1955) leo(1958) ///                         
      schoolalt(1950) school(1950)  school(1955), ///
      trunit(9) trperiod(1955) unitnames(country) mspeperiod(1950(1)1958)///
	  
		* Constantes (no negativas) de cada variable independiente
		mat list e(V_matrix)
		
		* Vector de pesos 
		mat list e(W_weights)


		// 1. Guardar la matriz de pesos en una nueva matriz
		matrix W = e(W_weights)

		clear
		set obs `=rowsof(W)'

		gen peso = .
		forvalues i = 1/`=_N' {
			replace peso = W[`i',2] in `i'
		}


		local countries : rownames W
		gen str20 country = ""
		local i = 1
		foreach name of local countries {
			replace country = "`name'" in `i'
			local ++i
		}
		keep if peso > 0

		*----------------------Weights Synthetic control-----------------------
		graph bar peso, over(country, sort(1) descending) ///
			bar(1, color(dknavy)) ///
			title("Pesos en el país sintético de Cuba") ///
			ytitle("Peso") ///
			ylabel(, format(%4.2f))
		graph export "${dir_resultados}\Temporal_Placebo_Weight_Cuba_Synthetic.png", as(png) replace

		
use "C:\Users\User\Desktop\EconoAvanz\Taller_03\Bases\Cuba_RC.dta", clear

*) Presentación gráfica del efecto del tratamiento
	
	* Base temporal
	tempfile synth
	
	* Estimación en una base
	qui synth imr imr gdppc urban ///                                 
		  leo(1950) leo(1955) leo(1958) ///                         
		  schoolalt(1950) school(1950)  school(1955), ///
		  trunit(9) trperiod(1955) unitnames(country) 				///
		  mspeperiod(1950(1)1958) keep(`synth')
		
	* Ajuste de la base
	use `synth', replace
	drop _Co_Number _W_Weight
	rename (_time _Y_treated _Y_synthetic) (year treat counterfact)
	
	* Gráfica
	twoway (line treat year,lp(solid)lw(vthin)lcolor(black))					///
			(line counterfact year,lp(solid)lw(vthin)lcolor(navy)),				///
			xline(1955, lpattern(shortdash) lcolor(black)) 						///
			xtitle("Año",si(medsmall)) xlabel(#10) 								///
			ytitle("Tasa de mortalidad infantil por cada 1000 habitantes", size(medsmall)) ///
			graphregion(fcolor(white))
			graph export "${dir_resultados}\Temporal_Placebo_Mortalidad_Infantil_Cuba_Synthetic.png", as(png) replace
//list year treat counterfact, sep(0)

	*Efecto
		gen effect=treat-counterfact
		
		twoway (line effect year,lp(solid)lw(vthin)lcolor(black)),				///
			xline(1955, lpattern(shortdash) lcolor(black)) 						///
			xtitle("Año",si(medsmall)) xlabel(#10) 								///
			ytitle("Efecto del tratamiento en el contrafactual", size(medsmall)) legend(off)		///
			graphregion(fcolor(white))
			graph export "${dir_resultados}\Temporal_Placebo_Mortalidad_Infantil_Cuba_Contrafactual_effect.png", as(png) replace
	list year effect counterfact, sep(0)

*-------------------- Synthetic control with Latam Doonors----------------------

use "C:\Users\User\Desktop\EconoAvanz\Taller_03\Bases\Cuba_RC.dta", clear

// 1. Crear indicador de país LATAM
gen latam = ///
    inlist(country, "argentina", "chile", "colombia", "costarica", "elsalvador", "guatemala", "honduras", "mexico", "peru") | ///
    inlist(country, "uruguay", "venezuela", "cuba")


// 2. Filtrar solo LATAM
keep if latam == 1

// 3. Ejecutar el método de control sintético con placebo temporal en 1955
synth imr imr gdppc urban ///                                 
      leo(1950) leo(1955) leo(1958) ///                         
      schoolalt(1950) school(1950)  school(1955), ///
      trunit(9) trperiod(1959) unitnames(country) ///
      mspeperiod(1950(1)1958)

* -------------------------------- *
*    Matrices de resultados        *
* -------------------------------- *

mat list e(V_matrix)     // Pesos de variables
mat list e(W_weights)    // Pesos de países

// 4. Extraer pesos de países y graficarlos
matrix W = e(W_weights)
clear
set obs `=rowsof(W)'

gen peso = .
forvalues i = 1/`=_N' {
	replace peso = W[`i',2] in `i'
}

local countries : rownames W
gen str20 country = ""
local i = 1
foreach name of local countries {
	replace country = "`name'" in `i'
	local ++i
}

keep if peso > 0

graph bar peso, over(country, sort(1) descending) ///
	bar(1, color(dknavy)) ///
	title("Pesos en el país sintético de Cuba (LATAM)") ///
	ytitle("Peso") ///
	ylabel(, format(%4.2f))

graph export "${dir_resultados}\Placebo_LATAM_Pesos_Cuba_Synthetic.png", as(png) replace


*---------------------------------------------------*
*    Gráfica de la evolución del efecto placebo     *
*---------------------------------------------------*

// Recargar base original con LATAM
use "C:\Users\User\Desktop\EconoAvanz\Taller_03\Bases\Cuba_RC.dta", clear
gen latam = ///
    inlist(country, "argentina", "chile", "colombia", "costarica", "elsalvador", "guatemala", "honduras", "mexico", "peru") | ///
    inlist(country, "uruguay", "venezuela", "cuba")

keep if latam == 1

// Guardar resultado  del placebo
tempfile synthlatam

	synth_runner imr imr gdppc urban ///
		leo(1950) leo(1955) leo(1958) school(1950) school(1955) schoolalt(1950), ///
		trunit(9) trperiod(1959) unitvariable(id) timevariable(year) ///
		mspeperiod(1950(1)1960) gen_vars
		


		di e(pval_joint_post_std)
		
		* en t
		mat l e(pvals_std)
		

use `synthlatam', replace
drop _Co_Number _W_Weight
rename (_time _Y_treated _Y_synthetic) (year treat counterfact)

twoway (line treat year, lp(solid) lw(vthin) lcolor(black)) ///
	   (line counterfact year, lp(solid) lw(vthin) lcolor(navy)), ///
	   xline(1959, lpattern(shortdash) lcolor(black)) ///
	   xtitle("Año", si(medsmall)) xlabel(#10) ///
	   ytitle("Tasa de mortalidad infantil por cada 1000 habitantes", size(medsmall)) ///
	   title("Control Sintético (LATAM)") ///
	   legend(label(1 "Cuba") label(2 "Contrafactual sintético")) ///
	   graphregion(fcolor(white))

graph export "${dir_resultados}\Placebo_LATAM_Mortalidad_Infantil_Cuba_Synthetic.png", as(png) replace
list year treat counterfact, sep(0)
  
  
*------------------------------------------------------------------------------
use "C:\Users\User\Desktop\EconoAvanz\Taller_03\Bases\Cuba_RC.dta", clear
gen latam = ///
    inlist(country, "argentina", "chile", "colombia", "costarica", "elsalvador", "guatemala", "honduras", "mexico", "peru") | ///
    inlist(country, "uruguay", "venezuela", "cuba")


keep if latam == 1


* 2. Crear variable de mortalidad infantil relativa al año 1959
gen has1959 = 0
bysort id (year): replace has1959 = 1 if year == 1959 & !missing(imr)
bysort id (year): replace has1959 = has1959[_n-1] if missing(has1959)
bysort id (year): replace has1959 = has1959[_n+1] if missing(has1959)

gen imr1959 = .
replace imr1959 = imr if year == 1959
bysort id (year): replace imr1959 = imr1959[_n-1] if missing(imr1959)
forvalues i = 1/9 {
    bysort id (year): replace imr1959 = imr1959[_n+1] if missing(imr1959)
}

gen imr_rel = imr / imr1959

* 3. Guardar datos por si acaso


qui synth imr_rel imr gdppc urban ///
     leo(1950) leo(1955) leo(1958) ///
     schoolalt(1950) school(1950)  school(1955), ///
     trunit(9) trperiod(1959) unitnames(country) ///
     mspeperiod(1950(1)1958) keep("synth_output.dta", replace)

		
	use "synth_output.dta", replace
	drop _Co_Number _W_Weight
	rename (_time _Y_treated _Y_synthetic) (year treat counterfact)
	
	* Gráfica
	twoway (line treat year,lp(solid)lw(vthin)lcolor(black))					///
			(line counterfact year,lp(solid)lw(vthin)lcolor(navy)),				///
			xline(1959, lpattern(shortdash) lcolor(black)) 						///
			xtitle("Año",si(medsmall)) xlabel(#10) 								///
			ytitle("Tasa de mortalidad infantil por cada 1000 habitantes", size(medsmall)) ///
			graphregion(fcolor(white))

			graph export "${dir_resultados}\MortalidadRelativa_LATAM_Cuba_Synthetic_imr_rel.png", as(png) replace
	

list year treat counterfact if year <= 1980, sep(0)

 
  
  
  
  
  