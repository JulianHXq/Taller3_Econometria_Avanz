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
    
// Primer ejercicio
 // Cargar base de datos
 use "${dir_bases}\smoke_day_v3.dta" , clear
// Primer ejercicio

// Punto b: declarar panel
xtset countyfip day

// Identificar primer día de tratamiento por municipio
gen treat_day = day if smoke_day == 1
egen first_treat = min(treat_day), by(countyfip)

// Crear indicador de tratamiento
gen diatrat = first_treat
replace diatrat = 0 if missing(first_treat)

// Variable de tratamiento (tratado desde su primer incendio en adelante)
gen treated = (day >= diatrat & diatrat != 0)

// Crear variable de tiempo relativo al tratamiento
gen diarel = day - diatrat

// Crear dummies para cada periodo relativo al tratamiento
tabulate diarel, generate(rel)

// Crear leads (periodos previos al tratamiento)
forvalues i = 1/10 {
    local pos = 81 - `i'
    gen lead`i' = rel`pos'
    label variable lead`i' "-`i'"
}

// Crear lags (periodos posteriores al tratamiento)
forvalues i = 0/10 {
    local pos = 81 + `i'
    gen lag`i' = rel`pos'
    label variable lag`i' "`i'"
}

// Eliminar el periodo base (r = -1)
replace lead1 = 0

// Mantener solo ventanas [-10, 10] alrededor del tratamiento
drop if (diatrat != .) & (diarel < -10 | diarel > 10)
