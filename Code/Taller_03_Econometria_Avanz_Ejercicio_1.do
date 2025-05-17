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

//---------------------------Punto b--------------------------------------------

// Declarar panel
xtset countyfip day

// Identificar primer día de tratamiento por municipio
gen treatday = day if smoke_day == 1
egen firsttreat = min(treatday), by(countyfip)

// Crear indicadora de tratamiento
gen diatrat = firsttreat
replace diatrat = 0 if missing(firsttreat)

// Variable de tratamiento (tratado desde su primer incendio en adelante)
gen treated = (day >= diatrat & diatrat != 0)

// Crear variable de tiempo relativo al tratamiento
gen diarel = day - diatrat

// Crear dummies para cada periodo relativo al tratamiento
tabulate diarel, generate(rel)

// Crear leads (periodos previos al tratamiento)
forvalues i = 1/10 {
    local pos = 81 - `i'
    gen lead_`i' = rel`pos'
    label variable lead_`i' "-`i'"
}

// Crear lags (periodos posteriores al tratamiento)
forvalues i = 0/10 {
    local pos = 81 + `i'
    gen lag_`i' = rel`pos'
    label variable lag_`i' "`i'"
}

// Eliminamos el periodo base r=-1
replace lead_1 = 0

// Mantener solo ventanas [-10, 10] alrededor del tratamiento
drop if (diatrat != .) & (diarel < -10 | diarel > 10)

//---------------------------Punto d--------------------------------------------

// Instalar paquetes

ssc install reghdfe, replace
ssc install ftools, replace
ssc install moremata, replace

ssc install coefplot, replace

ssc install outreg2, replace

// Obtener el día de la semana a partir de la fecha
gen diasem = dow(date)

// Metodología TWFE dinámico

reghdfe pm25 lead_* lag_*, absorb(countyfip day diasem) cluster(countyfip) noconstant

// Visualizar resultados dinámicos
coefplot, keep(lead_* lag_*) xline(0)

// TWFE estático

reghdfe pm25 treated, absorb(countyfip date diasem) cluster(countyfip) noconstant

// Exportar resultados
outreg2 using "estatico.doc", replace ///
    addtext("Efectos fijos de municipio, fecha y dia semana")

// Punto f

// Definir número de periodos antes y después del tratamiento
global n_leads = 10
global n_lags  = 10

// Nombre de la variable de evento
global evento_var "event_plot"

// Estilo del gráfico
global estilo_base "default_look"

// Etiquetas de ejes
global eje_x "xlabel(-`n_leads'(1)`n_lags')"
global eje_y "ytitle(Impacto estimado)"
global titulo_grafico "xtitle(Días relativos al tratamiento)"

// Agrupar todas las opciones en una sola macro
global opciones_plot "$eje_x $eje_y $titulo_grafico"

// Mostrar todos los efectos juntos
global agrupado "together"