#' Modelo de un compartimento no infusi�n ecuaci�n anal�tica
#'
#' @param D0 Dosis, (esquema de administraci�n) - [mg]
#' @param Vd Volumen de Distribuci�n [L]
#' @param ke Constante de velocidad de eliminaci�n [h^{-1}]
#' @param t Tiempo de observaci�n [horas]
#'
#' @return Cp Concentraci�n plasm�tica [mg/L, mcg/mL]
#' @export
#'
#' @examples cmptm.1.IV(D0 = 4000, Vd = 3.6, ke = 0.234, t = 1) = 879.2909 
cmptm.1.IV = function(D0, Vd, ke, t) {
  Cp = (D0/Vd)*exp(-ke * t)
  return(Cp)
  }

#' Modelo de Un Compartimento sin Infusi�n (Eq. anal�tica) PO
#'
#' @param D0 Dosis, (esquema de administraci�n) - [mg]
#' @param ka Constante de velocidad de absorci�n [h^{-1}]
#' @param ke Constante de velocidad de eliminaci�n [h^{-1}]
#' @param Vd Vd Volumen de Distribuci�n [L]
#' @param t Tiempo de observaci�n [horas]
#'
#' @return Cp Concentraci�n plasm�tica [mg/L, mcg/mL]
#' @export
#'
#' @examples cmptm.1.PO(D0 = 200, ka = 1.02, Vd = 4.6, ke = 0.343, t = 0.5) = 15.8463
cmptm.1.PO = function(D0, ka, ke, Vd, t) {
  Cp = (ka*D0/(Vd*(ka-ke)))*(exp(-ke*t)-exp(-ka*t))
  return(Cp)
}