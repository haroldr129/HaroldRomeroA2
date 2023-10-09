#' Funcion para que el usuario digite un numero entre 1 y 3 y le retorne la solucion de cada uno de los ejercicios propuestos
#' @param seleccionar es un numero
#' @return De acuerdo con la opcion seleccionada, el programa llevara al usuario al numero del punto solucionado, mostrando al usuario un mensanje de la opcion seleccionada
#' en caso de que el numero no se encuentre dentro de los numeros del 1 al 3, arrojara un error
#' @examples
#' retrieve_answer(1)
#' retrieve_answer(2)
retrieve_answer <- function(seleccionar)
{
  seleccionar <- as.integer(readline("Ingrese una opción entre 1 y 3: "))

  # Verificar si la opción ingresada es válida
  if (grepl("^\\d+$", seleccionar)) {  # Utiliza una expresión regular para verificar números enteros
    seleccionar <- as.integer(seleccionar)
  }
  else {
    cat("Error: Por favor, ingrese un número entero válido.\n")
    retrieve_answer}

  if (seleccionar >= 1 && seleccionar <= 3) {
    cat("Has seleccionado la opción:",seleccionar, "\n")
    if (seleccionar==1)
    {
      cat("Punto 1", "\n")
      source("C:\\Users\\harold.romero\\G. BARCO S.A\\Usuarios - Harold Romero\\Documentos\\Documentos\\Harold\\PERSONAL\\Universidad\\Electiva 3\\R PROYECTOS\\HaroldRomeroA2\\R\\Punto1.R")
    }
    else {
      if(seleccionar==2){
        cat("Punto 2", "\n")
        source("C:\\Users\\harold.romero\\G. BARCO S.A\\Usuarios - Harold Romero\\Documentos\\Documentos\\Harold\\PERSONAL\\Universidad\\Electiva 3\\R PROYECTOS\\HaroldRomeroA2\\R\\Punto2.R")
      }
      else{
        if(seleccionar==3)
        {
          cat("Ejercicios 5.4", "\n")
          source("C:\\Users\\harold.romero\\G. BARCO S.A\\Usuarios - Harold Romero\\Documentos\\Documentos\\Harold\\PERSONAL\\Universidad\\Electiva 3\\R PROYECTOS\\Classwork\\R\\Opcion3.R")
        }}}}
  else {
    cat("Error: La opción debe estar entre 1 y 3.\n")
    seleccionar <- as.integer(readline("Ingrese una opción entre 1 y 3: "))}
}
retrieve_answer()


