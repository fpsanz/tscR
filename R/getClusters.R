# Función clustering a partir de matriz de distancias slopes
#TODO hay que controlar tamaño máximo de la matrix
getClusters <- function(x, k){
  require(cluster)
  if( !inherits(x, "dist") ){
    stop("X must be a dist class matrix")
  }
  myclust <- pam(x, k=k, diss = T)
  return(myclust)
}
