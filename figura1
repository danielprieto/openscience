#Graphic based on a solution by Alex at https://stackoverflow.com/questions/32046889/connecting-two-points-with-curved-lines-s-ish-curve-in-r
#Create the function
curveMaker <- function(x1, y1, x2, y2, ...){
  curve( plogis( x, scale = 0.08, loc = (x1 + x2) /1.8 ) * (y2-y1) + y1, 
         x1, x2, add = TRUE, ...)
}

#Prepare data:
parent <- c(2, 56)
children <- cbind(3, cbind(1:9)*11.5-1)
grandchildren <- cbind(7, (1:24)*4.5-1)
ggchildren <- cbind(12, c(15,20,25,30,35,50,55,60,65,90,97,105))
labels <- c("Ciencia Abierta (OS) ", "Herramientas de OS","Proyectos de OS","Políticas de OS", "Lineamientos de OS","Evaluación de la OS","Definición de OS", "Investigación Reproducible","Datos Abiertos (DA)", "Acceso Abierto (OA)", "Herramientas de flujo de trabajo abierto", "Servicios abiertos", "Repositorios abiertos","Políticas por tema", "Mandatos organizacionales","Revisión por pares abierta", "Métricas e impacto abiertos", "Testeo de reproducibilidad", "Lineamientos de reproducibilidad","Código abierto en OS", "Flujos de trabajo en OA", "Cuadernos de laboratorio abiertos", "Estudios de irreproducibilidad", "Definición de OS reproducible", "Datos gubernamentales abiertos", "Uso y reuso de DA", "Estándares de DA", "Revistas de DA", "Definición de DA", "Grandes DA", "Uso y reuso de OA", "Rutas de OA","Iniciativas de OA", "Definicion de OA", "Políticas de OD", "Políticas de OA", "Políticas institucionales" , "Políticas de gobierno", "Políticas agencias financiadoras", "Webometrics", "Semantometrics", "Bibliometrics", "Altmetrics", "Ruta verde","Ruta Dorada", "Ruta Diamante?" )


#Make a blank plot canvas
plot(0, type="n", ann = FALSE, xlim = c( 0, 15.5 ), ylim = c( 0, 110), axes = FALSE )

#Plot curves
#Parent and children
invisible( mapply( curveMaker, 
                   x1 = parent[ 1 ], 
                   y1 = parent[ 2 ], 
                   x2 = children[ , 1 ], 
                   y2 = children[ , 2 ], 
                   col = "grey", lwd = 3 ) )

#Children and grandchildren
invisible( mapply( curveMaker, #first node
                   x1 = children[ 9, 1 ], 
                   y1 = children[ 9, 2 ], 
                   x2 = grandchildren[ 21:24 , 1 ], 
                   y2 = grandchildren[ 21:24, 2 ], 
                   col = "coral", lwd = 3 ) ) 
invisible( mapply( curveMaker, #second node
                   x1 = children[ 8, 1 ], 
                   y1 = children[ 8, 2 ], 
                   x2 = grandchildren[ 15:20 , 1 ], 
                   y2 = grandchildren[ 15:20, 2 ], 
                   col = "gray", lwd = 3 ) )
invisible( mapply( curveMaker, #third node
                   x1 = children[ 7, 1 ], 
                   y1 = children[ 7, 2 ], 
                   x2 = grandchildren[ 7:14 , 1 ], 
                   y2 = grandchildren[ 7:14, 2 ], 
                   col = "burlywood", lwd = 3 ) )
invisible( mapply( curveMaker, #fourth node
                   x1 = children[ 5, 1 ], 
                   y1 = children[ 5, 2 ], 
                   x2 = grandchildren[ 4:6 , 1 ], 
                   y2 = grandchildren[ 4:6, 2 ], 
                   col = "darkseagreen", lwd = 3 ) )
invisible( mapply( curveMaker, #fifth node
                   x1 = children[ 3, 1 ], 
                   y1 = children[ 3, 2 ], 
                   x2 = grandchildren[ 1:3 , 1 ], 
                   y2 = grandchildren[ 1:3, 2 ], 
                   col = gray( 0.6, alpha = 0.6 ), lwd = 3 ) )

#Grandchildren and ggchildren
invisible( mapply( curveMaker, 
                   x1 = grandchildren[ 4, 1 ], 
                   y1 = grandchildren[ 4, 2 ], 
                   x2 = ggchildren[ 1:2 , 1 ], 
                   y2 = ggchildren[ 1:2, 2 ], 
                   col = "darkseagreen1", lwd = 2 ) ) 
invisible( mapply( curveMaker, 
                   x1 = grandchildren[ 5, 1 ], 
                   y1 = grandchildren[ 5, 2 ], 
                   x2 = ggchildren[ 3:5 , 1 ], 
                   y2 = ggchildren[ 3:5, 2 ], 
                   col = "darkseagreen3", lwd = 2 ) )
invisible( mapply( curveMaker, 
                   x1 = grandchildren[ 7, 1 ], 
                   y1 = grandchildren[ 7, 2 ], 
                   x2 = ggchildren[ 6:9 , 1 ], 
                   y2 = ggchildren[ 6:9, 2 ], 
                   col = "burlywood", lwd = 2 ) )
invisible( mapply( curveMaker, 
                   x1 = grandchildren[ 22, 1 ], 
                   y1 = grandchildren[ 22, 2 ], 
                   x2 = ggchildren[ 10:12 , 1 ], 
                   y2 = ggchildren[ 10:12, 2 ], 
                   col = "coral3", lwd = 2 ) )
#Plot text
text( x = c(parent[1], children[,1], grandchildren[,1], ggchildren[,1]), 
      y = c(parent[2], children[,2], grandchildren[,2], ggchildren[,2]),
      labels = labels,
      pos = c(2,4, 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4),
      cex = 1.2) 

#Plot points
points( x = c(parent[1], children[,1], grandchildren[,1], ggchildren[,1]),
        y = c(parent[2], children[,2], grandchildren[,2], ggchildren[,2]), 
        pch = 21, bg = "white", col="black", lwd=2.5, cex=1)
