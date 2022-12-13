# NextGame
Utility to predict next playable game.
###Pintar diagrama flujo


###Data Adquisition-Data Pre-Processing-Machine Algo-Pattern evaluation-Knowledge Representation
###                               - Data analysis/Representation


Por hacer:

Proyecto 1
X - Problema con desconexión, se pierde un match (regular el valor de "i")
- Buscar el hit por similarity?
    X - Problemas con los ":"
    X - Algunos no se encuentan porque aparecen escritos de forma similar. Problemas con Complete o Remaster
    X - Problema con juegos como Trine 2, no es el mismo orden

X - Completar el mayor número de NA posibles
- Descargar otros metadatos. Género, Calidad, fecha lanzamiento...
- Integrar
X - Qué hacer con los que acaban con un espacio...quitar el espacio? (Need for Speed Heat)
X - Warhammer 40,000 no sale bien, me cargo las ","
X - Probar el archivo solo con las últimas lineas de "Game.txt", que no tenga que quitarlas a mano
X - Quitar simbolos especiales del Game.txt, pero mantener en una columna el nombre original

Proyecto 2
Visualizar datos
- Juegos con logros más fáciles: diferencia menor de tiempo entre completacionista y base, se puede relativizar a tiempo de juego base y se genera un índice (cuanto más alto mejor)
- Etiqueta de "Tiempo en completarlo/logros", "Removed", "Géneros", "Completado", "Puntuación en %", "Fecha salida", "Cuándo se compró"

Proyecto 3
Recomendación juegos
- Unsupervised, clusters en función de parámetros

Proyecto 4
Recomendación de compra



4-12-22

- Arreglado los caracteres con TM y los ü
- Agrega info de Completationist

La info de Completationist no la coge bien en la tabla, arreglar...

5-12-22

- Eliminado los duplicados que aparecen con espacios
- Puestos tiempos de juego base y completacionista en una sola línea
- Eliminado lo de que aparezca el nombre del juego dos veces

6-12-22

- No se salta un elemento de la lista cuando hay error, la recorremos con un While. 
- Comprueba que el archivo Game.txt existe, no vuelve a descargar el archivo desde Steam.
- Busca también coincidencias cercanas, apunta cuales pasan por exactas y cuales no
- elimina todos los "\u{4}" 
- Quita también los | 

7-12-22

- Grandes cambios.
  - Se filtran los resultados y se coge el de mayor similitud, no el primero.
  - Se han hecho varios cambios de control de filtrado de información para evitar problemas al procesas el dato.
  - Esto soluciona el problema de Trine 2 y similares

8-12-22

- Modificación grande para los ":", no va bien del todo...ver por qué algunos se abren...monitorear durante ejecución

9-12-22

- Fallaba por cómo se gestiona "&". Sigue fallando con el cambio que tiene hecho, modificarlo (tiene que estar) y ver si a los juegos que tienen "Collector's edition" etc les afecta y se arregla al cortar esa cola
- Nueva forma del pasted, pero fallan otros juegos como "Mount & Blade: With Fire and Sword". Ver qué pasa, antes iba...Probar también Red Comrades Save the Galaxy: Reloaded

10-12-22

- Mount & Blade arreglado, Red Comrades no. Quitar ":" y "-" en la próxima versión
- SEGA Mega Drive & Genesis Classics

11-12-22

- Arreglado los problemas, los NA solucionados. Ver cómo limitar los falsos positivos

12-12-22

- Hay que revisar el xls, vamos por el 200 de los no match
- Voy por el 400
- Arreglado los problemas con "Reload" y "Collection". Se han dejado, el único collection que se ha modificado es el "Sonic Transformed Collection".
- Arreglado los casos en que no se daba el hit y se intentaba poner ":", no se ponía bien en esta versión y solo buscaba la primera palabra.
- Parece que se ha mejorado todo lo que había que mirar, revisar la lista de los NA y los No Exact a ver qué se puede apurar...
- Quedan muy pocos en No Exact, se han ido mucho a NA...por ejemplo PUBG, estaba bien. Comparar el V3 y V4!

13-12-22

- Si no encuentra hit, mirar si tiene Disney al principio. Si lo tiene, quitarlo.
- Si no encuentra nada, después de poner ":", buscar solo la primera palabra...si solo hay un hit, guardarlo.
- Las dos cosas ya funcionan
- Arreglar: Saw, Monaco, Resident Evil 0, Age of Empires II (2013), ARCADE GAME SERIES: PAC-MAN: 1h 3h Ms. Pac-Man. Entran en Exact pero no deberían, corregir esa parte del código para que solo guarde los exactos 100%.
- No se cortan bien los nombres para generar Games.txt. Problemas con el juego anterior a: "Aesthetic World", "F1 Race Stars"
- Arreglado el recorte de los nombres. Ahora no aparecen nombres duplicados, se puede eliminar esa parte de código! Además los dos de arriba ya aparecen bien.
- Generamos el V5, comparar con V3 y V4, a ver cuántos se han solucionado.
- Hay mucho Exact que no está bien...mirar los que están en la línea 4, a ver qué se puede hacer..contar los problemas que hay! El V6 será la versión donde los exact sean exact perfectos...ahí caerán los :, '...mirar bien
- Enhanced edition habría que dejarlo...cuál más dejar? Ver cuántos hay de casa caso, dejar un archivo Game.txt con todos ellos (sacar con un grep)