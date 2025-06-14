# Análisis de Tendencias en Pinterest

Este repositorio contiene un análisis de tendencias utilizando datos extraídos de Pinterest Trends. Se han sacado datos de hombres y mujeres de Europa del Sur y de Hispanoamérica a lo largo del año 2024.

## Estructura del repositorio

- `Análisis de tendencias.R`: Código en R que realiza el análisis de los datos. 
- `datos/`: Archivos con los datos usados para el análisis en (.xlsx).
- `presentación/`: Presentación en RMarkdown en la que aparecen los resultados de los análisis y su explicación de manera esquematizada partiendo de los análisis que realizamos dentro de un documento Word, así como en los otros apartados del proyecto.
- `Pinterest-Trends.Rproj`: Archivo de proyecto de RStudio con las rutas relativas correctas.

## Instrucciones script
Todas las instrucciones del desarrollo del análisis están aclaradas dentro del propio script, es decir, en el archivo `Análisis de tendencias.R`.

Asegurarse de que están instalados todos los paquetes necesarios para que funcione el script con los análisis y la presentación. Estos están indicados también en el archivo (.R).

⚠️ IMPORTANTE: Abrir el proyecto con el archivo `Pinterest-Trends.Rproj` antes de inciar el script o el RMarkdown. Es necesario para que las rutas relativas (here::here()) funcionen correctamente en todos los equipos y se puedan abrir sin problema los archivos. 

En cualquier caso ejecutar el `Análisis de tendencias.R` con todos sus paquetes y posteriormente abrir la `presentación/`, para abrirla es necesario que esté intalado el paquete `here` que aparece en el `.R` junto al resto de paquetes. 
