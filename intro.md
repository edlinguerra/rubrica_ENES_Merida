# Instrucciones de uso

Esta aplicación web apoya el procesamiento, análisis y preselección de
postulaciones para impartir docencia en **todas las licenciaturas de la
ENES Mérida**, conforme a los *Criterios Generales para la Asignación de
Docencia 2027‑1*, aprobados por el H. Consejo Técnico.

El sistema organiza la información declarada en los formularios
institucionales, aplica la **Rúbrica Institucional de Valoración de
Perfiles Docentes**, y genera insumos para la toma de decisiones
colegiadas por parte de los Comités Académicos (CA).  
Su uso es **estrictamente auxiliar** y **no constituye un mecanismo
automático de asignación**.

------------------------------------------------------------------------

## Paso 1. Preparación de datos

Cargue el archivo .csv con las respuestas del formulario institucional,
tal como fue entregado por la Secretaría Técnica de las Licenciaturas.

Al presionar **Ordenar solicitudes**, la aplicación:

-   organiza la información del formulario,  
-   asigna un ID único a cada solicitud,  
-   estructura los datos por profesor y asignatura.

La tabla resultante se muestra en la pestaña **Postulaciones ordenadas**
y puede descargarse en formato `.csv`.

------------------------------------------------------------------------

## Paso 2. Aplicación de la rúbrica institucional

Cargue la rúbrica validada por el Comité Académico en formato Excel.  
Al presionar **Aplicar rúbrica**, el sistema asigna puntajes a cada
profesor según los criterios institucionales aprobados por el H. Consejo
Técnico.

La tabla detallada se muestra en **Postulaciones valoradas en detalle**
y debe descargarse en `.csv` para su revisión por el CA.

Posteriormente, el botón **Preseleccionar** genera la valoración global
por solicitud, ponderada por carga horaria y clasifica cada postulación
como:

-   **VALIDAR POR CA**, o  
-   **CONDICIONADA**.

El Comité Académico deberá revisar estas solicitudes, asignar la
valoración que corresponde al rubro 6 (**Desempeño previo**) y, en su
caso, modificar la columna *adjudicación* a:

-   **VALIDADA**,  
-   **CONDICIONADA**, o  
-   **NO ADJUDICADA**,

según los criterios académicos establecidos en los *Criterios Generales
para la Asignación de Docencia 2027‑1*.

------------------------------------------------------------------------

## Paso 3. Datos para nómina

Cargue el archivo Excel con las adjudicaciones validadas por el CA.  
Al presionar **Adjudicados**, la aplicación filtra las solicitudes
correspondientes y muestra la tabla en la pestaña **Adjudicados**.

El botón **Datos para nómina** genera la tabla con la información
requerida para la elaboración de nombramientos y documentación
administrativa.

------------------------------------------------------------------------

## Paso 4. Identificación de asignaturas desiertas

Cargue el catálogo de asignaturas convocadas, con las columnas. Al
presionar **Identificar desiertas**, la aplicación contrasta el catálogo
con las asignaturas adjudicadas y muestra en la pestaña **Desiertas**
aquellas que no recibieron una postulación válida.

------------------------------------------------------------------------

# Sobre esta aplicación

Esta herramienta fue desarrollada en **R** y **Shiny** por el  
**Dr. Edlin Guerra Castro**, Secretario Académico, y la **Dra. Aline
Romero Natale**, Secretaria Técnica de Licenciaturas.

Versión de la aplicación: **0.1.0**  
Aplicable al proceso de planeación docente del semestre **2027‑1**.
