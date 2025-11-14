# suicidios_0-20-ARGENTINA
El código de este repositorio puede utilizarse para analizar y graficar la mortalidad por distintos grupos de causas en Argentina para los años 2015-2023 inclusive. Los datos provienen del DEIS MinSal (http://datos.salud.gob.ar/dataset/datos-salud-gob-ar-dataset-defunciones-mensuales-ocurridas-en-la-republica-argentina).
El código que proveo en el archivo test2_mort_suicidios_ped.R permite generar/entrenar modelos GAM (modelos generales aditivos) en base a los datos de mortalidad 2015-2019 que permiten capturar tendencias crecientes, decrecientes y estacionales para cada causa de muerte, por grupo etario. Esto permite estimar cuantas muertes esperaríamos por cada causa (mejor dicho, grupo de causas) para cada mes de 2020, 2021, 2022 y 2023.

El primer gráfico que arroja el código grafica las muertes mensuales esperadas por cada grupo de causas para el grupo etario de 0 a 20 años en distintos colores, y las muertes reales observadas en color negro. Se indican como dos líneas rojas verticales los meses que corresponden a los picos de casos de COVID-19 en 2020, 2021 y 2022.

![Mortalidad mensual por distintas causas 0-20 años Argentina](https://github.com/rquiroga7/suicidios_0-20-ARGENTINA/blob/main/0-20_tendencias_mortalidad_mensual_causas_picos.png)

El segundo gráfico indica la cantidad de suicidios mensuales para el grupo etario de 0 a 20 años desagregados por sexo (Varones y Mujeres) en negro, en azul la cantidad esperada para varones en base a las tendencias 2015-2019, y en rojo la cantidad esperada para mujeres.

![Suicidios mensuales 0-20 años Argentina por sexo](https://github.com/rquiroga7/suicidios_0-20-ARGENTINA/blob/main/0-20_tendencias_mortalidad_mensual_SUICIDIOS_POR_SEXO.png)

El tercer gráfico muestra la mortalidad anual total por distintas causas para el grupo etario de 0 a 20 años, sin proyecciones estadísticas, permitiendo una visualización más simple de las tendencias anuales.

![Mortalidad anual por causas 0-20 años Argentina](https://github.com/rquiroga7/suicidios_0-20-ARGENTINA/blob/main/0-20_mortalidad_anual_causas_simple.png)

El cuarto gráfico presenta los suicidios anuales en el grupo de 0 a 20 años desagregados por región geográfica. Las regiones se agrupan en: NOA (Noroeste Argentino), NEA (Noreste Argentino), Cuyo, Centro y Patagonia, permitiendo observar diferencias regionales en las tendencias de mortalidad por suicidio.

![Suicidios anuales por región 0-20 años Argentina](https://github.com/rquiroga7/suicidios_0-20-ARGENTINA/blob/main/0-20_mortalidad_anual_SUICIDIOS_POR_REGION.png)

El quinto gráfico muestra los suicidios anuales desagregados por región y sexo para el grupo de 0 a 20 años. Los varones se representan en azul y las mujeres en rojo, permitiendo comparar las tendencias entre sexos dentro de cada región geográfica.

![Suicidios anuales por región y sexo 0-20 años Argentina](https://github.com/rquiroga7/suicidios_0-20-ARGENTINA/blob/main/0-20_mortalidad_anual_SUICIDIOS_POR_REGION_Y_SEXO.png)

Ante cualquier duda, sugerencia o correcciones que tengan, contactarse conmigo por Twitter: https://twitter.com/rquiroga777
