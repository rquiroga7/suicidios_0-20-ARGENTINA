# suicidios_0-20-ARGENTINA
El código de este repositorio puede utilizarse para analizar y graficar la mortalidad por distintos grupos de causas en Argentina para los años 2015-2023 inclusive. Los datos provienen del DEIS MinSal (http://datos.salud.gob.ar/dataset/datos-salud-gob-ar-dataset-defunciones-mensuales-ocurridas-en-la-republica-argentina).
El código que proveo en el archivo test2_mort_suicidios_ped.R permite generar/entrenar modelos GAM (modelos generales aditivos) en base a los datos de mortalidad 2015-2019 que permiten capturar tendencias crecientes, decrecientes y estacionales para cada causa de muerte, por grupo etario. Esto permite estimar cuantas muertes esperaríamos por cada causa (mejor dicho, grupo de causas) para cada mes de 2020, 2021, 2022 y 2023.

El primer gráfico que arroja el código grafica las muertes mensuales esperadas por cada grupo de causas para el grupo etario de 0 a 20 años en distintos colores, y las muertes reales observadas en color negro. Se indican como dos líneas rojas verticales los meses que corresponden a los picos de casos de COVID-19 en 2020, 2021 y 2022.

![Mortalidad mensual por distintas causas 0-20 años Argentina](https://github.com/rquiroga7/suicidios_0-20-ARGENTINA/blob/main/0-20_tendencias_mortalidad_mensual_causas_picos.png)

El segundo gráfico indica la cantidad de suicidios mensuales para el grupo etario de 0 a 20 años en negro, y en rojo la cantidad esperada en base a las tendencias 2015-2019.

![Suicidios mensuales 0-20 años Argentina](https://github.com/rquiroga7/suicidios_0-20-ARGENTINA/blob/main/0-20_tendencias_mortalidad_mensual_SUICIDIOS.png)

Ante cualquier duda, sugerencia o correcciones que tengan, contactarse conmigo por Twitter: https://twitter.com/rquiroga777
