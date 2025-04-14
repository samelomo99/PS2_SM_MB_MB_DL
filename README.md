# PS2_SM_MB_MB_DL  
## Taller 2 - Problem Set 2: Predicting Poverty  

### Santiago Melo, Miguel Blanco, Diana Lopera  

---

## 📂 Estructura del repositorio PS2_SM_MB_MB_DL  

El repositorio está organizado en las siguientes carpetas:  

### 📄 `document`  
Contiene el documento final en formato PDF del Problem Set 2. Más adelante se puede observar un resumen del ejercicio realizado, así como los principales resultados obtenidos.  

### 📜 `scripts`  
Contiene los scripts en R utilizados para el procesamiento de datos, entrenamiento de modelos y generación de resultados:  
1. **01_preprocesamiento.R**: Limpieza y preparación de los datos, creación de nuevas variables y ajustes a las bases de entrenamiento y prueba.  
2. **02_modelos.R**: Entrenamiento y validación cruzada de distintos modelos de clasificación (Logit, Elastic Net, CART, Random Forest y XGBoost), junto con selección de hiperparámetros y evaluación de métricas.  

### 📊 `stores`  
Contiene la base de datos construida a partir de los archivos del DANE (MESEP), con variables consolidadas a nivel hogar:  
- `train_completo_hogares.csv`  
- `test_completo_hogares.csv`  

### 📈 `views`  
Almacena los cuadros y gráficas en orden según su aparición en el documento principal.  

- **Gráficas**: Se encuentran en formatos JPG y PDF, lo que permite su inclusión en Overleaf sin inconvenientes.  
- **Cuadros**: Guardados en formato `.tex`, compatibles con LaTeX.  

---

## 📌 Resumen del ejercicio  

Este trabajo tuvo como objetivo desarrollar un modelo predictivo que permitiera identificar hogares en situación de pobreza a partir de información sociodemográfica y estructural. Utilizando técnicas de machine learning sobre datos del DANE y MESE, se evaluó el rendimiento de distintos algoritmos, incluyendo regresión logística, Elastic Net, CART y Random Forest.  

La metodología incluyó validación cruzada, ajuste de hiperparámetros, comparación de métricas y análisis de importancia de variables. El modelo con mejor desempeño fue el Random Forest, el cual superó a las alternativas tradicionales tanto en sensibilidad como en AUC-ROC.  

---

### 🔍 Principales hallazgos  

- El modelo de **Random Forest** logró un **AUC-ROC superior a 0.84** y una alta capacidad de detección de hogares pobres (sensibilidad > 0.96), sin requerir técnicas explícitas de rebalanceo.  
- Las variables más importantes en la predicción fueron la **edad del jefe de hogar**, **educación promedio del hogar**, **número de ocupados** y **tasa de dependencia**, en línea con la literatura económica.  
- Modelos tradicionales como Logit y Elastic Net, si bien útiles, presentaron menor capacidad para capturar relaciones complejas y no lineales.  
- La aplicación de modelos de clasificación puede mejorar significativamente la focalización de políticas públicas, al facilitar la identificación precisa de beneficiarios potenciales, optimizando así los recursos y el diseño de intervenciones.  

---

📌 **Este repositorio corresponde a un ejercicio de análisis y predicción de la pobreza, aplicando técnicas aprendidas durante la clase de Big Data y Machine Learning de la Universidad de los Andes.**
