# ENSO-Prediction

ENSO prediction using periodic AR models with non-linear predictors.

This was coded as part of a group project. All R and RMarkdown code was completed by me and is my own work. All content from pages 1-11 is **NOT** my work. Pages 12 onwards is my own work.

The model fit code contains code for fitting Autoregressive models. Although there are many R packages for fitting $AR(p)$ models a few challenges came about from using these. 

- There was no option for including seasonal predictor terms e.g $x_{n+1}=c+\beta_{1}x_n+\beta_{2}x_{n}Cos(\frac{2\pi k_{n}}{12})+\beta_{3}x_{n}Sin(\frac{2\pi k_{n}}{12})$
- 
