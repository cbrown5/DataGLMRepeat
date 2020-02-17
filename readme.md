# DataGLMRepeat: Tools that speed up the repetitive bits of GLMing

 CJ Brown  2020
 
 
Is the monotony of fitting GLM after GLM getting you down? DataGLMRepeat is here to help. 

This package includes tools for: 
- Calculating Dunn-Smyth residuals and a rootogram given observations and model predictions  
- Plotting a semivariogram with an arbitrary distance matrix  
- repeatedly fitting GLMs based on parameters and formulas given in a data frame. 
- And a range of other miscellaneous functions that Chris finds helpful for plotting 

To install this package use:

`remotes::install_github("cbrown5/DataGLMRepeat")`

This package also has some vignette guides to model checking, to access those do: 

`remotes::install_github("cbrown5/DataGLMRepeat", build_vignettes = TRUE)`

Then to get started in R see `vignette("checking-multiple-models")` and `vignette("with_groups")`. 

To see all available functions do: `help(package = "DataGLMRepeat")`

 
TODO:
Add vignette with advice on checking spatial residuals, 
including example with gam, sf and generating model for spatial AC
