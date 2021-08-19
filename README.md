# SAS_Macro
In development: macros to perform basic univariate and bivariate analysis using SAS

To univariate analysis:
   %NUMERICAL(data, var, *alpha=);
 
   %NUMERICAL(data, var, *class, *alpha=); * for grouped analysis;

   %CATEGORICAL(data, var,* alpha=);

To bivariate analysis:
%BIVAR_CAT_CAT(data, var1, var2, *alpha=)

%BIVAR_CAT_CONT(data, class, var, *alpha=)

%BIVAR_CONT_CONT(data, var1, var2)


*alpha= , is not a mandatory parameter
* class for grouped univariate numerical analysis is optional.
