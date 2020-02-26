print_equation= function(model, latex= FALSE, ...){
  dots <- list(...)
  cc= model$coefficients
  var_sign= as.character(sign(cc[-1]))%>%gsub("1","",.)%>%gsub("-"," - ",.)
  var_sign[var_sign==""]= ' + '
  
  f_args_sig= f_args_abs= f_args= dots
  f_args$x= cc
  f_args_abs$x= abs(cc)
  cc_= do.call(format, args= f_args)
  cc_abs= do.call(format, args= f_args_abs)
  f_args_sig$x= summary(model)$sigma
  sig= do.call(format, args= f_args_sig)
  
  if(latex){
    star= " \\cdot "
    y_var= strsplit(as.character(model$call$formula), "~")[[2]]%>%
      paste0("\\hat{",.,"_{i}}")
    x_vars= names(cc_)[-1]%>%paste0(.,"_{i}")
  }else{
    star= " * "
    y_var= strsplit(as.character(model$call$formula), "~")[[2]]        
    x_vars= names(cc_)[-1]
  }
  
  pred_vars=
    cc_abs[-1]%>%
    paste(., x_vars, sep= star)%>%
    paste(var_sign,.)%>%paste(., collapse= "")
  
  equ= paste(y_var,"=",cc_[1],pred_vars)
  if(latex){
    equ= paste0(equ," + \\hat{\\varepsilon_{i}} \\quad where \\quad \\varepsilon \\sim \\mathcal{N}(0,",
                sig,")")%>%paste0("$",.,"$")
  }
  cat(equ)
}