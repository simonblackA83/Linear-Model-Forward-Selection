
#pass your linear model object (dataframe), your desired p-value and your dataset to the function
#the function will carry out a forward selection of your "best" linear model, quitting the process at your desired level (p-value) 

forward_p = function(obj,p,data){
  
  forward = function(obj){
    
    f = formula(obj)
    txt = as.formula(paste(f[2],paste(1),sep="~"))
    empty = lm(txt,data=data)
    
    selection_one = function(obj, empty){
      
      m = add1(empty, scope = obj, test="F")
      index = which.min(m[,6])
      select = rownames(m)[index]
      one = ".~."
      paste = as.formula(paste(one,paste(select),sep="+"))
      fw = update(empty,paste) 
      return(fw)
    }
    
    fw = selection_one(obj, empty)
  }
  
  fw = forward(obj)
  
  selection = function(obj, fw,m){
    
    f_2 = formula(fw)
    index = which.min(m[,6]); input = rownames(m)[index]
    paste = as.formula(paste(f_2[2], "~", paste(f_2[3],input,sep="+")))
    fw = update(fw,paste)
    return(fw)
    
  }
  
  m = add1(fw, scope = obj, test="F")
  length = length(m[,6])
  n = 0
  
  
  
  for (x in 1:length(m[,6]-1)){
    
    if(sort(m[,6], decreasing=F)[1] <= p){
      fw = selection(obj,fw,m)
      n = n + 1
    } 
    else break
    
    if ((n+1) < length) m = add1(fw, scope = obj, test="F")
    else break
  }
  return(fw)
}
