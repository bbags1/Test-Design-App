modelparams <- function(df){
  for (z in c(1:length(names(df)))){
    if (z == 1){
      params <- combn(names(df), m = z)  
    }
    
    else if (z == 2) {
      df1 = combn(names(df), m = z, simplify = TRUE)
      l = dim(df1)[2]
      for (x in c(1:l)){
        int = paste(df1[1,x], df1[2,x], sep = ":")
        params = append(params, int)
        
      }
    }
    else if(z == 3){
      df1 = combn(names(df), m = z, simplify = TRUE)
      l = dim(df1)[2]
      for (x in c(1:l)){
        int = paste(df1[1,x],df1[2,x], df1[3,x], sep = ":")
        params = append(params,int)
      }
    }
    else if(z == 4){
      df1 = combn(names(df), m = z, simplify = TRUE)
      l = dim(df1)[2]
      for (x in c(1:l)){
        int = paste(df1[1,x],df1[2,x], df1[3,x], df1[4,x], sep = ":")
        params = append(params,int)
      }
    }
    else if(z == 5){
      df1 = combn(names(df), m = z, simplify = TRUE)
      l = dim(df1)[2]
      for (x in c(1:l)){
        int = paste(df1[1,x],df1[2,x], df1[3,x], df1[4,x], df1[5,x],sep = ":")
        params = append(params,int)
      }
    }
    
    
    
  }
  print(params) 
}
