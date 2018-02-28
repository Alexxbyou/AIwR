# setwd("D:/bdtest/AIwR")
# code<-scan("D:/bdtest/AIwR/BDVis.R",what = "character",sep="\n",blank.lines.skip=F)
# write.code(add.fn.content(code),"D:/bdtest/AIwR/BDVis.R")
##########################################################################
##########################################################################

# Title: AI Analysis - Visualization
# Description: Univariate visualization & Comparison


##########################################################################
##########################################################################


library(stringr)

find.all.fn<-function(code){
  r.ind<-grep("<-\\s*function\\(",code)
  if(length(r.ind)>0){
    code.sub<-code[r.ind]
    fn.name<-gsub(
      "<-","",
      str_extract(code.sub,"[^(\\s)].*<-"))
    df<-data.frame(
      FunctionName=fn.name,
      RowNumber=r.ind,
      stringsAsFactors=F
    )
    return(df)
  }
}

find.fn.desc.session<-function(code){
  start<-grep("## Functions ===",code)
  end<-grep("## ===",code)
  if(length(start)>0&length(end)>0)return(c(start,end))
}

add.fn.content<-function(code){
  fn.session<-find.fn.desc.session(code)
  if(!is.null(fn.session)){
    code<-code[-(fn.session[1]:fn.session[2])]
    new.fn.start<-fn.session[1]
  }else{
    new.fn.start<-grep("# Description",code)+1
  }
  fn.df<-find.all.fn(code)
  fn.df$RowNumber<-fn.df$RowNumber+nrow(fn.df)+2
  fn.content<-c("## Functions ===",paste("##\t",fn.df$FunctionName,"\t",fn.df$RowNumber,sep=""),"## ===")
  if(!is.null(new.fn.start)){
    code<-c(code[1:(new.fn.start-1)],fn.content,code[new.fn.start:length(code)])
  }else{
    code<-c(fn.content,code)
  }
  return(code)
}

write.code<-function(code,path){
  write.table(code,path,row.names=F,col.names = F,quote=F)
}

CodeReorg<-function(path){
  code<-scan(path,what = "character",sep="\n",blank.lines.skip=F)
  write.code(add.fn.content(code),path)
}





