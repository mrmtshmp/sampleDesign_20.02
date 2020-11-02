#' (one phrase discription what this function does.)
#' 
#' @parameter df2 A data.frame object.
#' @parameter column_for_Fisher A numeric vector indexing target column.
#' 
#' @export

FisherAll<-function (df2, column_for_Fisher){
  Fisher_result <- 
    c("Namej","Namei","j0_i0","j0_i1","j1_i0","j1_i1","p.value", "j.pos", "i.pos") 
  
  for (j in column_for_Fisher){
    for (i in column_for_Fisher){
      i<-c(colnames(df2)[j], colnames(df2)[i], xtabs(~unlist(df2[j])+unlist(df2[i]), df2)[1,1], xtabs(~unlist(df2[j])+unlist(df2[i]), df2)[1,2],xtabs(~unlist(df2[j])+unlist(df2[i]), df2)[2,1],xtabs(~unlist(df2[j])+unlist(df2[i]), df2)[2,2],fisher.test(xtabs(~unlist(df2[j])+unlist(df2[i]), df2),alternative ="t")$p.value, table(df2[j])[2], table(df2[i])[2])
      Fisher_result<-rbind(Fisher_result,i)
      }
    }
    Fisher_result
    }