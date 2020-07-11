PatternCount <-function(Text, Pattern){
  Text <- strsplit(Text, split=NULL)[[1]]
  Pattern <- strsplit(Pattern, split=NULL)[[1]]
  count<-0
  if(length(Pattern)>length(Text)){
    stop("Error: Length of pattern should be less than length of text")
  }else{
    for(i in 1:(length(Text)-length(Pattern)+1)){
      if(sum(Text[i:(i+length(Pattern)-1)] == Pattern) == length(Pattern)){
        count <- count+1
      }
    }
    return(count)
  }
}