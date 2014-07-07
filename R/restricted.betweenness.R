restricted.betweenness <- function(g, from=V(g), to=V(g), parallel=T){
  require("plyr")

  rb.one.node <- function(i){
    sp = get.all.shortest.paths(g, i, to)$res
    sp = sp[sapply(sp, function(x) length(x) > 2)]
      
    if(length(sp) > 0){  
      path.lengths = sapply(sp, length) - 2
      ends = unlist(sapply(1:length(path.lengths), 
                    function(i) rep(sp[[i]][length(sp[[i]])], path.lengths[[i]])))
      mids = unlist(sapply(sp, function(x) x[2:(length(x)-1)]))
      sp.df = data.frame(mid=mids, end=ends)
      
      value = 1/table(sp.df$end)
      value = data.frame(end=names(value), value=as.numeric(value))
      sp.df = merge(sp.df, value)
      
      bt = aggregate(sp.df$value, by=list(sp.df$mid), sum)
      colnames(bt) = c("vid", "bt")
      return(bt)
    }
    else{
      return(NA)
    }
  }
  rb.one.node = failwith(NA, rb.one.node)
  
  bts = llply(from, 
              rb.one.node, 
              .parallel=parallel, 
              .progress=ifelse(parallel, "none", "text"))
  
  bt = do.call(rbind, bts[!is.na(bts)])
  bt.final = aggregate(bt$bt, by=list(bt$vid), sum)
  colnames(bt.final) = c("vid", "bt")
  return(bt.final)
}
