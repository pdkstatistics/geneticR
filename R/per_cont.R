per_cont =function(data,scale){
    if(!is.data.frame(data)){
        stop("The data should be a dataframe")}


    mmx_scale <- function(x) {       # x is a dataset
        return ((x - min(x)) / (max(x) - min(x)))}


    if(scale == TRUE){
        data1 = c()
        for (i in 1:dim(data)[2]) {
            data1=cbind(data1, as.data.frame(mmx_scale(data[i]))[,1])}
    } else {data1 = data}


    t = c()
    for (i in 1:dim(data)[2]) {
        t = cbind(t, t(combn(data1[,i],2))[,1]-t(combn(data1[,i],2))[,2])}

    r.t = t(apply(-t, 1, rank))

    cc  = cbind(Rank = colSums(r.t == 1)) %>%
        as.data.frame() %>%
        mutate('Percentage(%)' = round(Rank/sum(Rank) * 100, 3)) %>%
        `rownames<-`(names(data))

    return(cc)
}
