makeCachematrix<-function(x=matrix()){
        invx<-matrix()
        set<-function(y){
                x<<-y
                invx<<-matrix()
        }
        get<-function()
                x
        setinv<-function(inv)
                invx<<-inv
        getinv<-function()
                invx
        list(set=set,get=get,setinv=setinv,getinv=getinv)
                
}

cacheSolve<-function(x,...){
        invx<-x$getinv()
        if(!is.na(invx)){
                message("Has been calculated")
                return(solve(invx))
        }
        data<-x$get()
        invx<-solve(data,...)
        x$setinv(invx)
        invx
}
