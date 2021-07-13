appendList<-function#Binds two lists together
### Appends the second list at the first and returns the new list 
###
### FunctionGroup:
### HelperTool
(l1,##<< The list as countable list l1[[1..n]]
### The first list. 
l2##<< The list as countable list l2[[1..m]]
### The second list. 
){
  lng<-length(seq(along = l1))
  ##ll<-list()
  ll<-l1
  for (i in seq(along = l2)) {
    ll[[i+lng]]<-l2[[i]]
  }
  return(ll)
### ll is the combined list. As countable list ll[[1..n+m]]
}

