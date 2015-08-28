# given a set of levels for a factor
# create a function that takes a string and maps that to one of the levels
# doing as few character comparisons as possible.

if(FALSE) {
 f = genGetLevel(c("albert", "bob", "boy", "jane", "jill", "xavier", "zoe"))
 f("bob")
 f("boy")
 f("jane")
 f("jill")
 f("xavier")
 f("xavie") # matches
 f("xavier")
 f("zoe")
 f("xxx")   # matches xavier
 f("w")  # NA


   # Compile for generatinc LLVM compiled code. First parameter is the string and no need for strsplit().
  f = genGetLevel(c("albert", "bob", "boy", "jane", "jill", "xavier", "zoe"), forR = FALSE)


 # Note fails on this.  Asks if 4 element is "" for bob
  f = genGetLevel(c("bob", "bobby", "boy"))
 
}

genGetLevel =
function(labels, levels = seq(along = labels), sort = TRUE, forR = TRUE)
{

    if(sort)
       labels = sort(labels)

    names(levels) = labels

    e = mkIf(levels)

    e[[ length(e) + 1L ]] = quote(return(NA))
    
    f = function(w, token = strsplit(w, "")[[1]]) {}
    body(f)[seq(along = e) + 1L] = e

    if(!forR)
        formals(f) = alist(token=)
    
    environment(f) = globalenv()
    f
}

mkIf =
function(levels, offset = 1L)
{
    while( TRUE ) {
      first = substring(names(levels), offset, offset)
      if(length(unique(first)) > 1)
         break
       offset = offset + 1L
    }
    
    a = split(seq(along = levels), first)
    num = sapply(a, length)

    e = list()
    if(any(i <- (num == 1))) {

        u = unlist(a[i])
        e = mapply(function(v, num, i)
                      substitute(if(token[i] == x) return(num), list(i = i, num = num, x = v)),
                    first[u], levels[u], MoreArgs = list(i = offset))
        
        a = unlist(a[!i])
    }

    if(length(a))
       e = append(e, mkBranch(names(levels)[a], levels[a], first[a], offset = offset + 1L))
    
    e
}

mkBranch =
function(labels, levels, first = substring(labels, offset, offset), offset = 1L) 
{
   e = lapply(unique(first),
                function(let, offset)
                   substitute(if(token[i] == let) { tmp  }, list(let = let, i = offset - 1L)),
               offset = offset)

   names(levels) = labels

   a = split(levels, first)
   mapply(function(e, a) {
             k = mkIf(a, offset = offset)
             e[[3]][seq(along = k) + 1L] = k
             e
          }, e, a)
}




