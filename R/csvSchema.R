# Given the schema, generate a routine specific to that schema to read a CSV file

AllocRoutines = c("character" = "NEW_CHARACTER",
                  "integer" = "NEW_INTEGER",
                  "numeric" = "NEW_NUMERIC",
                  "logical" = "NEW_LOGICAL",
                  "factor" = "NEW_INTEGER"
                 )

DataAccessors = c("character" = NA,
                  "integer" = "INTEGER",
                  "numeric" = "REAL",
                  "logical" = "LOGICAL",
                  "factor" = "INTEGER"
                 )
PtrDecls  = c("character" = NA,
              "integer" = "int *",
              "numeric" = "double *",
              "logical" = "int *",
               "factor" = "int *"
              )

convertPrimitive = c("integer" = "atoi",
                     "factor" = "atoi",
                     "logical" = "atoi",
                     "numeric" = "atof")

NAs = c("integer" = "R_NaInt",
        "factor" = "R_NaInt",
        "logical" = "R_NaInt",
        "numeric" = "R_NaReal")



if(FALSE) {
 x = genCCSVSchemaReader(colClasses = c(rep("character", 3), "integer", "factor", "character", "character", "integer", "integer",  "integer", rep("numeric", 4)), header = TRUE)
 includes = c("#include <Rdefines.h>", "#include <stdio.h>", "#include <stdlib.h>",
              "char *nextToken(FILE *, int *end);")
 nt = readLines("~/GitWorkingArea/RCompileIdioms/src/nextToken.c")
 cat(includes, "", "", nt, "", "", x, file = "/tmp/foo.c", sep = "\n")
 system("R CMD SHLIB /tmp/foo.c")


 N = 1e7L
 f = path.expand("~/Data/NYTaxis/trip_data_1.csv")
 dyn.load("/tmp/foo.so")
# system.time({ o = as.data.frame(.Call("readCSV", f, N), stringsAsFactors = FALSE)})
 varNames = c("medallion", "hack_license", "vendor_id", "rate_code", "store_and_fwd_flag", 
"pickup_datetime", "dropoff_datetime", "passenger_count", "trip_time_in_secs", 
"trip_distance", "pickup_longitude", "pickup_latitude", "dropoff_longitude", 
"dropoff_latitude")
 system.time({ o = structure(.Call("readCSV", f, N), class = "data.frame", row.names = NULL, names = varNames)}) 
 system.time(read.csv(f, stringsAsFactors = FALSE, nrow = N))
}

genCCSVSchemaReader =
    # C version
function(funName = "readCSV", ..., colClasses = list(...), header = NA)
{

    if(is.null(names(colClasses)))
        names(colClasses) = paste0("V", seq(along = colClasses))

    rvars = paste0("r", names(colClasses))
    txt = paste("SEXP", paste(rvars, collapse = ", "), ";")
    txt1 = sprintf("PROTECT(%s = %s(n));", rvars, AllocRoutines[colClasses])
    
    isPrimitive = !is.na(PtrDecls[colClasses])
    primitive = colClasses[isPrimitive]
    txt2 = paste0(paste(PtrDecls[primitive], names(primitive), collapse = ";\n"), ";")
    
    txt3 = sprintf("%s = %s(%s);", names(primitive), DataAccessors[primitive], rvars[isPrimitive])


    txt4 = c("int i = 0;", "char *cur;", "int end = 0;")
    txt5 = "while(i < n) {"

    txt6 = c("cur = nextToken(con, &end);",
              "if(end)",
              "   break;"
#    ,    'printf("i = %d\\n", i);'
            )

    setEls = rep("", length(colClasses))
    setEls[isPrimitive] = sprintf("%s[i] = cur ? %s(cur) : %s;",
                                     #DataAccessors[ primitive ],
                                     names(primitive),
                                     convertPrimitive[ primitive ],
                                     NAs[ primitive ])

    isNULL = colClasses == "NULL"

    setEls[!isPrimitive & !isNULL] = sprintf("SET_STRING_ELT(%s, i, cur ? mkChar(cur) : R_NaString);", rvars[!isPrimitive & !isNULL])

    setEls[isNULL] = ""
    
    loopBody = sprintf("%s\n %s", c("", rep("cur = nextToken(con, NULL);", length(setEls[-1]))), setEls)
    
    txt7  = c("i++;", "}")
    

       
     makeDf = c("SEXP ans;",
                sprintf("PROTECT(ans = NEW_LIST(%d));", length(colClasses)),
                sprintf("SET_VECTOR_ELT(ans, %d, %s);", seq(along = colClasses) - 1L, rvars))

    open = c('FILE *con = fopen(CHAR(STRING_ELT(r_filename, 0)), "r");',
             'if(!con) {',
                'PROBLEM "cannot open %s", CHAR(STRING_ELT(r_filename, 0))', 
                'ERROR;',
             '}'
            )

    fgets = "char dummy[100000];\n fgets(dummy, 100000, con);"

    if(is.na(header)) {
       fgets = sprintf("if(LOGICAL(header)[0]) { \n%s\n}", fgets)
    } else if(!header)
        fgets = ""
    
    c("SEXP",
      sprintf("%s(SEXP r_filename, SEXP r_num%s)", funName, if(is.na(header)) ", SEXP header" else ""),
      "{",
      open,
      fgets,
      "int n = INTEGER(r_num)[0];",
      txt, txt1, txt2, txt3, txt4, txt5, txt6, loopBody, txt7, makeDf, 
      sprintf("UNPROTECT(%d);", length(colClasses) + 1L),
      "return(ans);",
      "}")
}
    





###############################


genCSVSchemaReader =
function(..., colClasses = list(...), m = Module())
{

    if(is.null(names(colClasses)))
        names(colClasses) = paste0("V", seq(along = colClasses))

    fun = Function("read", SEXPType, list(filename = StringType, nrow = Int32Type), module = m)
    params = getParams(fun, FALSE)
    
    
    init = BasicBlock(fun, "init")
    ir = IRBuilder(init)

    mapply(createLocalVars,
           names(colClasses), AllocRoutines[colClasses], 
            MoreArgs = list(sizeParam = params[[2]], builder = ir))
    
    
}

createLocalVars =
function(id, alloc, sizeParam, builder)
{
    var = createLocalVariable(ir, SEXPType, paste0("r", id))
    col = createCall(alloc, )
    ir$createStore(col, var)
}



