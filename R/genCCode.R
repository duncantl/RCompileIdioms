if(FALSE) {
g = genCollector()
k = genXMLSchemaCCode(s[[1]]$PageType, s, collector = g)
writeCode(g, "/tmp/wiki.c")
}


genCollector =
function(id = NA)
{
   routines = list()
   structure(list(routines = function() structure(routines, class = "RoutineList"),
                  addRoutine = function(r, name) {
                      routines[[name]] <<- r
                    },
                  hasRoutine = function(name) name %in% names(routines)),
              class = "RoutineCollector")
}

names.RoutineCollector =
function(x)
    names(x$routines())


writeCode =
function(x, file = stdout(), ...)
  UseMethod("writeCode")

writeCode.RoutineCollector =
function(x, file = stdout(), ...)
  writeCode(x$routines(), file, ...)




writeCode.RoutineList = 
function(x, file = stdout(), SupportRoutinesFile = system.file("Ccode", "xmlValue_GetAttr.c", package = "RCompileIdioms"),
          makevars = system.file("Make/", "akevars", package = "RCompileIdioms"), ...)
{
  mk = file.path(dirname(file), "Makevars")
  if(!file.exists(mk))
      file.copy(makevars, mk)
  
  code = sapply(x, formatRoutine)
  cat("#include <Rdefines.h>",
      "#include <R_ext/Arith.h>",
      "#include <libxml/tree.h>",
      readLines(SupportRoutinesFile),
      code,
      file = file, sep = "\n\n", ...)
}

formatRoutine =
function(x)
{
  x[-c(1:3, length(x))] = paste0("\t", x[-c(1:3, length(x))])
  paste(x, collapse = "\n")       
}


#############

genXMLSchemaCCode =
    #
    # for a given XML element name, generate the code to read the contents of the node into a list.
    #
    #  genXMLSchemaCCode(s[[1]]$PageType, s)
    #
function(desc, schema, collector = genCollector())
{
    rname = paste0("R_mk_", desc@name)
    if(collector$hasRoutine(rname))
        return(collector$routines()[[rname]])
    
    els = desc@slotTypes
    nels = length(els)

    code = c("SEXP ans, names, val;",
             sprintf("PROTECT(ans = NEW_LIST(%d));", nels),
             sprintf("xmlNodePtr cur = node%s;",  if(is(desc, "ClassDefinition") && any(sapply(desc@slotTypes, is, "AttributeDef"))) "" else "->children"))

    code = c(code,
             mapply(mkElCode, els, seq(along = els) - 1L, names(els), MoreArgs = list(schema = schema, collector = collector)))

    setNames = c(sprintf("PROTECT(names = NEW_CHARACTER(%d));", nels),
                 sprintf("SET_STRING_ELT(names, %d, mkChar(\"%s\"));", seq(along = els) - 1L, names(els)),
                 "SET_NAMES(ans, names);")

    code = c("SEXP",
      sprintf("%s(xmlNodePtr node)", rname),
      "{",
      code,
      "",
      setNames,
      "UNPROTECT(2);",
      "return(ans);",
      "}")

    collector$addRoutine(code, rname)

    rrname = sprintf("R_%s", desc@name)
    collector$addRoutine(sprintf("SEXP %s(SEXP r_node) { xmlNodePtr node = (xmlNodePtr) R_ExternalPtrAddr(r_node); return(%s(node));}", rrname, rname), rrname)
    
    collector
}

##################################################################################

mkElCode =
function(el, pos, nodeName = el@name, schema, var = "cur", collector = genCollector())
{
   code = ""

   nxt = "   cur = cur->next;"

   value = convertValueToR(el, var, schema, collector = collector)

   defaultValue = getDefaultValue(el, schema)


paste(   
   if(!is(el, "GenericAttributeType") && length(el@count) && el@count["min"] == 0) {
       ifCode = if(is(el, "SimpleSequenceType")) 
                   sprintf("(%s)", paste(sprintf('strcmp(cur->name, "%s") == 0', names(s[[1]]$PageType@slotTypes[[6]]@elType@slotTypes)), collapse = " || "))
                else
                   sprintf('strcmp(cur->name, "%s") == 0', nodeName)
       
      c(sprintf('if(cur && %s) {', ifCode),
        sprintf("           SET_VECTOR_ELT(ans, %d, %s);", pos, value),
        paste("        ", nxt),
       if(length(defaultValue) && defaultValue != "R_NilValue")
          c("} else",
            sprintf("SET_VECTOR_ELT(ans, %d, defaultValue);", pos))
       else
         "}")
   } else
       c(sprintf("SET_VECTOR_ELT(ans, %d, %s);", pos, value),
          nxt)
    , collapse = "\n")
}

#############################################################################

setGeneric("convertValueToR",
            function(el, varName, schema, collector, targetVar = "val")
               standardGeneric("convertValueToR"))

setMethod("convertValueToR", "SchemaVoidType",
          function(el, varName, schema, collector, targetVar = "val") {
             "ScalarLogical(1)"
          })

setMethod("convertValueToR", "SchemaStringType",
          function(el, varName, schema, collector, targetVar = "val") {
              sprintf("ScalarString(mkChar(xmlValue(%s)))", varName)
          })

setMethod("convertValueToR", "PrimitiveSchemaType",
          function(el, varName, schema, collector, targetVar = "val") {
              if(el@name %in% c("nonNegativeInteger", "positiveInteger"))
                 return(sprintf("ScalarInteger( atoi(xmlValue(%s)))", varName))
          })

setMethod("convertValueToR", "SchemaTypeReference",
          function(el, varName, schema, collector, targetVar = "val") {
              convertValueToR(resolve(el, schema), varName, schema, collector, targetVar)
          })

setMethod("convertValueToR", "LocalElement",
          function(el, varName, schema, collector, targetVar = "val") {
              convertValueToR(el@type, varName, schema, collector, targetVar)
          })

setMethod("convertValueToR", "ExtendedClassDefinition",
          function(el, varName, schema, collector, targetVar = "val") {
              if(!is(el@baseType, "SchemaStringType"))
                  stop("not yet")

               # XXX Handle the attributes
              convertValueToR(el@baseType, varName, schema, collector, targetVar)
          })

setMethod("convertValueToR", "ClassDefinition",
          function(el, varName, schema, collector, targetVar = "val") {
               rname = sprintf("R_mk_%s", el@name)
               if(!collector$hasRoutine(rname))
                  genXMLSchemaCCode(el, schema, collector = collector)
               
               sprintf("%s(%s)", rname, varName)
          })

setMethod("convertValueToR", "SimpleSequenceType",
          function(el, varName, schema, collector, targetVar = "val") {
              rname = sprintf("R_mk_%s_sequence", el@name)
               if(!collector$hasRoutine(rname)) {
                  routine = makeSequenceRoutine(el, varName, schema, collector, rname)
                  collector$addRoutine(routine, rname)
              }
              sprintf("%s(%s)", rname, varName)  # ?? use node or cur?
          })

setMethod("convertValueToR", "UnionDefinition",
          function(el, varName, schema, collector, targetVar = "val") {
              ans = sapply(el@slotTypes, convertValueToR, varName, schema, collector)
              
              sprintf('if(strcmp(%s->name, "%s") == 0)\n   %s = %s;', varName, sapply(el@slotTypes, slot, "name"), targetVar, ans)
          })

setMethod("convertValueToR", "AttributeDef",
          function(el, varName, schema, collector, targetVar = "val") {
              varName = "node"
              sprintf("xmlGetAttr(%s, \"%s\")", varName, el@name)
          })

setMethod("convertValueToR", "SchemaDateTimeType",
          function(el, varName, schema, collector, targetVar = "val") {
               sprintf("ScalarString(mkChar(xmlValue(%s)))", varName)  # Convert to POSIXct
           })

############################################################################

setGeneric("getDefaultValue", function(type, schema) standardGeneric("getDefaultValue"))

setMethod("getDefaultValue", "SchemaStringType",
           function(type, schema) {
               "ScalarString(R_NaString)"
           })

setMethod("getDefaultValue", "PrimitiveSchemaType",
           function(type, schema) {
               "ScalarInteger(R_NaInt)"
           })

setMethod("getDefaultValue", "ANY",
           function(type, schema) {
               "R_NilValue"
           })



#####################################################################

makeSequenceRoutine =
function(el, varName, schema, collector = genCollector(), rname = sprintf("R_mk_%s_sequence", elName))
{
    isUnion = is(el@elType, "UnionDefinition")
    isOneOfOurNodes = if(isUnion)
                         sprintf("strcmp(%s->name, \"%s\") == 0", varName, names(el@elType@slotTypes))
                       else
                         character()

    
    code = c(sprintf("while(%s) {", varName),
              if(length(isOneOfOurNodes))
                  sprintf("if(!(%s)) break;", paste(isOneOfOurNodes, collapse = " || ")),
               "ctr++;",
               "cur = cur->next;",
             "}")

    processNode = convertValueToR(el@elType, "cur", schema, collector, targetVar = "val")

    c("SEXP",
       sprintf("%s(xmlNodePtr node)", rname),
      "{",
      "int ctr = 0;",
      "xmlNodePtr cur = node;",
       code,
       "",
       "SEXP ans, val;",
       "PROTECT(ans = NEW_LIST(ctr));",
       "cur = node;",
       "for(int i = 0; i < ctr; i++) {",
        processNode,
        "SET_VECTOR_ELT(ans, i, val);",
        "cur = cur->next;",
        "}",
        "UNPROTECT(1);",
        "return(ans);",
      "}")
}
