const char *
xmlValue(xmlNodePtr node)
{
   xmlChar *tmp  = xmlNodeGetContent(node);
   return( (char *) tmp);
}

SEXP
xmlGetAttr(xmlNodePtr node, const char *name)
{
    xmlAttr *atts = node->properties;
    xmlChar *val = "";
    while(atts) {
	if(strcmp(atts->name, name) == 0) {
	   val = (atts->xmlChildrenNode && atts->xmlChildrenNode->content ? atts->xmlChildrenNode->content : "");
	   break;
	}
	atts = atts->next;
    }
    return(ScalarString(mkChar(val)));
}


typedef SEXP (*XMLRoutine)(xmlNodePtr node);

SEXP
R_convertNode(SEXP r_node, SEXP r_routine)
{

    xmlNodePtr n = (xmlNodePtr) R_ExternalPtrAddr(r_node);
    XMLRoutine r = (XMLRoutine) R_ExternalPtrAddr(r_routine);
    return ( r (n) );
}
