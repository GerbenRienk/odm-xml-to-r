# Reading data from OpenClinica exports in the following xml format:
# CDISC ODM XML 1.3 Clinical Data with OpenClinica extension

# Pending:
# get sites info (site name, investigator)!?


# Preparation -------------------------------------------
library(XML)
library(plyr)

# Parsing the xml file
xmlFile <- "odm1.3_full_(etc).xml"
doc = xmlParse(xmlFile, encoding="US_ASCII")



# defining alias for default namespace
default.ns =  c(ns=xmlNamespaces(doc)[[1]]$uri)
oc.ns       = c(oc=xmlNamespaces(doc)[[2]]$uri)

# GlobalVariables ----
GlobalVariables = as.list(sapply(xpathApply(doc, "//ns:Study/ns:GlobalVariables",namespaces=default.ns, fun=xmlChildren)[[1]], 
                        xmlValue))

# Measurement units ------
MeasurementUnit=ldply(xpathApply(doc, "//ns:MeasurementUnit",namespaces=default.ns, fun=xmlAncestors,xmlAttrs),
      data.frame, stringsAsFactors=FALSE)
MeasurementUnit = MeasurementUnit[,c("OID","OID.1","Name")]
names(MeasurementUnit) = c("StudyOID","OID","Name")

# StudyEventRef ----
StudyEventRef <- ldply (xpathApply(doc, "//ns:StudyEventRef",namespaces=default.ns, fun=xmlAncestors,xmlAttrs), 
            data.frame, stringsAsFactors=FALSE)
StudyEventRef = unique(StudyEventRef[,c("StudyEventOID", "OrderNumber", "Mandatory")])
names(StudyEventRef)[1] = "EventOID"

# StudyEventDef  ----
StudyEventDef  <- ldply (xpathApply(doc, "//ns:StudyEventDef",namespaces=default.ns, fun=xmlAncestors,xmlAttrs), 
            data.frame, stringsAsFactors=FALSE)
StudyEventDef  = StudyEventDef [,c("OID.2","Name.1","Repeating","Type")]
names(StudyEventDef)[1] = "EventOID"
names(StudyEventDef)[2] = "Name"

# FormRef ----
FormRef <- ldply (xpathApply(doc, "//ns:FormRef",namespaces=default.ns, fun=xmlAncestors,xmlAttrs), 
            data.frame, stringsAsFactors=FALSE)
FormRef = FormRef[,c("OID.2", "FormOID", "Mandatory")]
names(FormRef)[1] = "EventOID"

# FormDef ----
FormDef <- ldply (xpathApply(doc, "//ns:FormDef",namespaces=default.ns, fun=xmlAncestors,xmlAttrs), 
            data.frame, stringsAsFactors=FALSE)
FormDef = FormDef[,c("OID.2","Name.1","Repeating")]
names(FormDef)[1] = "FormOID"
names(FormDef)[2] = "Name"

# ItemGroupRef ----
FormDef <- ldply (xpathApply(doc, "//ns:ItemGroupRef",namespaces=default.ns, fun=xmlAncestors,xmlAttrs), 
                    data.frame, stringsAsFactors=FALSE)
 

# ItemGroupDef ----
ItemGroupDef <- ldply (xpathApply(doc, "//ns:ItemGroupDef",namespaces=default.ns, fun=xmlAncestors,xmlAttrs), 
                       data.frame, stringsAsFactors=FALSE)
ItemGroupDef = ItemGroupDef[,c("OID.2","Name.1","Repeating")]
names(ItemGroupDef)[1] = "ItemGroupOID"
names(ItemGroupDef)[2] = "Name"

# ItemRef ----
ItemRef <- ldply (xpathApply(doc, "//ns:ItemRef",namespaces=default.ns, fun=xmlAncestors,xmlAttrs), 
                       data.frame, stringsAsFactors=FALSE)
ItemRef = ItemRef[,c("OID.2","ItemOID","OrderNumber","Mandatory")]
names(ItemRef)[1] = "ItemGroupOID"

# ItemGroupRepeat ----
ItemGroupRepeat <- ldply (xpathApply(doc, "//oc:ItemGroupRepeat",namespaces=oc.ns, fun=xmlAncestors,xmlAttrs), 
                       data.frame, stringsAsFactors=FALSE)
ItemGroupRepeat = ItemGroupRepeat[,c("FormOID", "ItemGroupOID", "ShowGroup", "RepeatNumber", "RepeatMax")]

# ItemDef ----
ItemDef <- ldply (xpathApply(doc, "//ns:ItemDef",namespaces=default.ns, fun=xmlAncestors,xmlAttrs), 
                       data.frame, stringsAsFactors=FALSE)
ItemDef = ItemDef[,-c(1:9)]
names(ItemDef)[1] = "ItemOID"
names(ItemDef)[2] = "Name"

# CodeList ----
CodeList <- ldply (xpathApply(doc, "//ns:CodeList",namespaces=default.ns, fun=xmlAncestors,xmlAttrs), 
                       data.frame, stringsAsFactors=FALSE)
CodeList = CodeList[,c("OID.2","Name.1", "DataType", "SASFormatName")]
names(CodeList)[1] = "CodeListOID"
names(CodeList)[2] = "Name"

# CodeListRef -----
CodeListRef = ldply(xpathApply(doc, "//ns:CodeListRef",namespaces=default.ns,fun=xmlAncestors,xmlAttrs),
                    data.frame, stringsAsFactors=FALSE)
CodeListRef=CodeListRef[,c("FormOIDs","OID.2","CodeListOID")]
names(CodeListRef)[1:2] = c("FormOID","ItemOID")

# CodeListItem ----------
codes = ldply(xpathApply(doc, "//ns:CodeList/ns:CodeListItem/ns:Decode/ns:TranslatedText",namespaces=default.ns, fun=xmlAncestors,xmlAttrs),
              data.frame, stringsAsFactors=FALSE)
decodes = as.data.frame(sapply(xpathApply(doc, "//ns:CodeList/ns:CodeListItem/ns:Decode/ns:TranslatedText",namespaces=default.ns, fun=xmlValue),
                               unlist), stringsAsFactors=FALSE)
names(decodes) = "Decode"
CodeListItem = cbind(codes,decodes)
rm(codes,decodes)
CodeListItem = CodeListItem[,-(1:9)]
names(CodeListItem)[1] = "CodeListOID"
names(CodeListItem)[2] = "Name"

# ClinicalData ------

# SubjectData ------
SubjectData = as.data.frame(do.call("rbind",sapply(xpathApply(doc, "//ns:ClinicalData/ns:SubjectData",namespaces=default.ns,fun=xmlAttrs),
                                                   unclass)), stringsAsFactors=FALSE)

# ItemData -------
ItemData <- ldply (xpathApply(doc, "//ns:ItemData",namespaces=default.ns, fun=xmlAncestors,xmlAttrs), 
                    data.frame, stringsAsFactors=FALSE)
ItemData = ItemData[,-c(1:6,8)]
names(ItemData)[4] = "SubjectStatus"
# names(ItemData)[7] = "EventStatus"
# names(ItemData)[10] = "FormStatus"

# MeasurementUnitRef ----
MeasurementUnitRef <- ldply (xpathApply(doc, "//ns:ItemData//ns:MeasurementUnitRef",namespaces=default.ns, fun=xmlAncestors,xmlAttrs), 
                   data.frame, stringsAsFactors=FALSE)
MeasurementUnitRef = unique(MeasurementUnitRef[,c("ItemOID","MeasurementUnitOID")])


# Change encoding of character vectors if needed ----
Encoding(CodeListItem$Decode) <- "UTF-8"
Encoding(ItemData$Value) <- "UTF-8"
Encoding(ItemDef$Comment) <- "UTF-8"
Encoding(MeasurementUnit$Name) <- "UTF-8"

# delete objects defined for XML reading and saving OC db ------
rm(default.ns, oc.ns, doc, xmlFile)
rm(GlobalVariables)
save.image("OC.RData")
