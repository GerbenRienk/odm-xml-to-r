library(xml2)
library(dplyr)


x <- read_xml("odm1.3_clinical_ext_All_items_for_All_available_CRFs_(etc).xml")
xml_ns_strip(x)    # to get rid of namespaces

# SubjectData ----
bind_rows(lapply(xml_attrs(xml_find_all(x, "//SubjectData")), as.data.frame.list, 
                 stringsAsFactors=FALSE)) -> subjects

# ItemData ----
item_data_nodes <- xml_find_all(x, "//ItemData")

# fun to get all data as a character vector
get_item_data <- function (node) {
    node_attrs <- xml_attrs(node) 
    parent_attrs <- Reduce(c, xml_attrs(xml_parents(node))[5:1], right=TRUE)
    c(parent_attrs, node_attrs)
}

# to apply pevious function to each node and get list of character vectors
lapply(item_data_nodes, get_item_data) -> k

# to have all vectors as rows in a dataframe
bind_rows(lapply(k, as.data.frame.list, stringsAsFactors=FALSE)) %>%
    select(-MetaDataVersionOID, -SubjectKey, -Status, -Sex, -StartDate, -TransactionType) -> data


# clean
rm(k, item_data_nodes, get_item_data)

# Events ----
xml_attrs(xml_find_all(x, "//Study/MetaDataVersion/StudyEventDef")) -> k
bind_rows(lapply(k, as.data.frame.list, stringsAsFactors=FALSE)) -> events 
rm(k)

# Forms ----
xml_attrs(xml_find_all(x, "//Study/MetaDataVersion/FormDef")) -> k
bind_rows(lapply(k, as.data.frame.list, stringsAsFactors=FALSE)) -> forms 
rm(k)

# ItemGroups ----
xml_attrs(xml_find_all(x, "//Study/MetaDataVersion/ItemGroupDef")) -> k
bind_rows(lapply(k, as.data.frame.list, stringsAsFactors=FALSE)) -> groups 
rm(k)

# Items ----
xml_attrs(xml_find_all(x, "//Study/MetaDataVersion/ItemDef")) -> k
bind_rows(lapply(k, as.data.frame.list, stringsAsFactors=FALSE)) -> items 
rm(k)

# to get items with corresponding codelist ----
clr <- xml_find_all(x, "//CodeListRef")

# fun to get all data as a character vector
get_item_dic <- function (node) {
    node_attrs <- unlist(xml_attrs(node)) 
    parent_attrs <- unlist(xml_attrs(xml_parent(node)))
    c(parent_attrs, node_attrs)
}

# to apply pevious function to each node and get list of character vectors
lapply(clr, get_item_dic) -> k

# to have all vectors as rows in a dataframe
bind_rows(lapply(k, as.data.frame.list, stringsAsFactors=FALSE)) -> item_dics

# to add codelists to items dataframe
left_join(items, item_dics) -> items

# clean
rm(k, get_item_dic, clr, item_dics)


# CodeLists ---
cl <- xml_find_all(x, "//Study/MetaDataVersion/CodeList/CodeListItem/Decode/TranslatedText")

# function to get all relevant info in a character vector
get_dic <- function (node) {
    decode <- xml_text(node)
    names(decode) <- "Decode"
    other <- Reduce(c, xml_attrs(xml_parents(node)[2:3]))
    c(decode, other)
    
} 

# to apply pevious function to each node and get list of character vectors
lapply(cl, get_dic) -> k

# to have all vectors as rows in a dataframe
bind_rows(lapply(k, as.data.frame.list, stringsAsFactors=FALSE)) %>%
    select(OID:SASFormatName, CodedValue, Decode) -> items_codelists

# clean
rm(cl, get_dic, k)

# get LeftItemText (to use as var labels) ----
xml_find_all(x, "//OpenClinica:LeftItemText") -> lit_nodes

data.frame(ItemOID = character(),
           LeftItemText = character(),
           stringsAsFactors = FALSE) -> lit_df

for (i in lit_nodes) {
  # to get the left item text
  lit <- xml_text(i) 
  names(lit) <- "LeftItemText"
  
  # to get the item OID
  parent2 <- xml_attrs(xml_parent(xml_parent(i))) 
  
  # combine both in a vector and update lit_df
  res <- c(parent2, lit)
  lit_df <- bind_rows(lit_df, res)
}

# remove duplicates
unique(lit_df) -> lit_df

# merge with items
items %>%
  left_join(rename(lit_df, OID = ItemOID)) %>%
  select(OID:Comment, LeftItemText, everything()) -> items


# clean
rm(lit_nodes, lit_df, i, lit, parent2, res)
rm(x)

# save.image("oc.RData")
