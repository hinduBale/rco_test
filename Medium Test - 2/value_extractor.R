texts <-  list(paste("mtcars[32, 11]",
                     "#This is a comment!!",
                     "mtcars [[11]][32]",
                     "mtcars$carb[32]",
                     "yo <- 1",
                     "yo",
                     "mtcars[[c(11,32)]]",
                     ".subset2(mtcars, 11)[32]",
                     sep = "\n"))


pd <- parse_text(texts)

fpd_ <- flatten_leaves(pd)

comments_df <- fpd_[fpd_$parent < 0, ] 
fpd <- fpd_[fpd_$parent >= 0, ] 

exam_nodes <- get_roots(fpd)

###############################################################################################################################################################################################
#Functions for Dollar ($)
###############################################################################################################################################################################################

has_dollar_sign <- function(fpd, node_id)
{
  father <- node_id
  mother <- fpd[fpd$parent == node_id &is.na(fpd$next_lines) & is.na(fpd$next_spaces) & is.na(fpd$prev_spaces), ]$id
  test_dollar_df <- fpd[fpd$parent == father | fpd$parent == mother, ]
  dollar <- match("'$'", test_dollar_df$token)
  dollar_flag <- (!(is.na(dollar)))
  return (dollar_flag)
}

dollar_dataset <- function(fpd, node_id)
{
  father <- node_id
  mother <- fpd[fpd$parent == node_id &is.na(fpd$next_lines) & is.na(fpd$next_spaces) & is.na(fpd$prev_spaces), ]$id
  dollar_loc <- fpd[(fpd$parent == father | fpd$parent == mother) & fpd$token == "'$'", ]
  replace_df_dollar <- fpd[fpd$pos_id < dollar_loc$pos_id & (fpd$parent == father | fpd$parent == mother) & fpd$token == "SYMBOL", ]$text
  return (replace_df_dollar)
}

dollar_colnum <- function(fpd, node_id)
{
  father <- node_id
  mother <- fpd[fpd$parent == node_id &is.na(fpd$next_lines) & is.na(fpd$next_spaces) & is.na(fpd$prev_spaces), ]$id
  dollar_loc_colnum <- fpd[(fpd$parent == father | fpd$parent == mother) & fpd$token == "'$'", ]
  replace_colname_dollar <- fpd[fpd$pos_id > dollar_loc_colnum$pos_id & (fpd$parent == father | fpd$parent == mother) & fpd$token == "SYMBOL", ]$text
  return (which(colnames(mtcars) == replace_colname_dollar[1]))
}

dollar_rownum <- function(fpd, node_id)
{
  father <- node_id
  mother <- fpd[fpd$parent == node_id &is.na(fpd$next_lines) & is.na(fpd$next_spaces) & is.na(fpd$prev_spaces), ]$id
  replace_rowname_dollar <- fpd[(fpd$parent == father | fpd$parent == mother) & fpd$token == "NUM_CONST", ]$text
  return (as.numeric(replace_rowname_dollar))
}

###############################################################################################################################################################################################
#Functions for LBB
###############################################################################################################################################################################################

has_square_brackets <- function(fpd, node_id)
{
  father <- node_id
  mother <- fpd[fpd$parent == node_id &is.na(fpd$next_lines) & is.na(fpd$next_spaces) & is.na(fpd$prev_spaces), ]$id
  test_square <- fpd[fpd$parent == father | fpd$parent == mother, ]
  square <- match("LBB", test_square$token)
  square_flag <- (!(is.na(square)))
  return (square_flag)
}

square_dataset <- function(fpd, node_id)
{
  father <- node_id
  mother <- fpd[fpd$parent == node_id &is.na(fpd$next_lines) & is.na(fpd$next_spaces) & is.na(fpd$prev_spaces), ]$id
  square_loc <- fpd[(fpd$parent == father | fpd$parent == mother) & fpd$token == "LBB", ]
  replace_df_square <- fpd[fpd$pos_id < square_loc$pos_id & (fpd$parent == father | fpd$parent == mother) & fpd$token == "SYMBOL", ]$text
  return (replace_df_square)
}

square_colnum <- function(fpd, node_id)
{
  father <- node_id
  mother <- fpd[fpd$parent == node_id &is.na(fpd$next_lines) & is.na(fpd$next_spaces) & is.na(fpd$prev_spaces), ]$id
  square_loc_colnum <- fpd[(fpd$parent == father | fpd$parent == mother) & fpd$token == "LBB", ]
  replace_colnum_square <- fpd[fpd$pos_id > square_loc_colnum$pos_id & (fpd$parent == father | fpd$parent == mother) & fpd$token == "NUM_CONST", ]$text
  return (replace_colnum_square[1])
}

square_rownum <- function(fpd, node_id)
{
  father <- node_id
  mother <- fpd[fpd$parent == node_id &is.na(fpd$next_lines) & is.na(fpd$next_spaces) & is.na(fpd$prev_spaces), ]$id
  square_loc_colnum <- fpd[(fpd$parent == father | fpd$parent == mother) & fpd$token == "LBB", ]
  replace_colnum_square <- fpd[fpd$pos_id > square_loc_colnum$pos_id & (fpd$parent == father | fpd$parent == mother) & fpd$token == "NUM_CONST", ]$text
  return (replace_colnum_square[2])
}

###############################################################################################################################################################################################
#Function to check whether single square bracket exists
###############################################################################################################################################################################################

has_single_bracket <- function(fpd, node_id)
{
  father <- node_id
  mother <- fpd[fpd$parent == node_id &is.na(fpd$next_lines) & is.na(fpd$next_spaces) & is.na(fpd$prev_spaces), ]$id
  test_single_bracket <- fpd[fpd$parent == father, ]
  single_bracket <- match("'['", test_single_bracket$token)
  single_bracket_flag <- (!(is.na(single_bracket)))
  single_bracket_flag1 <- TRUE
  if(length(mother) > 0L)
    single_bracket_flag1 <- FALSE
  return (single_bracket_flag & single_bracket_flag1)
}

single_dataset <- function(fpd, node_id)
{
  single_loc <- fpd[fpd$parent == node_id & fpd$token == "'['", ]
  replace_df_single <- fpd[fpd$pos_id < single_loc$pos_id & fpd$parent == node_id & fpd$token == "SYMBOL", ]$text
  return (replace_df_single)
}

single_colnum <- function(fpd, node_id)
{
  single_loc_colnum <- fpd[fpd$parent == node_id & fpd$token == "'['", ]
  replace_colnum_dollar <- fpd[fpd$pos_id > single_loc_colnum$pos_id & fpd$parent == node_id & fpd$token == "NUM_CONST", ]$text
  return (replace_colnum_dollar[2])
}

single_rownum <- function(fpd, node_id)
{
  single_loc_colnum <- fpd[fpd$parent == node_id & fpd$token == "'['", ]
  replace_colnum_dollar <- fpd[fpd$pos_id > single_loc_colnum$pos_id & fpd$parent == node_id & fpd$token == "NUM_CONST", ]$text
  return (replace_colnum_dollar[1])
}


##############################################################################################################################################################################################
#Examining the nodes from exam_nodes to make sure that it is indeed for column selection and then put it to to_edit fpd
#############################################################################################################################################################################################

i <- 1
final_exam_nodes <- NULL
data_frame <- NULL
column_name <- NULL
row_name <- NULL
not_to_edit <- NULL
for(i in seq_len(nrow(exam_nodes)))
{
  test_id <- exam_nodes[i, ]$id
  if(has_dollar_sign(fpd, test_id) == TRUE)
  {
    data_frame <- append(data_frame, dollar_dataset(fpd, test_id))
    column_name <- append(column_name, dollar_colnum(fpd, test_id))
    row_name <- append(row_name, dollar_rownum(fpd, test_id))
    final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[i, ])
  }
  else if(has_square_brackets(fpd, test_id) == TRUE)
  {
    data_frame <- append(data_frame, square_dataset(fpd, test_id))
    column_name <- append(column_name, square_colnum(fpd, test_id))
    row_name <- append(row_name, square_rownum(fpd, test_id))
    final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[i, ])
  }
  else if(has_single_bracket(fpd, test_id) == TRUE)
  {
    data_frame <- append(data_frame, single_dataset(fpd, test_id))
    column_name <- append(column_name, single_colnum(fpd, test_id))
    row_name <- append(row_name, single_rownum(fpd, test_id))
    final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[i, ])
  }
  else
  {
    not_to_edit <- rbind(not_to_edit, exam_nodes[i, ])
  }
}

############################################################################################################################################################################################
#Replacing the text of column selecting roots witht the most optimum .subset2 alternative
############################################################################################################################################################################################

j <- 1
for(j in seq_len(nrow(final_exam_nodes)))
{
  final_exam_nodes[j, ]$text <- sprintf(".subset2(%s, %s)[%s]", data_frame[j], column_name[j], row_name[j])
}

############################################################################################################################################################################################
#Curating the fpd that was not to be edited (from the exam_nodes) and including the comments
############################################################################################################################################################################################

not_to_edit <- rbind(not_to_edit, comments_df)
k <- 1
for(k in seq_len(nrow(not_to_edit)))
{
  father <- not_to_edit[k, ]$id
  mother <- fpd[fpd$parent == not_to_edit[k, ]$id &is.na(fpd$next_lines) & is.na(fpd$next_spaces) & is.na(fpd$prev_spaces), ]$id
  if(length(mother) > 0)
    not_to_edit <- rbind(not_to_edit, fpd[(fpd$parent == father | fpd$parent == mother), ])
  not_to_edit <- rbind(not_to_edit, fpd[fpd$parent == father, ])
}

not_to_edit <- not_to_edit[order(not_to_edit$pos_id), ]

##############################################################################################################################################################################################
#Transforming the final_exam_nodes array so that, it may become compatible with not_to_edit
##############################################################################################################################################################################################

itr <- 1
new_fpd <- NULL

for(itr in seq_len(nrow(final_exam_nodes)))
{
  act_fpd <- final_exam_nodes[itr, ]
  new_act_fpd <- flatten_leaves(parse_text(act_fpd$text))
  
  #Backing up the original new_act_fpd
  new_act_fpd_duplicate <- new_act_fpd
  
  #Setting new ids for the newly edited and parsed codes
  new_act_fpd$id <- paste0(act_fpd$id, "_", new_act_fpd$id)
  
  #Keeping old parents for new fpd
  new_act_fpd$parent[new_act_fpd$parent != 0] <- paste0(act_fpd$id, "_", new_act_fpd$parent[new_act_fpd$parent != 0])
  new_act_fpd$parent[new_act_fpd$parent == 0] <- act_fpd$parent
  
  #Calling a pre-wriiten rco::function....
  new_act_fpd$pos_id <- create_new_pos_id(act_fpd, nrow(new_act_fpd), act_fpd$id)
  
  #Fixing the next_spaces section of new_fpd
  new_act_fpd$next_spaces[nrow(new_act_fpd)] <- act_fpd$next_spaces
  
  #Fixing the next_lines section of new_fpd
  new_act_fpd$next_lines[nrow(new_act_fpd)] <- act_fpd$next_lines
  
  #Fixing the prev_spaces section of new_fpd
  new_act_fpd$prev_spaces[which(new_act_fpd$terminal)[[1]]] <- act_fpd$prev_spaces
  
  #Merging the new_fpd and the act_fpd(obtained upon iteration)
  new_fpd <- rbind(new_fpd, new_act_fpd)
  
  #Ordering the new_fpd according to the pos_id
  new_fpd <- new_fpd[order(new_fpd$pos_id), ]
}

############################################################################################################################################################################################
#Merging the new_fpd(derived from final_exam_nodes) with the not_to_edit data.frame into resultant_fpd
############################################################################################################################################################################################
resultant_fpd <- rbind(not_to_edit, new_fpd[order(new_fpd$pos_id), ])
resultant_fpd <- resultant_fpd[order(resultant_fpd$pos_id), ]

#Identifying the lines where the new line character has to be introduced.
next_lines <- NULL
next_lines <- resultant_fpd[resultant_fpd$parent == 0, ]
next_lines <- rbind(next_lines, resultant_fpd[resultant_fpd$parent < 0, ])
next_lines <- next_lines[order(next_lines$pos_id), ]

############################################################################################################################################################################################
#Results obtained in "optimized_text"
############################################################################################################################################################################################

target_id <- NULL
resultant_fpd[is.na(resultant_fpd)] <- 0 #Replacing the NAs with zeroes.
l <- 1
for(l in seq_len(nrow(next_lines)-1))
{
  target_id <- append(target_id, max(resultant_fpd[resultant_fpd$pos_id >= next_lines[l, ]$pos_id & resultant_fpd$pos_id < next_lines[l+1, ]$pos_id, ]$pos_id))
}

target_id <- append(target_id, resultant_fpd[nrow(resultant_fpd), ]$pos_id, )

m <- 1
for(m in seq_len(length(target_id)))
{
  resultant_fpd[which(resultant_fpd$pos_id == target_id[m]), ]$next_lines <- 1
}

resultant_fpd <-resultant_fpd[!duplicated(resultant_fpd), ]



optimized_text <- deparse_data(resultant_fpd)
optimized_text
