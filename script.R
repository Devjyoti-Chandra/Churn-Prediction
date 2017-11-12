

na=colSums(is.na(train))/nrow(train)
na=as.data.frame(na)

# class(train$NO_OF_Accs)
# class(train$HNW_CATEGORY)

train$HNW_CATEGORY = as.factor(train$HNW_CATEGORY)
#unique(train$HNW_CATEGORY)

#class(train$vintage)
#range(train$vintage)
#mean(train$vintage)
#sd(train$vintage)

#class(train$EMAIL_UNSUBSCRIBE)
#unique(train$EMAIL_UNSUBSCRIBE)
train$EMAIL_UNSUBSCRIBE = ifelse(is.na(train$EMAIL_UNSUBSCRIBE), 0, 1)

#unique(train$OCCUP_ALL_NEW)
# NA -> MISSING
train$OCCUP_ALL_NEW = ifelse(is.na(train$OCCUP_ALL_NEW), "MISSING", train$OCCUP_ALL_NEW)
#unique(train$OCCUP_ALL_NEW)
#table(train$OCCUP_ALL_NEW)
train$OCCUP_ALL_NEW = as.factor(paste(train$OCCUP_ALL_NEW))

train$dependents = ifelse(is.na(train$dependents), 0, train$dependents)
train$dependents = ifelse(train$dependents<1, "0", ifelse(train$dependents<3, "1-2", ">3"))
train$dependents = as.factor(train$dependents)

# Not considering city if taking zip
train$zip = substr(train$zip, 0, 1)
train$zip = as.factor(paste(train$zip))

train$FINAL_WORTH_prev1 = as.factor(train$FINAL_WORTH_prev1)
train$ENGAGEMENT_TAG_prev1 = as.factor(train$ENGAGEMENT_TAG_prev1)

# ATM_D_prev1 has only 0's.(15)
# train$MB_C_prev1, train$MB_D_prev1, train$POS_C_prev1 has only 0's.(20-22)
# train$COUNT_ATM_C_prev1 (26), train$COUNT_MB_C_prev1 (32) , train$COUNT_POS_C_prev1 (34) has only 0's.

# Replacing NA -> 0
train$custinit_CR_amt_prev1 = ifelse(is.na(train$custinit_CR_amt_prev1), 0, train$custinit_CR_amt_prev1)
train$custinit_DR_amt_prev1 = ifelse(is.na(train$custinit_DR_amt_prev1), 0, train$custinit_DR_amt_prev1)
train$custinit_CR_cnt_prev1 = ifelse(is.na(train$custinit_CR_cnt_prev1), 0, train$custinit_CR_cnt_prev1)
train$custinit_DR_cnt_prev1 = ifelse(is.na(train$custinit_DR_cnt_prev1), 0, train$custinit_DR_cnt_prev1)
train$ATM_amt_prev1 = ifelse(is.na(train$ATM_amt_prev1), 0, train$ATM_amt_prev1)
train$ATM_CW_Amt_prev1 = ifelse(is.na(train$ATM_CW_Amt_prev1), 0, train$ATM_CW_Amt_prev1)
train$ATM_CW_Cnt_prev1 = ifelse(is.na(train$ATM_CW_Cnt_prev1), 0, train$ATM_CW_Cnt_prev1)
train$BRN_CW_Amt_prev1 = ifelse(is.na(train$BRN_CW_Amt_prev1), 0, train$BRN_CW_Amt_prev1)
train$BRN_CW_Cnt_prev1 = ifelse(is.na(train$BRN_CW_Cnt_prev1), 0, train$BRN_CW_Cnt_prev1)
train$BRN_CASH_Dep_Amt_prev1 = ifelse(is.na(train$BRN_CASH_Dep_Amt_prev1), 0, train$BRN_CASH_Dep_Amt_prev1)
train$BRN_CASH_Dep_Cnt_prev1 = ifelse(is.na(train$BRN_CASH_Dep_Cnt_prev1), 0, train$BRN_CASH_Dep_Cnt_prev1)


# Replacing NA -> 0
train$custinit_CR_amt_prev2 = ifelse(is.na(train$custinit_CR_amt_prev2), 0, train$custinit_CR_amt_prev2)
train$custinit_DR_amt_prev2 = ifelse(is.na(train$custinit_DR_amt_prev2), 0, train$custinit_DR_amt_prev2)
train$custinit_CR_cnt_prev2 = ifelse(is.na(train$custinit_CR_cnt_prev2), 0, train$custinit_CR_cnt_prev2)
train$custinit_DR_cnt_prev2 = ifelse(is.na(train$custinit_DR_cnt_prev2), 0, train$custinit_DR_cnt_prev2)
train$ATM_amt_prev2 = ifelse(is.na(train$ATM_amt_prev2), 0, train$ATM_amt_prev2)
train$ATM_CW_Amt_prev2 = ifelse(is.na(train$ATM_CW_Amt_prev2), 0, train$ATM_CW_Amt_prev2)
train$ATM_CW_Cnt_prev2 = ifelse(is.na(train$ATM_CW_Cnt_prev2), 0, train$ATM_CW_Cnt_prev2)
train$BRN_CW_Amt_prev2 = ifelse(is.na(train$BRN_CW_Amt_prev2), 0, train$BRN_CW_Amt_prev2)
train$BRN_CW_Cnt_prev2 = ifelse(is.na(train$BRN_CW_Cnt_prev2), 0, train$BRN_CW_Cnt_prev2)
train$BRN_CASH_Dep_Amt_prev2 = ifelse(is.na(train$BRN_CASH_Dep_Amt_prev2), 0, train$BRN_CASH_Dep_Amt_prev2)
train$BRN_CASH_Dep_Cnt_prev2 = ifelse(is.na(train$BRN_CASH_Dep_Cnt_prev2), 0, train$BRN_CASH_Dep_Cnt_prev2)

# Replacing NA -> 0
train$custinit_CR_amt_prev3 = ifelse(is.na(train$custinit_CR_amt_prev3), 0, train$custinit_CR_amt_prev3)
train$custinit_DR_amt_prev3 = ifelse(is.na(train$custinit_DR_amt_prev3), 0, train$custinit_DR_amt_prev3)
train$custinit_CR_cnt_prev3 = ifelse(is.na(train$custinit_CR_cnt_prev3), 0, train$custinit_CR_cnt_prev3)
train$custinit_DR_cnt_prev3 = ifelse(is.na(train$custinit_DR_cnt_prev3), 0, train$custinit_DR_cnt_prev3)
train$ATM_amt_prev3 = ifelse(is.na(train$ATM_amt_prev3), 0, train$ATM_amt_prev3)
train$ATM_CW_Amt_prev3 = ifelse(is.na(train$ATM_CW_Amt_prev3), 0, train$ATM_CW_Amt_prev3)
train$ATM_CW_Cnt_prev3 = ifelse(is.na(train$ATM_CW_Cnt_prev3), 0, train$ATM_CW_Cnt_prev3)
train$BRN_CW_Amt_prev3 = ifelse(is.na(train$BRN_CW_Amt_prev3), 0, train$BRN_CW_Amt_prev3)
train$BRN_CW_Cnt_prev3 = ifelse(is.na(train$BRN_CW_Cnt_prev3), 0, train$BRN_CW_Cnt_prev3)
train$BRN_CASH_Dep_Amt_prev3 = ifelse(is.na(train$BRN_CASH_Dep_Amt_prev3), 0, train$BRN_CASH_Dep_Amt_prev3)
train$BRN_CASH_Dep_Cnt_prev3 = ifelse(is.na(train$BRN_CASH_Dep_Cnt_prev3), 0, train$BRN_CASH_Dep_Cnt_prev3)


# Replacing NA -> 0
train$custinit_CR_amt_prev4 = ifelse(is.na(train$custinit_CR_amt_prev4), 0, train$custinit_CR_amt_prev4)
train$custinit_DR_amt_prev4 = ifelse(is.na(train$custinit_DR_amt_prev4), 0, train$custinit_DR_amt_prev4)
train$custinit_CR_cnt_prev4 = ifelse(is.na(train$custinit_CR_cnt_prev4), 0, train$custinit_CR_cnt_prev4)
train$custinit_DR_cnt_prev4 = ifelse(is.na(train$custinit_DR_cnt_prev4), 0, train$custinit_DR_cnt_prev4)
train$ATM_amt_prev4 = ifelse(is.na(train$ATM_amt_prev4), 0, train$ATM_amt_prev4)
train$ATM_CW_Amt_prev4 = ifelse(is.na(train$ATM_CW_Amt_prev4), 0, train$ATM_CW_Amt_prev4)
train$ATM_CW_Cnt_prev4 = ifelse(is.na(train$ATM_CW_Cnt_prev4), 0, train$ATM_CW_Cnt_prev4)
train$BRN_CW_Amt_prev4 = ifelse(is.na(train$BRN_CW_Amt_prev4), 0, train$BRN_CW_Amt_prev4)
train$BRN_CW_Cnt_prev4 = ifelse(is.na(train$BRN_CW_Cnt_prev4), 0, train$BRN_CW_Cnt_prev4)
train$BRN_CASH_Dep_Amt_prev4 = ifelse(is.na(train$BRN_CASH_Dep_Amt_prev4), 0, train$BRN_CASH_Dep_Amt_prev4)
train$BRN_CASH_Dep_Cnt_prev4 = ifelse(is.na(train$BRN_CASH_Dep_Cnt_prev4), 0, train$BRN_CASH_Dep_Cnt_prev4)


# Replacing NA -> 0
train$custinit_CR_amt_prev5 = ifelse(is.na(train$custinit_CR_amt_prev5), 0, train$custinit_CR_amt_prev5)
train$custinit_DR_amt_prev5 = ifelse(is.na(train$custinit_DR_amt_prev5), 0, train$custinit_DR_amt_prev5)
train$custinit_CR_cnt_prev5 = ifelse(is.na(train$custinit_CR_cnt_prev5), 0, train$custinit_CR_cnt_prev5)
train$custinit_DR_cnt_prev5 = ifelse(is.na(train$custinit_DR_cnt_prev5), 0, train$custinit_DR_cnt_prev5)
train$ATM_amt_prev5 = ifelse(is.na(train$ATM_amt_prev5), 0, train$ATM_amt_prev5)
train$ATM_CW_Amt_prev5 = ifelse(is.na(train$ATM_CW_Amt_prev5), 0, train$ATM_CW_Amt_prev5)
train$ATM_CW_Cnt_prev5 = ifelse(is.na(train$ATM_CW_Cnt_prev5), 0, train$ATM_CW_Cnt_prev5)
train$BRN_CW_Amt_prev5 = ifelse(is.na(train$BRN_CW_Amt_prev5), 0, train$BRN_CW_Amt_prev5)
train$BRN_CW_Cnt_prev5 = ifelse(is.na(train$BRN_CW_Cnt_prev5), 0, train$BRN_CW_Cnt_prev5)
train$BRN_CASH_Dep_Amt_prev5 = ifelse(is.na(train$BRN_CASH_Dep_Amt_prev5), 0, train$BRN_CASH_Dep_Amt_prev5)
train$BRN_CASH_Dep_Cnt_prev5 = ifelse(is.na(train$BRN_CASH_Dep_Cnt_prev5), 0, train$BRN_CASH_Dep_Cnt_prev5)


# Replacing NA -> 0
train$custinit_CR_amt_prev6 = ifelse(is.na(train$custinit_CR_amt_prev6), 0, train$custinit_CR_amt_prev6)
train$custinit_DR_amt_prev6 = ifelse(is.na(train$custinit_DR_amt_prev6), 0, train$custinit_DR_amt_prev6)
train$custinit_CR_cnt_prev6 = ifelse(is.na(train$custinit_CR_cnt_prev6), 0, train$custinit_CR_cnt_prev6)
train$custinit_DR_cnt_prev6 = ifelse(is.na(train$custinit_DR_cnt_prev6), 0, train$custinit_DR_cnt_prev6)
train$ATM_amt_prev6 = ifelse(is.na(train$ATM_amt_prev6), 0, train$ATM_amt_prev6)
train$ATM_CW_Amt_prev6 = ifelse(is.na(train$ATM_CW_Amt_prev6), 0, train$ATM_CW_Amt_prev6)
train$ATM_CW_Cnt_prev6 = ifelse(is.na(train$ATM_CW_Cnt_prev6), 0, train$ATM_CW_Cnt_prev6)
train$BRN_CW_Amt_prev6 = ifelse(is.na(train$BRN_CW_Amt_prev6), 0, train$BRN_CW_Amt_prev6)
train$BRN_CW_Cnt_prev6 = ifelse(is.na(train$BRN_CW_Cnt_prev6), 0, train$BRN_CW_Cnt_prev6)
train$BRN_CASH_Dep_Amt_prev6 = ifelse(is.na(train$BRN_CASH_Dep_Amt_prev6), 0, train$BRN_CASH_Dep_Amt_prev6)
train$BRN_CASH_Dep_Cnt_prev6 = ifelse(is.na(train$BRN_CASH_Dep_Cnt_prev6), 0, train$BRN_CASH_Dep_Cnt_prev6)

# train$FRX_PrevQ1 & train$FRX_PrevQ1_N comes out to be same. Seems to be insignificant. Only 61 non-na values.


train$EFT_SELF_TRANSFER_PrevQ1 = as.factor(train$EFT_SELF_TRANSFER_PrevQ1)

# Replacing NA with 0 (no plan)
train$Billpay_Active_PrevQ1 = ifelse(is.na(train$Billpay_Active_PrevQ1), 0, train$Billpay_Active_PrevQ1)
train$Billpay_Reg_ason_Prev1 = ifelse(is.na(train$Billpay_Reg_ason_Prev1), 0, train$Billpay_Reg_ason_Prev1)

train$NO_OF_FD_BOOK_PrevQ1 = ifelse(is.na(train$NO_OF_FD_BOOK_PrevQ1), 0, train$NO_OF_FD_BOOK_PrevQ1)
train$NO_OF_FD_BOOK_PrevQ2 = ifelse(is.na(train$NO_OF_FD_BOOK_PrevQ2), 0, train$NO_OF_FD_BOOK_PrevQ2)
train$NO_OF_RD_BOOK_PrevQ1 = ifelse(is.na(train$NO_OF_RD_BOOK_PrevQ1), 0, train$NO_OF_RD_BOOK_PrevQ1)
train$NO_OF_RD_BOOK_PrevQ2 = ifelse(is.na(train$NO_OF_RD_BOOK_PrevQ2), 0, train$NO_OF_RD_BOOK_PrevQ2)
train$count_No_of_MF_PrevQ1 = ifelse(is.na(train$count_No_of_MF_PrevQ1), 0, train$count_No_of_MF_PrevQ1)
train$count_No_of_MF_PrevQ2 = ifelse(is.na(train$count_No_of_MF_PrevQ2), 0, train$count_No_of_MF_PrevQ2)




train$AGRI_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$AGRI_PREM_CLOSED_PREVQ1), 0, 1)
train$AL_CNC_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$AL_CNC_PREM_CLOSED_PREVQ1), 0, 1)
train$AL_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$AL_PREM_CLOSED_PREVQ1), 0, 1)
train$BL_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$BL_PREM_CLOSED_PREVQ1), 0, 1)
train$CC_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$CC_PREM_CLOSED_PREVQ1), 0, 1) #
train$CE_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$CE_PREM_CLOSED_PREVQ1), 0, 1)
train$CV_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$CV_PREM_CLOSED_PREVQ1), 0, 1)
train$EDU_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$EDU_PREM_CLOSED_PREVQ1), 0, 1) #
train$OTHER_LOANS_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$OTHER_LOANS_PREM_CLOSED_PREVQ1), 0, 1)
train$PL_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$PL_PREM_CLOSED_PREVQ1), 0, 1)
train$RD_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$RD_PREM_CLOSED_PREVQ1), 0, 1)
train$FD_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$FD_PREM_CLOSED_PREVQ1), 0, 1)
train$TL_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$TL_PREM_CLOSED_PREVQ1), 0, 1) #
train$TWL_PREM_CLOSED_PREVQ1 = ifelse(is.na(train$TWL_PREM_CLOSED_PREVQ1), 0, 1)

train$Prem_Closed_Tag_Count = rowSums(train[,264:277])



train$AGRI_Closed_PrevQ1 = ifelse(is.na(train$AGRI_Closed_PrevQ1), 0, 1)
train$AL_CNC_Closed_PrevQ1 = ifelse(is.na(train$AL_CNC_Closed_PrevQ1), 0, 1)
train$AL_Closed_PrevQ1 = ifelse(is.na(train$AL_Closed_PrevQ1), 0, 1)
train$BL_Closed_PrevQ1 = ifelse(is.na(train$BL_Closed_PrevQ1), 0, 1)
train$CC_CLOSED_PREVQ1 = ifelse(is.na(train$CC_CLOSED_PREVQ1), 0, 1)
train$CE_Closed_PrevQ1 = ifelse(is.na(train$CE_Closed_PrevQ1), 0, 1)
train$CV_Closed_PrevQ1 = ifelse(is.na(train$CV_Closed_PrevQ1), 0, 1)
train$EDU_Closed_PrevQ1 = ifelse(is.na(train$EDU_Closed_PrevQ1), 0, 1) #
train$GL_Closed_PrevQ1 = ifelse(is.na(train$GL_Closed_PrevQ1), 0, 1)
train$OTHER_LOANS_Closed_PrevQ1 = ifelse(is.na(train$OTHER_LOANS_Closed_PrevQ1), 0, 1)
train$PL_Closed_PrevQ1 = ifelse(is.na(train$PL_Closed_PrevQ1), 0, 1)
train$RD_CLOSED_PREVQ1 = ifelse(is.na(train$RD_CLOSED_PREVQ1), 0, 1)
train$FD_CLOSED_PREVQ1 = ifelse(is.na(train$FD_CLOSED_PREVQ1), 0, 1)
train$TL_Closed_PrevQ1 = ifelse(is.na(train$TL_Closed_PrevQ1), 0, 1)
train$TWL_Closed_PrevQ1 = ifelse(is.na(train$TWL_Closed_PrevQ1), 0, 1)
train$DEMAT_CLOSED_PREV1YR = ifelse(is.na(train$DEMAT_CLOSED_PREV1YR), 0, 1)
train$SEC_ACC_CLOSED_PREV1YR = ifelse(is.na(train$SEC_ACC_CLOSED_PREV1YR), 0, 1)

train$Closed_Tag_Count = rowSums(train[,278:294])

train$AGRI_TAG_LIVE = ifelse(is.na(train$AGRI_TAG_LIVE), 0, 1)
train$AL_CNC_TAG_LIVE = ifelse(is.na(train$AL_CNC_TAG_LIVE), 0, 1)
train$AL_TAG_LIVE = ifelse(is.na(train$AL_TAG_LIVE), 0, 1)
train$BL_TAG_LIVE = ifelse(is.na(train$BL_TAG_LIVE), 0, 1)
train$CC_TAG_LIVE = ifelse(is.na(train$CC_TAG_LIVE), 0, 1)
train$CE_TAG_LIVE = ifelse(is.na(train$CE_TAG_LIVE), 0, 1)
train$CV_TAG_LIVE = ifelse(is.na(train$CV_TAG_LIVE), 0, 1)
train$DEMAT_TAG_LIVE = ifelse(is.na(train$DEMAT_TAG_LIVE), 0, 1)
train$EDU_TAG_LIVE = ifelse(is.na(train$EDU_TAG_LIVE), 0, 1)
train$GL_TAG_LIVE = ifelse(is.na(train$GL_TAG_LIVE), 0, 1)
train$HL_TAG_LIVE = ifelse(is.na(train$HL_TAG_LIVE), 0, 1)
train$SEC_ACC_TAG_LIVE = ifelse(is.na(train$SEC_ACC_TAG_LIVE), 0, 1)
train$INS_TAG_LIVE = ifelse(is.na(train$INS_TAG_LIVE), 0, 1)
train$LAS_TAG_LIVE = ifelse(is.na(train$LAS_TAG_LIVE), 0, 1)
train$MF_TAG_LIVE = ifelse(is.na(train$MF_TAG_LIVE), 0, 1)
train$OTHER_LOANS_TAG_LIVE = ifelse(is.na(train$OTHER_LOANS_TAG_LIVE), 0, 1)
train$PL_TAG_LIVE = ifelse(is.na(train$PL_TAG_LIVE), 0, 1)
train$RD_TAG_LIVE = ifelse(is.na(train$RD_TAG_LIVE), 0, 1)
train$FD_TAG_LIVE = ifelse(is.na(train$FD_TAG_LIVE), 0, 1)
train$TL_TAG_LIVE = ifelse(is.na(train$TL_TAG_LIVE), 0, 1)
train$TWL_TAG_LIVE = ifelse(is.na(train$TWL_TAG_LIVE), 0, 1)
train$lap_tag_live = ifelse(is.na(train$lap_tag_live), 0, 1)

train$Live_Tag_Count = rowSums(train[,295:316])

train$Charges_cnt_PrevQ1 = ifelse(is.na(train$Charges_cnt_PrevQ1), 0, train$Charges_cnt_PrevQ1)

train$NO_OF_COMPLAINTS = ifelse(is.na(train$NO_OF_COMPLAINTS), 0, train$NO_OF_COMPLAINTS)

train$NO_OF_CHEQUE_BOUNCE_V1 = ifelse(is.na(train$NO_OF_CHEQUE_BOUNCE_V1), 0, train$NO_OF_COMPLAINTS)

train$AGRI_PREM_CLOSED_PREVQ1 = NULL
train$AL_CNC_PREM_CLOSED_PREVQ1 = NULL
train$AL_PREM_CLOSED_PREVQ1 = NULL
train$BL_PREM_CLOSED_PREVQ1 = NULL
train$CC_PREM_CLOSED_PREVQ1 = NULL
train$CV_PREM_CLOSED_PREVQ1 = NULL
train$OTHER_LOANS_PREM_CLOSED_PREVQ1 = NULL
train$PL_PREM_CLOSED_PREVQ1 = NULL
train$RD_PREM_CLOSED_PREVQ1 = NULL
train$FD_PREM_CLOSED_PREVQ1 = NULL
train$TL_PREM_CLOSED_PREVQ1 = NULL
train$TWL_PREM_CLOSED_PREVQ1 = NULL
train$CE_PREM_CLOSED_PREVQ1 = NULL
train$EDU_PREM_CLOSED_PREVQ1 = NULL

train$AGRI_Closed_PrevQ1 = NULL
train$AL_CNC_Closed_PrevQ1 = NULL
train$AL_Closed_PrevQ1 = NULL
train$BL_Closed_PrevQ1 = NULL
train$CC_CLOSED_PREVQ1 = NULL
train$CE_Closed_PrevQ1 = NULL
train$CV_Closed_PrevQ1 = NULL
train$EDU_Closed_PrevQ1 = NULL
train$GL_Closed_PrevQ1 = NULL
train$OTHER_LOANS_Closed_PrevQ1 = NULL
train$PL_Closed_PrevQ1 = NULL
train$RD_CLOSED_PREVQ1 = NULL
train$FD_CLOSED_PREVQ1 = NULL
train$TL_Closed_PrevQ1 = NULL
train$TWL_Closed_PrevQ1 = NULL
train$DEMAT_CLOSED_PREV1YR = NULL
train$SEC_ACC_CLOSED_PREV1YR = NULL

train$AGRI_TAG_LIVE = NULL
train$AL_CNC_TAG_LIVE = NULL
train$AL_TAG_LIVE = NULL
train$BL_TAG_LIVE = NULL
train$CC_TAG_LIVE = NULL
train$CE_TAG_LIVE = NULL
train$CV_TAG_LIVE = NULL
train$DEMAT_TAG_LIVE = NULL
train$EDU_TAG_LIVE = NULL
train$GL_TAG_LIVE = NULL
train$HL_TAG_LIVE = NULL
train$SEC_ACC_TAG_LIVE = NULL
train$INS_TAG_LIVE = NULL
train$LAS_TAG_LIVE = NULL
train$MF_TAG_LIVE = NULL
train$OTHER_LOANS_TAG_LIVE = NULL
train$PL_TAG_LIVE = NULL
train$RD_TAG_LIVE = NULL
train$FD_TAG_LIVE = NULL
train$TL_TAG_LIVE = NULL
train$TWL_TAG_LIVE = NULL
train$lap_tag_live = NULL

train$FRX_PrevQ1 = NULL
train$FRX_PrevQ1_N = NULL
train$AL_DATE=NULL
train$BL_DATE=NULL
train$CE_DATE=NULL
train$CV_DATE=NULL
train$GL_DATE=NULL
train$PL_DATE=NULL
train$TL_DATE=NULL
train$EDU_DATE=NULL
train$LAP_DATE=NULL
train$LAS_DATE=NULL
train$TWL_DATE=NULL
train$AGRI_DATE=NULL
train$AL_CNC_DATE=NULL
train$OTHER_LOANS_DATE=NULL
train$Percent_Change_in_Big_Expenses=NULL
train$Percent_Change_in_Self_Txn=NULL
train$Percent_Change_in_FT_outside=NULL
train$Percent_Change_in_FT_Bank=NULL
train$Percent_Change_in_Credits=NULL
train$Complaint_Resolved_PrevQ1=NULL
train$Complaint_Logged_PrevQ1=NULL
train$Query_Resolved_PrevQ1=NULL
train$Query_Logged_PrevQ1=NULL
train$Req_Resolved_PrevQ1=NULL
train$Req_Logged_PrevQ1=NULL
train$Recency_of_Activity=NULL
train$Recency_of_ATM_TXN=NULL
train$Recency_of_BRANCH_TXN=NULL
train$Recency_of_CR_TXN=NULL
train$Recency_of_DR_TXN=NULL
train$Recency_of_IB_TXN=NULL
train$Recency_of_MB_TXN=NULL
train$Recency_of_POS_TXN=NULL
train$city=NULL
train$brn_code = NULL

train$OCCUP_ALL_NEW = as.factor(paste(train$OCCUP_ALL_NEW))
train$BRN_CW_Amt_prev5 = as.numeric(train$BRN_CW_Amt_prev5)
train$BRN_CW_Cnt_prev5 = as.numeric(train$BRN_CW_Cnt_prev5)
train$Billpay_Active_PrevQ1_N = as.factor(train$Billpay_Active_PrevQ1_N)
train$Billpay_Reg_ason_Prev1_N = as.factor(train$Billpay_Reg_ason_Prev1_N)
train$Charges_cnt_PrevQ1_N = as.factor(train$Charges_cnt_PrevQ1_N)
train$RBI_Class_Audit = as.factor(train$RBI_Class_Audit)
train$gender_bin = as.factor(train$gender_bin)

train$Responders = as.factor(train$Responders)

#f= HNW_CATEGORY, OCCUP_ALL_NEW, dependents, zip, EFT_SELF_TRANSFER_PrevQ1, 
 #  Billpay_Active_PrevQ1_N, Billpay_Reg_ason_Prev1_N, Charges_cnt_PrevQ1_N, RBI_Class_Audit, gender_bin

train_new <- dummy.data.frame(train, names = c("HNW_CATEGORY","OCCUP_ALL_NEW",
                                                   "dependents","zip",
                                                   "EFT_SELF_TRANSFER_PrevQ1","Billpay_Active_PrevQ1_N", "Billpay_Reg_ason_Prev1_N",
                                                   "Charges_cnt_PrevQ1_N", "RBI_Class_Audit", "gender_bin"))
train_new <- dummy.data.frame(train, names = "HNW_CATEGORY")

train_new = train[,-c("HNW_CATEGORY","OCCUP_ALL_NEW", "dependents", "zip", "EFT_SELF_TRANSFER_PrevQ1","Billpay_Active_PrevQ1_N", "Billpay_Reg_ason_Prev1_N", "Charges_cnt_PrevQ1_N", "RBI_Class_Audit", "gender_bin")]
