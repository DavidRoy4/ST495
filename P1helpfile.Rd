\name{MERP Error Propagation Data}
\alias{MERP}
\docType{data}
\title{
%%   ~~ MERP data ~~
}
\description{
 Data collected from MERP centers. Patients follow a decision tree in their cancer treatment process. Variables include information about treatments, success rates, radiation levels, steps of the process, etc. 
}
\usage{data("MERP")}
\format{
  A data frame with 743 observations on the following 47 variables.
  \describe{
    
    \item{\code{DEVID}}{Error's deviation ID}
    \item{\code{CATLEVEL}}{Number for hierarchy levels in error tree (Category = 1, Subcategory = 2, Attribute = 3)}
    \item{\code{DESCR_IF_MISC}}{Custom attributes or errors created by the user}
    \item{\code{DESCRIPTION}}{Text description of an attribute or error, provided by the user}
    \item{\code{DATE_OCCURRED}}{Actual date or best estimate for when error occurred}
    \item{\code{IDENTIFIED_BY}}{Name of person who put in the error}
    \item{\code{MODIFIED_BY}}{If an error was assigned to another user for correcting, the user it was routed to goes here. If not, remains the same as the identifier}
    \item{\code{AFFECTED_TREATMENT}}{User is prompted about whether the error affected their treatment (0 = No, 1 = Yes)}
    \item{\code{CORRECTED}}{User is prompted about whether the error was fixed or not (1 = Yes, 2 = No, 3 = N/A)}
    \item{\code{WHYNOTCORRECTED}}{If 2 is selected for CORRECTED, user provides details here}
    \item{\code{TX_INTENT}}{Treatment information (0 = N/A or Default option, 1 = Curative, 2 = Palliative)}
    \item{\code{TX_METHOD}}{Treatment method (0 = Default option or Conventional, 1 = EBRT, 2 = 3D-CRT, 3 = IMRT, 4 = SRS, 5 = SBRT)}
    \item{\code{DEV_TYPE}}{Error selected by user upon first selection screen (0 = Clinical, 1 = Radiation Safety, 2 = Quality Assurance)}
    \item{\code{ASSIGNED_USER}}{The user the corrective action is assigned to}
    \item{\code{ASSIGNED_ROLE}}{The role of the user performing the corrective action in ASSIGNED_USER}
    \item{\code{Cat_Desc}}{Error's category}
    \item{\code{Sub_Desc}}{Error's subcategory}
    \item{\code{Att_Desc}}{Error's attribute}
    \item{\code{Attribute}}{Error's attribute code, comes from first 5 numbers of Attribute_Stage variable}
    \item{\code{Stage1}}{Current stage of the error being processed (1-5), comes from Attribute_Stage variable}
    \item{\code{Stage2}}{Second stage of the current error being processed (1-5), if applicable, comes from Attribute_Stage variable}
    \item{\code{Approved_By1}}{First approver of the error}
    \item{\code{Approved_By2}}{Second approver of the error, if one exists}
    \item{\code{Role1}}{First role of the approver}
    \item{\code{Role2}}{Second role of the approver, if one exists}
    \item{\code{CUSTOM_CORRECTION}}{Correction entered by the user}
    \item{\code{SELECTED_CORRECTION}}{Selected correction from the user}
    \item{\code{DOC_TYPE}}{In rare cases where error reaches grave significance, a document is created with the name ROOTCAUSE or MISADMIN}
    \item{\code{MOREINFO}}{User-chosen attribute}
    \item{\code{GROUPID}}{Role of the user who entered the error}
    \item{\code{ACTION}}{Action taken (2 = review deviation, 3 = deviation requires correction, 4 = review disapproved)}
    \item{\code{STATUS}}{Use unknown, most observations set to 4, 2 = observation is mostly "deviation disapproved" or "deviation requires correction"}
    \item{\code{ROLE_NAME1}}{Role of the user written out}
    \item{\code{ROLE_NAME2}}{Second role of the user written out, if one exists}
    \item{\code{ROLE_CODES}}{User's role and associated letter (T = therapist, P = physicist, R = radiation safety officer)}
    \item{\code{Attribute_Stage}}{Contains Attribute code and up to two stage codes}
    \item{\code{SL}}{Severity level with SL1 being the lowers and SL5 being the highest}
    \item{\code{IS_POST}}{Denotes whether error occurred before radiation treatment (Pre-Tx) or after (Post-Tx)}
    \item{\code{ROLE_CODES}}{Role of the approver (T = Therapist, P = Physicist, etc.)}
    \item{\code{REG_VIOLATION}}{1 = violation of state radiation regulations, 0 = no violation}
    \item{\code{POSSIBLE_VIOLATION}}{1 = possible violation of state radiation regulations, 0 = no violation}
    \item{\code{CLIN_SIGNIFICANT}}{1 = error is "clinically significant", 0 = not clinically significant}
    \item{\code{SIGNIFICANT_ERROR}}{1 = error is "significant", 0 = not significant}
    \item{\code{MISADMIN}}{1 = error is a "misadministration", 0 = not a misadministration}
    \item{\code{RECORDABLE}}{1 = error is a "recordable event", 0 = not a recordable event}
    \item{\code{INTENT_METHOD_APPLICABLE}}{User-selected method of treatment from a provided list}
    \item{\code{CORRECTION_ROLES}}{Role of user who should normally be assigned to correct the error}
    
  }
}

