
# Import custom user functions from lib
import_extract <- function(custom_file_path, studydef_file_path){

  if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){

    # ideally in future this will check column existence and types from metadata,
    # rather than from a cohort-extractor-generated dummy data

    data_studydef_dummy <- read_feather(studydef_file_path) %>%
      # because date types are not returned consistently by cohort extractor
      mutate(across(ends_with("_date"), ~ as.Date(.))) %>%
      mutate(patient_id = as.integer(patient_id))

    data_custom_dummy <- read_feather(custom_file_path)

    not_in_studydef <- names(data_custom_dummy)[!( names(data_custom_dummy) %in% names(data_studydef_dummy) )]
    not_in_custom  <- names(data_studydef_dummy)[!( names(data_studydef_dummy) %in% names(data_custom_dummy) )]


    if(length(not_in_custom)!=0) stop(
      paste(
        "These variables are in studydef but not in custom: ",
        paste(not_in_custom, collapse=", ")
      )
    )

    if(length(not_in_studydef)!=0) stop(
      paste(
        "These variables are in custom but not in studydef: ",
        paste(not_in_studydef, collapse=", ")
      )
    )

    # reorder columns
    data_studydef_dummy <- data_studydef_dummy[,names(data_custom_dummy)]

    unmatched_types <- cbind(
      map_chr(data_studydef_dummy, ~paste(class(.), collapse=", ")),
      map_chr(data_custom_dummy, ~paste(class(.), collapse=", "))
    )[ (map_chr(data_studydef_dummy, ~paste(class(.), collapse=", ")) != map_chr(data_custom_dummy, ~paste(class(.), collapse=", ")) ), ] %>%
      as.data.frame() %>% rownames_to_column()


    # if(nrow(unmatched_types)>0) stop(
    #   #unmatched_types
    #   "inconsistent typing in studydef : dummy dataset\n",
    #   apply(unmatched_types, 1, function(row) paste(paste(row, collapse=" : "), "\n"))
    # )

    data_extract <- data_custom_dummy
  } else {
    data_extract <- read_csv(studydef_file_path) %>%
      #because date types are not returned consistently by cohort extractor
      mutate(across(ends_with("_date"),  as.Date))
  }
  data_extract
}



roundmid_any <- function(x, to=1){
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}

ceiling_any <- function(x, to=1){
  # round to nearest 100 millionth to avoid floating point errors
  ceiling(plyr::round_any(x/to, 1/100000000))*to
}








vax_type_lookup = c(
  "BNT162b2"="pfizer",
  "ChAdOx1"="az",
  "mRNA-1273"="moderna",
  "BNT162b2/omicron"="pfizeromicron",
  "mRNA-1273/omicron"="modernaomicron",
  "BNT162b2/children"="pfizerchildren",
  "ChAdOx1/2"="az2",
  "Other"="other"
)

