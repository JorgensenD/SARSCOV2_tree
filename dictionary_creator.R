## plot dictionary
# uses  package countrycode
require(countrycode)
require(dplyr)

countrycode_edit <- function (sourcevar, origin, destination, warn = TRUE, nomatch = NA, 
          custom_dict = NULL, custom_match = NULL, origin_regex = FALSE) 
{
  if (is.null(custom_dict)) {
    if (origin == "country.name") {
      origin <- "country.name.en"
    }
    if (destination == "country.name") {
      destination <- "country.name.en"
    }
    if (origin %in% c("country.name.en", "country.name.de")) {
      origin <- paste0(origin, ".regex")
      origin_regex <- TRUE
    }
    else {
      origin_regex <- FALSE
    }
  }
  if (!is.null(custom_dict)) {
    dictionary <- custom_dict
    valid_origin <- colnames(dictionary)
    valid_destination <- colnames(dictionary)
  }
  else {
    dictionary = countrycode::codelist
    valid_origin = c("country.name", "country.name.de", 
                     "cowc", "cown", "ecb", "eurostat", "fao", "fips", 
                     "gaul", "genc2c", "genc3c", "genc3n", "gwc", "gwn", 
                     "imf", "ioc", "iso2c", "iso3c", "iso3n", "p4c", 
                     "p4n", "un", "un_m49", "unpd", "vdem", "wb", "wb_api2c", 
                     "wb_api3c", "wvs", "country.name.en.regex", "country.name.de.regex")
    valid_destination <- colnames(dictionary)
  }
  if ("tbl_df" %in% class(dictionary)) {
    dictionary <- as.data.frame(dictionary)
  }
  if (missing(sourcevar)) {
    stop("sourcevar is NULL (does not exist).")
  }
  if (!mode(sourcevar) %in% c("character", "numeric")) {
    stop("sourcevar must be a character or numeric vector. This error often\n             arises when users pass a tibble (e.g., from dplyr) instead of a\n             column vector from a data.frame (i.e., my_tbl[, 2] vs. my_df[, 2]\n                                              vs. my_tbl[[2]])")
  }
  if (!is.null(nomatch) & (length(nomatch) != 1) & (length(nomatch) != 
                                                    length(sourcevar))) {
    stop("nomatch needs to be NULL, or of length 1 or ", 
         length(sourcevar), ".")
  }
  if (!origin %in% valid_origin) {
    stop("Origin code not supported by countrycode or present in the user-supplied custom_dict.")
  }
  if (!destination %in% valid_destination) {
    stop("Destination code not supported by countrycode or present in the user-supplied custom_dict.")
  }
  if (class(dictionary) != "data.frame") {
    stop("Dictionary must be a data frame or tibble with codes as columns.")
  }
  if (!destination %in% colnames(dictionary)) {
    stop("Destination code must correpond to a column name in the dictionary data frame.")
  }
  dups = any(duplicated(stats::na.omit(dictionary[, origin])))
  if (dups) {
    stop("Countrycode cannot accept dictionaries with duplicated origin codes")
  }
  origin_vector <- sourcevar
  if (is.null(custom_dict)) {
    if ((class(origin_vector) == "character") & !grepl("country", 
                                                       origin)) {
      origin_vector = toupper(origin_vector)
    }
  }
  if (origin_regex) {
    dict <- stats::na.omit(dictionary[, c(origin, destination)])
    sourcefctr <- factor(origin_vector)
    matches <- sapply(c(levels(sourcefctr), NA), function(x) {
      matchidx <- sapply(dict[[origin]], function(y) grepl(y, 
                                                           x, perl = TRUE, ignore.case = TRUE))
      dict[matchidx, destination]
    })
    matches[sapply(matches, length) == 0] <- `class<-`(NA, 
                                                       class(dict[[destination]]))
    destination_list <- matches[sapply(matches, length) > 
                                  1]
    destination_list <- Map(c, names(destination_list), 
                            destination_list)
    matches[sapply(matches, length) > 1] <- `class<-`(NA, 
                                                      class(dict[[destination]]))
    matches <- sapply(matches, function(x) {
      x[length(x)]
    })
    if (!is.null(custom_match)) {
      matchidxs <- match(names(matches), names(custom_match))
      cust_matched <- !is.na(matchidxs)
      matches[cust_matched] <- custom_match[matchidxs][cust_matched]
    }
    destination_vector <- unname(matches[as.numeric(sourcefctr)])
  }
  else {
    dict <- stats::na.omit(dictionary[, c(origin, destination)])
    sourcefctr <- factor(origin_vector)
    matchidxs <- match(levels(sourcefctr), dict[[origin]])
    matches <- dict[[destination]][matchidxs]
    if (!is.null(custom_match)) {
      matchidxs <- match(levels(sourcefctr), names(custom_match))
      cust_matched <- !is.na(matchidxs)
      matches[cust_matched] <- custom_match[matchidxs][cust_matched]
    }
    destination_vector <- matches[as.numeric(sourcefctr)]
  }
  sane_sourcevar <- class(sourcevar)[1] == class(destination_vector)[1]
  sane_nomatch <- class(nomatch)[1] == class(destination_vector)[1]
  idx <- is.na(destination_vector)
  if (is.null(nomatch)) {
    if (sane_sourcevar) {
      destination_vector[idx] <- sourcevar[idx]
    }
    else if (class(sourcevar)[1] == "factor" & class(destination_vector)[1] == 
             "character") {
      destination_vector[idx] <- as.character(sourcevar[idx])
    }
    else {
      warning("The origin and destination codes are not of the same\n                    class. Filling-in bad matches with NA instead.")
    }
  }
  else if ((length(nomatch) == 1) & is.na(nomatch)) {
  }
  else if ((length(nomatch) == 1) & sane_nomatch) {
    destination_vector[idx] <- nomatch
  }
  else if ((length(nomatch) == length(sourcevar)) & sane_sourcevar) {
    destination_vector[idx] <- nomatch[idx]
  }
  else {
    warning("The argument `nomatch` must be NULL, NA, or of the same class\n                as the destination vector. Filling-in bad matches with NA instead.")
  }
  if (warn) {
    badmatch <- sort(unique(origin_vector[is.na(destination_vector)]))
    badmatch <- badmatch[!badmatch %in% names(custom_match)]
    if (length(badmatch) > 0) {
      warning("Some values were not matched unambiguously: ", 
              paste(badmatch, collapse = ", "), "\n")
    }
    if (origin_regex) {
      if (length(destination_list) > 0) {
        destination_list <- lapply(destination_list, 
                                   function(k) paste(k, collapse = ","))
        destination_list <- sort(unique(do.call("c", 
                                                destination_list)))
        warning("Some strings were matched more than once, and therefore set to <NA> in the result: ", 
                paste(destination_list, collapse = "; "), 
                "\n")
      }
    }
  }
  return(badmatch)
}

# load complete GISAID alignment
algn <- read.dna('./SARSCOV2/gisaid/gisaid_cov2020_sequences_aligned_filter1_noDups_april20.fas', format="fasta")

names <- unique( sapply( strsplit( labels(algn), '/' ), function(x) x[[2]]))
unmatched <- countrycode_edit(names, "country.name", "continent")
continent <- countrycode(names, "country.name", "continent", nomatch = NULL)

country_dict <- data.frame(name=names, continent=continent)

country_dict$continent <- 
  recode(
    country_dict$continent, 
    'Anhui'='Asia',
    'Beijing'='Asia',
    'Chongqing'='Asia',
    'England'='Europe',
    'Foshan'='Asia',
    'Fujian'='Asia',
    'Fuyang'='Asia',
    'Ganzhou'='Asia',
    'Guangdong'='Asia',
    'Guangzhou'='Asia',
    'Hangzhou'='Asia',
    'Hefei'='Asia',
    'Jian'='Asia',
    'Jiangsu'='Asia',
    'Jiujiang'='Asia',
    'Nanchang'='Asia',
    'NanChang'='Asia',
    'Nonthaburi'='Asia',
    'Northern_Ireland'='Europe',
    'Pingxiang'='Asia',
    'Scotland'='Europe',
    'Shandong'='Asia',
    'Shanghai'='Asia',
    'Shangrao'='Asia',
    'Shenzhen'='Asia',
    'Sichuan'='Asia',
    'South_Korea'='Asia',
    'Tianmen'='Asia',
    'Wales'='Europe',
    'Wuhan'='Asia',
    'Wuhan-Hu-1'='Asia',
    'Xinyu'='Asia',
    'Yunnan'='Asia',
    'Zhejiang'='Asia')
    
write.table(country_dict, "./SARSCOV2/country_dict.txt", col.names = T, row.names = F)
