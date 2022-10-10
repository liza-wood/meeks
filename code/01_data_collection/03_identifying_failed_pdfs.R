# Checking for corrupt files:

## Looked through metadata manually to identify where files were corrupt ----
# 5 failed files:
pdf = pdftools::pdf_info("documents/mpo_documents/sbcag/54_lpc_appendix_a.pdf") # failed
pdf = pdftools::pdf_info("documents/mpo_documents/sbcag/87_ff2040_appendices_final.pdf")  # failed
pdf = pdftools::pdf_info("documents/mpo_documents/sbcag/89_ff2040_seir.pdf") # failed
pdf = pdftools::pdf_info("documents/mpo_documents/stancog/58_2017-ftip.pdf") # failed
pdf = pdftools::pdf_info("documents/county_documents/cvag/5_CVAG%20Action%20Plan%20Final.pdf") # failed

# Creating a loop to try to see if there are other corrupt links that download.files() did not catch ----
## Basic approach ----
dir = list.dirs(path="documents/mpo_documents")
names = gsub(pattern = "(.*)\\..*$", replacement = "\\1", basename(dir))
names = data.frame(names[2:19])

pdf.details = list()

for(i in names){
  filepath <- file.path("documents/mpo_documents",paste(i))
  files <- list.files(filepath, full.names = T)
  for(j in files){
    pdf <- pdftools::pdf_info(j)
    pdf.details[[j]] <- data.frame(pdf)
  }
}

# This does not work because different columns
# df <- do.call("rbind",agency.match.total)

# think about joining list by similar columns

## Attempt 1: Trying to create a way to catch the errors, unsuccessful ----
pdf.details = list()
errordf = data.frame(
  msg = ""
)

for(i in names){
  filepath <- file.path("documents/mpo_documents",paste(i))
  files <- list.files(filepath, full.names = T)
  for(j in files){
    tryCatch(
      pdf <- pdftools::pdf_info(j),
      error = function(e){errordf[[j, 'msg']] <<- conditionMessage(e)}
    )
    pdf.details[[j]] <- data.frame(pdf)
  }
}

## Attempt 2: Trying to create a way to catch the errors, unsuccessful ----
pdf.details = NULL

for(i in names){
  filepath <- file.path("documents/mpo_documents",paste(i))
  files <- list.files(filepath, full.names = T)
  for(j in files){
    #ERROR HANDLING
    possibleError <- tryCatch(
      pdftools::pdf_info(j),
      error=function(e) e
    )
    
    if(!inherits(possibleError, "error")){
      #REAL WORK
      pdf <- pdftools::pdf_info(j)
    }
    pdf.details[[j]] <- data.frame(pdf)
  }  
}

