

#### which format to run #####
nolayout = T
format = '--no-layout'

packs = c('data.table','pdftools','pbapply','stringr')
need = packs[!packs %in% installed.packages()[,'Package']]
sapply(need,install.packages)

lapply(packs,require,character.only = T)

dir.create('reference_extracts_nolayout/')

already_extracted = list.files('reference_extracts_nolayout/',full.names = T,recursive = T,pattern = 'json')
big_dirs = list.dirs('documents/',recursive = F,full.names = T)


sapply(gsub('^documents','reference_extracts_nolayout',big_dirs[!grepl('ceqa',big_dirs)]),dir.create,recursive = T)


big_dirs = big_dirs[!grepl('ceqa',big_dirs)]
for(b in big_dirs[3]){

fls = list.files(b,recursive = T,pattern = 'PDF$|pdf$',full.names = T)
dirs = dirname(fls)
json_files <- gsub('^documents','reference_extracts_nolayout',fls)
json_files <- gsub('PDF$|pdf$','json',json_files)
json_dirs <- dirname(json_files)
sapply(unique(json_dirs[!dir.exists(json_dirs)]),dir.create,recursive = T)
still_need = !json_files %in% already_extracted
pblapply(seq_along(json_files[still_need]),function(i){
  system(paste('anystyle --overwrite -f json,xml,csl find --no-layout',fls[still_need][i],' ',json_dirs[still_need][i]))
},cl = 4)
}

# pdf_version = gsub('text_as_datatable','documents',dt)
# pdf_version = gsub('txt$','pdf',pdf_version)
# pdf_version = gsub('[0-9]{1,}--','',pdf_version)
# stringcombos = c('Works cited','Works Cited','References','Bibliography','Citations')
# stc = paste(c(stringcombos,toupper(stringcombos)),collapse='|')
# #test = sample(pdf_version,1)
# #temp= pdftools::pdf_text(test)
# refs = which(grepl(stc,temp)&!grepl('Table of Contents|TABLE OF CONTENTS|DOCUMENT ORGANIZATION',temp))


