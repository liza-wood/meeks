## Transportation agencies as consumers and producers of science: The case of state, regional, and county transportation agencies in California  

Contact:
* Liza Wood (belwood@ucdavis.edu)
* Tyler Scott  

This repository contains code for analyzing scientific references in California's transportation agency documents. Code is stored in the `code` folder and the data is stored in the `data` folder.  

**File organization**  

* `01_data_collection/`: A collection of scripts for (1) scraping and parsing agency websites for PDF links, (2) downloading the documents to Box `documents/`, (3) testing for failed downloads, and (4) combining all of the metadata we have about the scraped documents and their associated agencies. Outputs are assigned to `data/agency_doc_metadata/` folder in Box.  
* `02_citation_extraction/`: Feeds downloaded agency PDFs using Anystyle to identify probable citations in our documents. Outputs are in `data/compiled_anystyle_results/` in Box as RDS files. Outputs include both using Anystyle extraction with and without layout as a factor.  
* `03_data_cleaning/`: A collection of (1) three scripts that clean up the citations identified at each agency level (county, mpo, state), with outputs as `[agencylevel]_refined.csv` saved in Box within `data/compiled_anystyle_results/`. (2) `combined_citation_cleaning.R` combines and further cleans these citations, and stored their combined output in the same Box folder. (3) `journal_and_agency_cleaning.R` is then even more extensive cleaning of the names of journals and agencies to get them to match specified lists.  
* `04_analyses/`: Includes the final cleaning, summaries, and analysis for the published paper.  