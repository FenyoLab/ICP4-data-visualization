library(bslib)

home_ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    card(
      card_header ("Summary"),
      
      p("Successful viral infections reflect the balanced outcome of a tightly regulated program of viral gene expression and 
        manipulation of the host cell to favor production of new infectious particles. The productive replication cycle of herpes 
        simplex virus 1 (HSV-1) is dependent on the immediate-early essential transcription factor ICP4, which positively or negatively
        regulates transcription of immediate-early, early, and late HSV-1 genes at different steps in the temporal cascade, largely 
        through sequence-specific binding to cis-acting regulatory elements. In contrast, direct regulation of host transcription 
        programming by ICP4 is less well understood. In this study we exogenously expressed doxycycline-inducible wild-type and 
        mutant ICP4 proteins in uninfected primary human fibroblasts and performed RNA-Seq to identify ICP4-driven changes to the 
        host transcriptome. Cross-referencing our findings to a published dataset of ICP4-dependent changes to the host transcriptome 
        in cells infected with HSV-1 provided validation for a subset of ICP4-regulated differentially-expressed genes. Furthermore, 
        mutations in ICP4 that disrupt interactions with the host transcriptional machinery or interfere with site-specific DNA 
        binding further modified this ICP4-driven remodeling of host transcription programming. Taken together, these data provide 
        a comprehensive transcriptomic analysis of ICP4-driven dysregulation of host gene expression in uninfected cells. ")
    )
  )
}


home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server logic for the Home tab here
  })
}


