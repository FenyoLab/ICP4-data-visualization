library(bslib)

home_ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    card(
      card_header ("Summary"),
         
      p("Successful viral infections reflect the balanced outcome of a tightly regulated program of viral gene expression and 
        manipulation of the host cell environment to favor production of new infectious particles. The productive (lytic) replication
        cycle of herpes simplex virus 1 (HSV-1) is dependent on the essential transcription factor ICP4 encoded by one of five 
        immediate-early genes. At different steps in the HSV-1 temporal cascade, ICP4 either positively or negatively regulates
        transcription of immediate-early, early, and late HSV-1 genes, including its own, through sequence-specific binding to 
        cis-acting regulatory elements. Because of this, the direct regulatory consequences of ICP4 expression on the host 
        transcriptome are less well understood. In this study we used doxycycline-regulated lentiviral expression vectors to 
        inducibly express wild type and mutant ICP4 proteins in uninfected primary human fibroblasts and performed RNA-Seq to 
        identify ICP4-driven changes to the host transcriptome. Cross-referencing our findings to a published dataset of ICP4-dependent 
        changes to the host transcriptome in HSV-1 infected cells provided validation for a subset of differentially-expressed
        genes regulated by ICP4. Furthermore, disrupting the ICP4 DNA binding domain was sufficient to alter the cellular gene 
        transcriptional program responsive to ICP4. This indicates that the DNA-binding domain of ICP4, which is required for 
        site-specific DNA binding to the virus genome, may also regulate binding to the host genome. Together these data provide
        a comprehensive transcriptomic analysis of how wild type and mutant ICP4 expression impact cellular gene expression in uninfected cells.")
    )
  )
}


home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server logic for the Home tab here
  })
}


