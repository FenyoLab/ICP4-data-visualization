library(bslib)

methods_ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    card(
      card_header ("Methods"),
     # --------------------------
      HTML("<b><u>RNA-Seq Library Preparation & Analysis</u></b>"),
      HTML("<p>For RNA-Seq, extracted RNA samples were processed in the Genome Technology Center at NYU Langone Health. Sample quality 
        was determined using a Bioanalyzer before automated stranded RNA-Seq library prep with polyA selection on ~1 ug of RNA. 
        Sequencing was performed on NovaSeq X+ 10B 100 Cycle Flowcells. </br>
        
        Paired-end sequencing reads (FASTQ format) of length 150 bp (&Delta;CTA/mDBD/ICP4 batch) or 50 bp (&Delta;NTA/ICP4 batch) were 
        subjected to quality control using FastQC to assess read quality metrics(50). Adapter/quality trimming and filtering
        were conducted with Trim Galore (<span><a href='https://github.com/FelixKrueger/TrimGalore' target='_blank'>https://github.com/FelixKrueger/TrimGalore</a></span>)(51)
        and minimum length and quality parameters adjusted after examining the FastQC report. Reads were aligned with STAR(52) 
        to a modified GRCh38 reference genome that included the ICP4 WT or mutant sequence. STAR was run with default parameters 
        with output in sorted BAM format (--outSAMtype BAM SortedByCoordinate) and gene-level quantification enabled (--quantMode GeneCounts).</p>"),

     p("Mock controls were sequenced as a separate batch (paired-end length 50 reads). QC/Trimming/Alignment was performed as 
       described above, aligning to unmodified GRCh38. Batch effect could not be modeled due to a lack of matching induced 
       genotypes from the sequencing run."),
     
     p("Differential expression analysis was performed on all samples with a standard DESeq2 pipeline in R(53). Prior to DE 
       analysis, lowly expressed genes were removed by filtering to those where at least 10 reads were mapped in a minimum of 
       2 or 3 samples, depending on group size. We report raw p-values, q-values and log2FC for all remaining genes.  Linear 
       models included a term to account for batch effect, except when not possible (siNTC comparisons). GSEA pre-ranked(35) 
       was run for each DE comparison using the fgsea R package(54), ranking genes by sign(log2FC) x q-value."),
     
     HTML("<p>For comparisons between our datasets and existing ICP4 Chip-Seq in WT HSV-1 infection, ChipSeq data including Input 
       controls (FASTQ format) were downloaded from BioProject (<span><a href='https://www.ncbi.nlm.nih.gov/bioproject/PRJNA553563' target='_blank'>PRJNA553563</a></span>)
       and subjected to QC/Trimming/Alignment with FASTQC, Trim Galore and STAR (similar workflow as above), aligning to 
       GRCh38 with KOS (HSV-1) genome appended. Peaks were called with MACS2(55) for each of 2, 4 and 6 h time points, 
       producing narrowPeak files. The ChIPseeker R package was then used to annotate peak locations to genomic regions.
       Peaks were filtered to those found in Promotor and 5' UTR regions and then overlap with DEGs from RNASeq analysis by
       the nearest gene.</p>"),
     
     HTML("<p>For comparisons between our datasets and existing RNA-Seq in WT HSV-1 and n12 infection, Total RNA-Seq data including
       uninfected controls (FASTQ format) were downloaded from BioProject (<span><a href='https://www.ncbi.nlm.nih.gov/bioproject/PRJNA851702' target='_blank'>PRJNA851702</a></span>)
       and subjected to QC/Trimming/Alignment with FASTQC, Trim Galore and STAR (similar workflow as above), aligning to GRCh38
       with KOS (HSV-1) genome appended.</p>"),
     
     p("Over-representation analysis using the R package clusterProfiler was performed for pathway enrichment tests involving 
       unranked gene lists resulting from intersection of data sets(38).")
    ),
    card(
      card_header("References"),
      HTML("
        <p>35. Subramanian A, Tamayo P, Mootha VK, Mukherjee S, Ebert BL, Gillette MA, Paulovich A, Pomeroy SL, Golub TR, Lander ES, Mesirov JP. 2005. Gene set enrichment analysis: a knowledge-based approach for interpreting genome-wide expression profiles. Proc Natl Acad Sci U S A 102:15545–15550.</p>
        <p>38. Yu G, Wang L-G, Han Y, He Q-Y. 2012. clusterProfiler: an R package for comparing biological themes among gene clusters. OMICS 16:284–287.</p>
        <p>50. Brown J, Pirrung M, McCue LA. 2017. FQC Dashboard: integrates FastQC results into a web-based, interactive, and extensible FASTQ quality control tool. Bioinformatics 33:3137–3139.</p>
        <p>51. Krueger F. TrimGalore: A wrapper around Cutadapt and FastQC to consistently apply adapter and quality trimming to FastQ files, with extra functionality for RRBS data. Github. <span><a href='https://github.com/FelixKrueger/TrimGalore' target='_blank'>https://github.com/FelixKrueger/TrimGalore</a></span>. Retrieved 18 August 2025.</p>
        <p>52. Dobin A, Davis CA, Schlesinger F, Drenkow J, Zaleski C, Jha S, Batut P, Chaisson M, Gingeras TR. 2013. STAR: ultrafast universal RNA-seq aligner. Bioinformatics 29:15–21.</p>
        <p>53. Love MI, Huber W, Anders S. 2014. Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2. Genome Biol 15:550.</p>
        <p>54. Korotkevich G, Sukhov V, Budin N, Shpak B, Artyomov MN, Sergushichev A. 2016. Fast gene set enrichment analysis. biorxiv;060012v3. Bioinformatics. bioRxiv.</p>
        <p>55. Zhang Y, Liu T, Meyer CA, Eeckhoute J, Johnson DS, Bernstein BE, Nusbaum C, Myers RM, Brown M, Li W, Liu XS. 2008. Model-based analysis of ChIP-Seq (MACS). Genome Biol 9:R137.</p>"
      )
    )
  )
}

methods_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}