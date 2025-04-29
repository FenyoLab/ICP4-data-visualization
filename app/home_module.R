library(bslib)

home_ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = 1,
    heights_equal = "row",
    card(
         card_header ("Summary"),
         
    p("Under normal physiological conditions, white adipocytes efficiently store excess calories as triacylglycerol. 
        However, obesity frequently overwhelms their capacity to do so, contributing to the pathogenesis of type 2 
        diabetes and other metabolic diseases. Thermogenic brown and beige adipocytes, meanwhile, 
        can convert chemical energy, such as triacylglycerol, into heat, 
        and are associated with anti-diabetes effects in mice and humans."),
    
    
    p("In addition to their bioenergetic properties, white, brown, and beige adipocytes secrete distinct signaling molecules 
        that maintain homeostasis by crosstalk with tissues such as the pancreas, liver, skeletal muscle, and brain. 
        While a number of these white and brown adipocyte-derived endocrine factors have been demonstrated to have important physiological functions, 
        adipocytes are thought to secrete over 1,000 unique polypeptides and microproteins and an even greater number of metabolites. 
        Thus, we hypothesize that white and brown adipocytes may exert many of their effects on systemic metabolism via secreted mediators, 
        a large number of which have yet to be characterized. 
        To date, this vast, undefined adipocyte secretome represents a major opportunity for discovering new biology 
        with therapeutic potential for obesity, type 2 diabetes, and metabolic diseases. 
        However, a central obstacle in this area has been the lack of suitable technologies to quantitatively identify low abundance endocrine factors 
        and natural small molecules in the blood and determine their cellular source and molecular targets. 
        Using a host of newly validated cell type-selective protein labeling and affinity purification chemoproteomic approaches, 
        we aim to generate an encyclopedia of the white and brown adipocyte secretome 
        in mouse models and humans as a key prerequisite to elucidating the role of these mediators in normal physiology and disease."),
      ),
  
    card(
        card_header("Team"),
        p("These studies are supported by NIH grant RC2DK129961 and will be performed as part of a multi-institutional collaborative between the labs of:"),
        
        HTML("
        <ul>
          <li> <a href='https://cohenlab.rockefeller.edu/' target='_blank'>Paul Cohen</a> (Rockefeller University) </li>
          <li> <a href='https://lab.rockefeller.edu/chait/' target='_blank'>Brian Chait</a> (Rockefeller University) </li>
          <li> <a href='https://fenyolab.org/' target='_blank'>David Fenyö</a> (New York University School of Medicine) </li>
          <li> <a href='https://saghatelian.salk.edu/' target='_blank'>Alan Saghatelian</a> (Salk Institute for Biological Studies) </li>
          <li> <a href='http://www.tinglab.org/' target='_blank'>Alice Ting</a> (Stanford University) </li>
        </ul>
            ")
      )
  )

}


home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server logic for the Home tab here
  })
}


