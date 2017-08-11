library(rmarkdown)

render(input = 'Modelling_and_Analysis.Rmd',
       output_file = 'Modelling_and_Analysis.pdf', output_format = 'pdf_document')

render(input = 'Modelling_and_Analysis.Rmd',
       output_file = 'README.md', output_format = 'md_document')

render(input = 'Main_README.Rmd',
       output_file = '../README.md')
