#00_test.R
dir_markdown <- here::here("markdown")
dir_out_chapters <- here::here()

test_table <-  head(mtcars)


# Render the doc
rmarkdown::render(paste0(dir_markdown, "/test.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0("test.docx")
)
