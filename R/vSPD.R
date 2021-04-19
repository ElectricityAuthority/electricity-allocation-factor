#' Generate a file list to solve in vSPD from an input directory of GDX files
#'
#' @param out_file Path of file to generate 
#' @param in_dir Directory of files to solve
#'
#' @return outputs file for use with vSPD
generate_file_manifest_dir = function(in_dir, out_file = "data/output/inc/vSPDfileList.inc") {
    files_to_solve = list.files(in_dir)
    files_to_solve = files_to_solve[file_ext(files_to_solve) == "gdx"]
    files_to_solve = file_path_sans_ext(files_to_solve)
    file_manifest = paste("/", paste(paste("'", files_to_solve, "'", sep = ""), collapse = "\n"), "/", sep = "\n")
    writeLines(file_manifest, out_file)
}
