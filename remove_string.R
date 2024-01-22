remove_string_from_list <- function(input_list, string_to_remove) {
  index_to_remove <- grep(string_to_remove, input_list)
  if (length(index_to_remove) > 0) {
    output_list <- input_list[-index_to_remove]
  } else {
    output_list <- input_list
  }
  return(output_list)
}
