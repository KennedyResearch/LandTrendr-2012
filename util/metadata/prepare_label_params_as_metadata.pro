function prepare_label_params_as_metadata, params
  
  ;get the names of the cover model
  static_model = params.filter_params.static_model
  change_model = params.filter_params.change_model
  
  ;call the static cover model with a mock value (100) so it'll return the equation used
  if static_model ne 'none' then begin
    s_command = 'cover = '+static_model+'(100, equation=static)'
    ok = execute(s_command)
    ;static = s_result.equation
  endif else static = 'none'
  
  ;call the change cover model with a mock value (100) so it'll return the equation used
  if change_model ne 'none' then begin
    c_command = 'cover = '+change_model+'(100), equation=change)'
    ok = execute(c_command)
    ;change = c_result.equation
  endif else change = 'none'
  
  ;repackage the parameter file so that it is a single level structure that will work in yang's metadata concatenator
  struct = {data:params.data,$
    filename:params.filename,$
    parent_file:params.parent_file,$
    class_codes:params.class_codes,$
    diag_file:params.diag_file,$
    end_year:params.end_year,$
    extract_tc_ftv:params.extract_tc_ftv,$
    merge_recovery:params.merge_recovery,$
    run_name:params.run_name,$
    start_year:params.start_year,$
    collapse_dist_angle:params.filter_params.collapse_dist_angle,$
    collapse_recv_angle:params.filter_params.collapse_recv_angle,$
    pct_tree_gain:params.filter_params.pct_tree_gain,$
    pct_tree_loss1:params.filter_params.pct_tree_loss1,$
    pct_tree_loss20:params.filter_params.pct_tree_loss20,$
    pre_dist_cover:params.filter_params.pre_dist_cover,$
    change_model:params.filter_params.change_model,$
    change_model_equation:change,$
    static_model:params.filter_params.static_model,$
    static_model_equation:static,$
    use_relative_mag:params.use_relative_mag}
  
  return, struct
  
end
