filter_speed_base <- function(x, spatial_accuracy, max_sq_move_rate) {
# %% purge steps exceeding the reasonable speed limit
#  sq_sl <- distance_sq(x)
#  sq_mr <- sq_dist / diff(as.numeric(x$t_))
#
#  errors <- sq_sl > spatial_accuracy^2 & sq_mr > max_sq_move_rate

#
#  while any(errors)
#    if first_error==1
#        speed_deleted_records=[speed_deleted_records ind_data(1,1)];
#        ind_data(1,:)=[];
#    else
#        speed_deleted_records=[speed_deleted_records ind_data(first_error+1,1)];
#        ind_data(first_error+1,:)=[];
#    end
#    clear first_error
#    sq_step_length=(diff(ind_data(:,6)).^2)+(diff(ind_data(:,7)).^2);
#    sq_move_rates=sq_step_length./diff(ind_data(:,3));
#    errors=(sq_step_length>(spatial_accuracy^2))&(sq_move_rates>max_sq_move_rate);
#end
}
