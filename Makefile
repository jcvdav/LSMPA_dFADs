all: figures tables
figures: results/figs/global_map_of_dfad_effort_and_mpas.pdf results/figs/dFAD_gradient_plot.pdf results/figs/mpas_fad_dif.pdf results/figs/mpas_fad_pre_post.pdf
tables: results/tabs/regression_results.docx
supp_figures: results/figs/time_series.pdf results/figs/dFAD_effort_by_ring.pdf results/figs/abs_dFAD_gradient_plot.pdf

processed_data: processed_data/annual_rfmo_effort_1deg.rds processed_data/selected_LSMPAs_viz.gpkg

# Figures and tables
results/figs/global_map_of_dfad_effort_and_mpas.pdf: scripts/02_analysis_and_content/01_global_map_of_ps_and_mpas.R processed_data/annual_rfmo_effort_1deg.rds processed_data/selected_LSMPAs_viz.gpkg
		cd $(<D); Rscript $(<F)

results/figs/mpas_fad_dif.pdf: scripts/02_analysis_and_content/02_mpas_fad_pre_post_maps.R processed_data/annual_rfmo_effort_1deg.rds processed_data/selected_LSMPAs_viz.gpkg
		cd $(<D); Rscript $(<F)

results/figs/mpas_fad_pre_post.pdf: scripts/02_analysis_and_content/02_mpas_fad_pre_post_maps.R processed_data/annual_rfmo_effort_1deg.rds processed_data/selected_LSMPAs_viz.gpkg
		cd $(<D); Rscript $(<F)
		
results/figs/dFAD_gradient_plot.pdf: scripts/02_analysis_and_content/03_dfad_gradient.R processed_data/annual_rfmo_effort_1deg.rds processed_data/selected_LSMPAs_viz.gpkg
		cd $(<D); Rscript $(<F)

results/figs/abs_dFAD_gradient_plot.pdf: scripts/02_analysis_and_content/03_dfad_gradient.R processed_data/annual_rfmo_effort_1deg.rds processed_data/selected_LSMPAs_viz.gpkg
		cd $(<D); Rscript $(<F)

results/figs/dFAD_effort_by_ring.pdf: scripts/02_analysis_and_content/03_dfad_gradient.R processed_data/annual_rfmo_effort_1deg.rds processed_data/selected_LSMPAs_viz.gpkg
		cd $(<D); Rscript $(<F)

results/figs/time_series.pdf: scripts/02_analysis_and_content/04_time_series.R processed_data/annual_rfmo_effort_1deg.rds
		cd $(<D); Rscript $(<F)

results/tabs/regression_results.docx: scripts/02_analysis_and_content/03_dfad_gradient.R processed_data/annual_rfmo_effort_1deg.rds processed_data/selected_LSMPAs_viz.gpkg
		cd $(<D); Rscript $(<F)

# Analysis

# Processed data
processed_data/annual_wcpfc_effort_1deg.rds: scripts/01_make_inputs/04_clean_wcpfc.R
		cd $(<D); Rscript $(<F)

processed_data/annual_iattc_effort_1deg.rds: scripts/01_make_inputs/05_clean_iattc.R
		cd $(<D); Rscript $(<F)

processed_data/annual_iccat_effort_1deg.rds: scripts/01_make_inputs/06_clean_iccat.R
		cd $(<D); Rscript $(<F)
		
processed_data/annual_iotc_effort_1deg.rds: scripts/01_make_inputs/07_clean_iotc.R
		cd $(<D); Rscript $(<F)

processed_data/annual_rfmo_effort_1deg.rds: scripts/01_make_inputs/08_combine_rfmo_data.R processed_data/annual_wcpfc_effort_1deg.rds processed_data/annual_iattc_effort_1deg.rds processed_data/annual_iccat_effort_1deg.rds processed_data/annual_iotc_effort_1deg.rds
		cd $(<D); Rscript $(<F)

processed_data/selected_LSMPAs_viz.gpkg: scripts/01_make_inputs/09_clean_LSMPAs.R raw_data/list_LSMPAs.xlsx
		cd $(<D); Rscript $(<F)


# Output data
processed_data/annual_pre_post_activity_by_select_mpa.rds: scripts/02_analysis_and_content/02_mpas_fad_pre_post_maps.R processed_data/selected_LSMPAs_viz.gpkg processed_data/annual_rfmo_effort_1deg.rds
		cd $(<D); Rscript $(<F)

processed_data/pre_post_activity_by_select_mpa.rds: scripts/02_analysis_and_content/02_mpas_fad_pre_post_maps.R processed_data/selected_LSMPAs_viz.gpkg processed_data/annual_rfmo_effort_1deg.rds
		cd $(<D); Rscript $(<F)