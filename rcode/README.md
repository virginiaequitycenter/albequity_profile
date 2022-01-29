## rcode material

* data_sources.R - pulls data from: ACS, County health rankings, CDC/NCHS, United Way/ALICE; saves downloaded files: health_rankings.xlsx, tract_expectancy.xlsx, alice_va.xlsx; explrots processed data files for visualization: demographic_table.csv, sex_age.csv, ahdi_table.csv, tract_ahdi.csv, geographic_education.csv, tract_enroll.csv, student_data.csv, origins.Rda,  alice_alb_hhs.csv, alice_thresh.csv, med_inc_tract.csv, gini_index.csv, gini_index_all.csv, housing_costs.csv
* ahdi_visuals.R: reads ahdi_table.csv, tract_names.csv, tract_ahdi.csv; creates ahdi_table.html, ahdi_map.jpg, ahdi_dots_just_.jpg
* demographic_visuals.R: reads demographic_table.csv, albco_profile_raceovertime.xlsx, origins.Rda; creates race_1790_2010.jpg, race_poc_2000_2019.jpg, year_age.jpg, sex_age.jpg, citizen.jpg, origin.jpg
* health_visuals.R: reads tract_names.csv, tract_ahdi.csv, race_exp.csv, health_rankings.xlsx, food_insecurity.csv; creates life_exp.jpg, life_exp_map.jpg, food_insecure.jpg, food_insecure_icon.jpg
* snap_data.R: pulls data from ACS, USDA; creates snap_locations_map.jpg
* education_visuals.R: reads education_distrbution.csv, geographic_education.csv, tract_enroll.csv, student_data.csv; creates ed_race.jpg, ed_map.jpg, bach_deg_graph.jpg, enroll_map.jpg, suspended.jpg, absent.jpg, ap.jpg
* living_standards_visuals.R: reads housing_costs.csv, alice_alb_hhs.csv, alice_thresh.csv, med_inc_tract.csv, gini_index.csv, gini_index_all.csv; creates housing_costs.jpg, alice_hhs.jpg, alice_thresh.jpg, alice_thresh_overall.jpg, hhinc_map.jpg, gini_index.jpg, gini_index_2019.jpg
* tract_demo_table.R: pulls data from ACS, reads tract_names.csv; creates supplemental_tract_demo.pdf
* profile_visuals.R: compilation of above code (though some pieces are missing)
