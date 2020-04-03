clean:
	rm -rf workspace/*
	rm -f *.mp4
1dim_wave_eq_only_u:
	cargo script 1dim_wave_eq_only_u.rs
1dim_wave_eq_pml_both_side:
	cargo script 1dim_wave_eq_pml_both_side.rs
1dim_wave_eq_pml_processing:
	cargo script 1dim_wave_eq_pml_processing.rs
1dim_wave_eq_pml_right_wall:
	cargo script 1dim_wave_eq_pml_right_wall.rs
1dim_wave_eq_u_ut:
	cargo script 1dim_wave_eq_u_ut.rs
2dim_wave_eq_only_u:
	cargo script 2dim_wave_eq_only_u.rs
	python3 2dmp4.py
2dim_wave_eq_u_ut:
	cargo script 2dim_wave_eq_u_ut.rs
	python3 2dmp4.py
2dim_wave_eq_pml_both_side:
	cargo script 2dim_wave_eq_pml_both_side.rs
	python3 2dmp4.py
3dim_wave_eq_pml:
	cargo script 3dim_wave_eq_pml.rs
	python3 3dmp4.py
