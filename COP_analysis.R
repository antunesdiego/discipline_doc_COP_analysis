#1. Load packages
#install.packages("writexl")
#install.packages("knitr")
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(writexl)
library(knitr)

#2. Set the path to the main folder
main_folder <- "G:/Meu Drive/Doutorado UFSC/Disciplinas/Biomecânica/COP"

#3. List all COP folders
file_list <- list.files(path = main_folder,
                        pattern = "*.xlsx",
                        recursive = TRUE,
                        full.names = TRUE)

# 3b. Get the RELATIVE paths (for our new 'origin' column)
# Note: full.names = FALSE here
relative_file_paths <- list.files(path = main_folder,
                                  pattern = "*.xlsx", 
                                  recursive = TRUE,
                                  full.names = FALSE)

# 3c. Create a NAMED vector of files
# We use the relative_paths as the "names" for the full_paths.
named_file_list <- set_names(file_list, relative_file_paths)

#List Verification
print(named_file_list)

#4. Read and combine all Excel files into a single data frame
COP_data <- map_dfr(named_file_list, read_excel, .id = "origin", skip = 3)



#4a. Check the structure
glimpse(COP_data)

#4b. Check the first few rows
head(COP_data)

#5. Clean Data
# 1. Remove the first row of data from *each* file (the units row)
# 2. Convert the data columns from text back to numeric

COP_data_clean <- COP_data %>%
  group_by(origin) %>%
  slice(-1) %>%  # Remove the first row of each group
  ungroup() %>%
  mutate(across(!origin, as.numeric)) # Convert character columns to numeric

# 6. Now, check the *cleaned* data (replaces old step 5)
# The "units" row should be gone.
# The data columns should now be <dbl> (numeric).
glimpse(COP_data_clean)
head(COP_data_clean)  

#7. Select and rename columns
COP_df <- COP_data_clean %>%
  select(
    #keep the ID column
    origin,
    
    #rename columns Cx and Cy to COPx_mm and COPy_mm
    COPx_mm = Cx...9,
    COPy_mm = Cy...10,
  ) %>%

#8 Now, create the _cm columns from the _mm columns
mutate(
  COPx_cm = round(COPx_mm / 10, digits = 2),
  COPy_cm = round(COPy_mm / 10, digits = 2) 
)

#9. Check the final data frame
glimpse(COP_df)
head(COP_df)

#10. Add displacement Colummns COPx_disp and COPy_disp for Subjects

COP_df <- COP_df %>%
  group_by(origin) %>% # Process each file/subject separately
  mutate(
    # 1D Medial-Lateral (X-axis) Displacement
    # We take the absolute value of the change in X from the first frame
    COPx_disp = abs(COPx_cm - lag(COPx_cm)),
    
    # 1D Anterior-Posterior (Y-axis) Displacement
    # We take the absolute value of the change in Y from the first frame,
    COPy_disp = abs(COPy_cm - lag(COPy_cm))
  ) %>%
  ungroup() # We can ungroup now that the calculation is done

# 2. Check your updated data frame
# You will now see the two new segment distance columns
glimpse(COP_df)

# 3. Look at the data
head(COP_df, 10)

#11. Calculate Velocity Columns VELx_cm_s and VELy_cm_s for Subjects

# (1) Define your sampling frequency (2000 Hz)
SAMPLING_FREQUENCY_HZ <- 2000

# (2) Calculate the time interval (delta_t)
# delta_t will be 0.0005 seconds
delta_t_s <- 1 / SAMPLING_FREQUENCY_HZ

# (3) Now, calculate the velocities
COP_df <- COP_df %>%
  group_by(origin) %>% # Process each file/subject separately
  mutate(
    # Velocity in the Medial-Lateral direction (X-axis)
    VELx_cm_s = COPx_disp / delta_t_s,
    
    # Velocity in the Anterior-Posterior direction (Y-axis)
    VELy_cm_s = COPy_disp / delta_t_s
  ) %>%
  ungroup() # We can ungroup now that the calculation is done

# (4) Check your updated data frame
# You will now see the two new velocity columns
glimpse(COP_df)
head(COP_df, 10)

#12. Create COP summary stats

COP_summary <- COP_df %>%
  group_by(origin) %>%
  summarise(
    # --- 1. Amplitude (Range) Calculation ---
    amplitude_COPx_cm = max(COPx_cm, na.rm = TRUE) - min(COPx_cm, na.rm = TRUE),
    amplitude_COPy_cm = max(COPy_cm, na.rm = TRUE) - min(COPy_cm, na.rm = TRUE),
    # --- 2. Mean Velocity (What we already did) ---
    mean_VELx_cm_s = mean(VELx_cm_s, na.rm = TRUE),
    mean_VELy_cm_s = mean(VELy_cm_s, na.rm = TRUE),
    # --- 3. RMS Velocity (What the professor suggested) ---
    # We square the velocity (VELx_cm_s^2), take the mean, then the sqrt
    rms_VELx_cm_s = sqrt(mean(VELx_cm_s^2, na.rm = TRUE)),
    rms_VELy_cm_s = sqrt(mean(VELy_cm_s^2, na.rm = TRUE)),
    
    .groups = 'drop' # Good practice: ungroup after summarising
  )
 
# 2. Check your new stats table
# It will now have mean_VEL... AND rms_VEL...
print(COP_summary)
head(COP_summary)

#13. Calculate elipse area

calculate_ellipse_area <- function(data) {
  
  # Step 1: Get the data
  # The "box" is named 'data'. We pull out the x and y columns
  # and remove any NA rows (like the first row from our lag() calc).
  cop_data <- data.frame(x = data$COPx_cm, y = data$COPy_cm)
  cop_data <- na.omit(cop_data)
  
  
  # Step 2: Find the "Shape" of the data cloud
  # We calculate the 2x2 covariance matrix.
  # This tells us the spread (variance) in X, the spread in Y,
  # and the relationship (covariance) between X and Y.
  # This is what finds the "diagonal angle" of the data.
  cov_matrix <- cov(cop_data)
  
  
  # Step 3: Find the "Principal Axes" (a and b)
  # 'eigen()' is a linear algebra function that finds the
  # principal axes of the covariance matrix.
  # eigenvalues[1] = variance along the "major" (long) axis
  # eigenvalues[2] = variance along the "minor" (short) axis
  # This is the statistical way to find your 'a' and 'b'.
  eigenvalues <- eigen(cov_matrix)$values
  
  
  # Step 4: Get the 95% "Scaler"
  # This is a statistical constant (~5.991).
  # It's the "magic number" from the Chi-Squared distribution
  # that scales our axes to contain exactly 95% of the points.
  chisq_val <- qchisq(0.95, df = 2)
  
  
  # Step 5: Apply the Professor's Formula (A = pi * a * b)
  # This is the statistical version of that formula:
  # a (semi-axis) = sqrt(eigenvalue1 * chisq_val)
  # b (semi-axis) = sqrt(eigenvalue2 * chisq_val)
  # pi * a * b  ... simplifies to ...
  area <- pi * chisq_val * sqrt(eigenvalues[1] * eigenvalues[2])
  
  
  # Step 6: Return the final number
  # R returns this single number (e.g., 12.34 cm^2)
  # and 'map_dbl' puts it in the cell for that subject.
  return(area)
}
# 2. Now, apply this function to each subject's data
ellipse_areas <- COP_df %>%
  group_by(origin) %>%
  nest() %>%
  mutate(
    area_95_ellipse_cm2 = map_dbl(data, calculate_ellipse_area)
    ) %>%
  
  # 4. We only keep the results
  select(origin, area_95_ellipse_cm2)

# 5. Check the results
print(ellipse_areas)

#14. Combine summary stats and ellipse areas
COP_final_summary <- COP_summary %>%
  left_join(ellipse_areas, by = "origin")
# 2. Check the final summary table
print(COP_final_summary)
head(COP_final_summary)

#15. Export the final summary table to Excel
write_xlsx(COP_final_summary, path = "cop_final_results.xlsx")
# The file "cop_final_results.xlsx" will be saved in your working directory.

#16 # --- 3. Create a formatted table ---
# This is a bonus step:
# We round all the numeric columns to 2 decimal places
# *only* for this table (it doesn't change your 'final_stats_df')
COP_table_data <- COP_final_summary %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# 2. Now, create the formatted table using knitr::kable()
# The kable() function creates a clean, formatted table
kable(COP_table_data, caption = "Summary of COP Results")
