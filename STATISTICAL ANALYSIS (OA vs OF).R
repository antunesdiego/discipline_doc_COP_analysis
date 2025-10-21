# SCRIPT 2: STATISTICAL ANALYSIS (OA vs OF)
#Load libraries
#install.packages("ggplot2")
#install.packages("patchwork")
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(patchwork)

#1. Load data results
COP_stats_df <- read_excel("cop_final_results.xlsx")
#View data
print(COP_stats_df)

#2. Clean data and sep "condition", "subject" and "trial" columns
COP_stats_df <- COP_stats_df %>%
  # We assume the format is "OA - SubjectName_Trial.xlsx"
  # This separates "OA - Falconi_01.xlsx" into 3 useful columns
  tidyr::separate(
    col = origin, 
    
    # We create new columns
    into = c("condition", "subject", "trial"), 
    
    # We tell it to split at " - ", "_", or ".xlsx"
    sep = " - |_|\\.xlsx", 
    
    # Drop any extra pieces
    extra = "drop" 
  )
#View cleaned data
glimpse(COP_stats_df)

#3. Calculate Descriptive Statistics per Subject per Condition
# We will group by BOTH condition (OA/OF) and subject
descriptive_stats_subjects <- COP_stats_df %>%
  
  group_by(condition, subject) %>%
  
  # Now we summarise across the trials
  summarise(
    
    # First, let's count how many trials we are summarising
    n_trials = n(),
    
    # Now, calculate stats for ALL numeric columns (amplitude, velocity, area, etc.)
    across(where(is.numeric),
           
           # We apply a list of functions:
           list(
             mean = ~mean(., na.rm = TRUE),
             sd   = ~sd(., na.rm = TRUE),
             
             # CV = (sd / mean) * 100
             cv   = ~ (sd(., na.rm = TRUE) / mean(., na.rm = TRUE)) * 100,
             
             # SWC (Smallest Worthwhile Change)
             # We use the common 0.2 * SD (trivial) method
             swc_0.2 = ~ sd(., na.rm = TRUE) * 0.2
           ),
           
           # This names the new columns like:
           # "amplitude_Cx_cm_mean", "amplitude_Cx_cm_sd", "amplitude_Cx_cm_cv"
           .names = "{.col}_{.fn}"
    ),
    
    .groups = "drop" # Always good practice to ungroup after summarise
  )

#View descriptive stats per subject
print(descriptive_stats_subjects, width = Inf)


#4. Final Analysis condition comparisons for each subject
#Reshape data for easier analysis considering only low CV

# We need to get OA and OF side-by-side for comparison

swc_data_wide_N2 <- descriptive_stats_subjects %>%
  # Select only the subject, condition, and ALL velocity metrics
  select(
    subject, 
    condition,
    # Mean Velocity columns (mean and swc)
    mean_VELx_cm_s_mean, 
    mean_VELy_cm_s_mean,
    mean_VELx_cm_s_swc_0.2,
    mean_VELy_cm_s_swc_0.2,
    
    # RMS Velocity columns (mean and swc)
    rms_VELx_cm_s_mean, 
    rms_VELy_cm_s_mean,
    rms_VELx_cm_s_swc_0.2, 
    rms_VELy_cm_s_swc_0.2
  ) %>%
  
  # Pivot to get OA and OF side-by-side
  pivot_wider(
    id_cols = subject,             
    names_from = condition,      
    values_from = c(ends_with("_mean"), ends_with("_swc_0.2")) 
  )

# Check the new wide data
glimpse(swc_data_wide_N2)
print(swc_data_wide_N2, width = Inf)  

#Calculate Change and Compare to SWC
# This is our main analysis. We compare the change (OF-OA)
# to the SWC of the baseline (OA) condition.

final_practical_summary <- swc_data_wide_N2 %>%
  mutate(
    
    # --- Mean Velocity (X-axis) ---
    change_mean_vel_x = mean_VELx_cm_s_mean_OF - mean_VELx_cm_s_mean_OA,
    is_worthwhile_mean_vel_x = abs(change_mean_vel_x) > mean_VELx_cm_s_swc_0.2_OA,
    
    # --- Mean Velocity (Y-axis) ---
    change_mean_vel_y = mean_VELy_cm_s_mean_OF - mean_VELy_cm_s_mean_OA,
    is_worthwhile_mean_vel_y = abs(change_mean_vel_y) > mean_VELy_cm_s_swc_0.2_OA,
    
    # --- RMS Velocity (X-axis) ---
    change_rms_vel_x = rms_VELx_cm_s_mean_OF - rms_VELx_cm_s_mean_OA,
    is_worthwhile_rms_vel_x = abs(change_rms_vel_x) > rms_VELx_cm_s_swc_0.2_OA,
    
    # --- RMS Velocity (Y-axis) ---
    change_rms_vel_y = rms_VELy_cm_s_mean_OF - rms_VELy_cm_s_mean_OA,
    is_worthwhile_rms_vel_y = abs(change_rms_vel_y) > rms_VELy_cm_s_swc_0.2_OA
    
  ) %>%
  
  # Clean up: Keep only the subject and the final TRUE/FALSE results
  select(subject, starts_with("is_worthwhile_"))


# --- 3. Print your FINAL results table ---
print("--- Practical Significance (SWC) Results (N=2) ---")
print(final_practical_summary)
# You can export this final table if needed
write_xlsx(final_practical_summary, path = "COP_final_practical_summary.xlsx")

#5. Graphical Representation (Optional)
# Let's create a grouped bar plot for RMS Velocity (X-axis)
# showing the mean values for OA and OF per subject,
# with an error bar representing the SWC for the OA condition.
# 1. Filter data just for the plot (optional, but cleaner)
plot_data_rms_x <- descriptive_stats_subjects %>%
  select(subject, condition, rms_VELx_cm_s_mean, rms_VELx_cm_s_swc_0.2)

# 2. Create the grouped bar plot
plot_x <- ggplot(
  plot_data_rms_x, 
  aes(x = subject, y = rms_VELx_cm_s_mean, fill = condition)
) +
  # Create side-by-side bars
  geom_col(position = "dodge") +
  # Add the SWC error bar *only* on top of the OA (baseline) bar
  geom_errorbar(
    # Filter to only get OA data for the error bars
    data = filter(plot_data_rms_x, condition == "OA"), 
    
    # ymin = top of the bar, ymax = top of the bar + SWC
    aes(
      ymin = rms_VELx_cm_s_mean, 
      ymax = rms_VELx_cm_s_mean + rms_VELx_cm_s_swc_0.2
    ),
    
    # Align the error bar with the 'OA' bar
    position = position_dodge(width = 0.9), 
    
    # Style the error bar
    color = "black",
    width = 0.25 # Width of the error bar cap
  ) +
  
  # --- Labels and Titles ---
  labs(
    title = "Practical Significance (SWC) for RMS Velocity (X-axis)",
    subtitle = "Change (OA vs OF) compared to SWC threshold (black bar)",
    x = "Subject",
    y = "RMS Velocity (cm/s)",
    fill = "Condition"
  ) +
  theme_minimal() # A clean theme

# 3. Show the plot
print(plot_x)

#5. Graphical Representation (Optional)
# Let's create a grouped bar plot for RMS Velocity (y-axis)
# showing the mean values for OA and OF per subject,
# with an error bar representing the SWC for the OA condition.
# 1. Filter data just for the plot (optional, but cleaner)
plot_data_rms_y <- descriptive_stats_subjects %>%
  select(subject, condition, rms_VELy_cm_s_mean, rms_VELy_cm_s_swc_0.2)

# 2. Create the grouped bar plot
plot_y <- ggplot(
  plot_data_rms_y, 
  aes(x = subject, y = rms_VELy_cm_s_mean, fill = condition)
) +
  # Create side-by-side bars
  geom_col(position = "dodge") +
  # Add the SWC error bar *only* on top of the OA (baseline) bar
  geom_errorbar(
    # Filter to only get OA data for the error bars
    data = filter(plot_data_rms_y, condition == "OA"), 
    
    # ymin = top of the bar, ymax = top of the bar + SWC
    aes(
      ymin = rms_VELy_cm_s_mean, 
      ymax = rms_VELy_cm_s_mean + rms_VELy_cm_s_swc_0.2
    ),
    
    # Align the error bar with the 'OA' bar
    position = position_dodge(width = 0.9), 
    
    # Style the error bar
    color = "black",
    width = 0.25 # Width of the error bar cap
  ) +
  
  # --- Labels and Titles ---
  labs(
    title = "Practical Significance (SWC) for RMS Velocity (Y-axis)",
    subtitle = "Change (OA vs OF) compared to SWC threshold (black bar)",
    x = "Subject",
    y = "RMS Velocity (cm/s)",
    fill = "Condition"
  ) +
  theme_minimal() # A clean theme

# 3. Show the plot
print(plot_y)

#both graphics
plot_x + plot_y

# --- END OF SCRIPT ---