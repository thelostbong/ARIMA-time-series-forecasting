@echo off
echo ===================================================
echo     COVID-19 ARIMA Analysis - Quick Run
echo ===================================================
echo This will run the complete unified analysis
echo ===================================================
echo.

REM Check if R is installed
echo [1/3] Checking R installation...
R --version >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: R is not installed or not in PATH!
    echo Please install R from https://cran.r-project.org/
    echo Or add R to your system PATH
    pause
    exit /b 1
)
echo - R is installed and working

REM Check if data file exists
echo.
echo [2/3] Checking data file...
if not exist "time-series-19-covid-combined.csv" (
    echo ERROR: Data file 'time-series-19-covid-combined.csv' not found!
    echo Please ensure the CSV file is in the same directory.
    pause
    exit /b 1
)
echo - Data file found

REM Check if analysis scripts exist
echo.
echo [3/3] Checking analysis scripts...
if not exist "COVID19_UNIFIED_ANALYSIS.R" (
    echo ERROR: Analysis script 'COVID19_UNIFIED_ANALYSIS.R' not found!
    pause
    exit /b 1
)
if not exist "SIMPLE_ANALYSIS.R" (
    echo ERROR: Backup script 'SIMPLE_ANALYSIS.R' not found!
    pause
    exit /b 1
)
echo - Analysis scripts ready

echo.
echo ===================================================
echo READY TO ANALYZE COVID-19 DATA
echo ===================================================
echo.
echo Analysis options:
echo 1. Complete Analysis (recommended - requires packages)
echo 2. Simple Analysis (base R only - no packages needed)
echo.
echo Default settings: US, Confirmed cases, 30-day forecast
echo.

set /p "choice=Choose option (1 or 2): "

if "%choice%"=="2" goto simple_analysis

REM Default to complete analysis (option 1 or any other input)
:complete_analysis
echo.
echo ===================================================
echo RUNNING COMPLETE COVID-19 ARIMA ANALYSIS...
echo ===================================================
echo.
echo This may take 2-3 minutes on first run (installing packages)...
echo.

REM Try the complete analysis first
R --vanilla --slave -e "source('COVID19_UNIFIED_ANALYSIS.R')"

REM Check if successful
if %errorlevel% equ 0 (
    echo.
    echo Complete analysis succeeded!
    goto success
)

echo.
echo ===================================================
echo COMPLETE ANALYSIS FAILED - TRYING SIMPLE VERSION
echo ===================================================
echo.
echo The complete analysis failed (likely package issues).
echo Automatically falling back to simple analysis...
echo.

:simple_analysis
echo.
echo ===================================================
echo RUNNING SIMPLE COVID-19 ARIMA ANALYSIS...
echo ===================================================
echo.
echo Using base R only (no external packages required)...
echo This will take 1-2 minutes...
echo.

REM Run the simple analysis
R --vanilla --slave -e "source('SIMPLE_ANALYSIS.R')"

REM Check if successful
if %errorlevel% neq 0 (
    echo.
    echo ===================================================
    echo ERROR: Simple analysis also failed!
    echo ===================================================
    echo.
    echo This suggests a more fundamental issue.
    echo Please check:
    echo 1. R installation is working properly
    echo 2. Data file is not corrupted
    echo 3. You have write permissions in this directory
    echo.
    echo Press any key to exit...
    pause >nul
    exit /b 1
)

echo.
echo Simple analysis completed successfully!

:success
echo.
echo ===================================================
echo SUCCESS! Analysis completed!
echo ===================================================
echo.

REM Show generated files
echo Generated files:
if exist "COVID_ARIMA_Results_*.RData" (
    for %%f in (COVID_ARIMA_Results_*.RData) do echo - %%f
)
if exist "COVID_Forecast_*.csv" (
    for %%f in (COVID_Forecast_*.csv) do echo - %%f
)
if exist "COVID_Results_*.RData" (
    for %%f in (COVID_Results_*.RData) do echo - %%f
)
if exist "COVID19_Analysis_Plots_*.pdf" (
    for %%f in (COVID19_Analysis_Plots_*.pdf) do echo - %%f
)
if exist "COVID19_Simple_Analysis_*.pdf" (
    for %%f in (COVID19_Simple_Analysis_*.pdf) do echo - %%f
)

echo.
echo What was accomplished:
echo - Processed COVID-19 time series data
echo - Fitted optimal ARIMA model with diagnostics
echo - Generated 30-day forecast with confidence intervals
echo - Created comprehensive plots and saved to PDF
echo - Saved results for future use
echo.

echo Analysis type used:
if "%choice%"=="2" (
    echo Simple Analysis (base R only)
) else (
    echo Complete Analysis attempted
)
echo.

set /p "settings=Change analysis settings for different country/metric? (y/n): "
if /i "%settings%" equ "y" (
    echo.
    echo To change settings:
    echo.
    echo For Complete Analysis:
    echo 1. Edit COVID19_UNIFIED_ANALYSIS.R
    echo 2. Find CONFIG section (around line 25)
    echo 3. Change: target_country, target_metric, forecast_horizon
    echo.
    echo For Simple Analysis:
    echo 1. Edit SIMPLE_ANALYSIS.R
    echo 2. Find lines 8-11
    echo 3. Change: target_country, target_metric, forecast_days
    echo.
    set /p "edit=Open a script for editing? (1=Complete, 2=Simple, n=No): "
    if /i "!edit!"=="1" (
        notepad COVID19_UNIFIED_ANALYSIS.R
    )
    if /i "!edit!"=="2" (
        notepad SIMPLE_ANALYSIS.R
    )
)

echo.
echo ===================================================
echo TROUBLESHOOTING PACKAGE ISSUES
echo ===================================================
echo.
echo If the complete analysis failed due to packages:
echo.
echo Solution 1 - Install packages manually:
echo 1. Open R or RStudio
echo 2. Run: install.packages(c("forecast", "tseries"))
echo 3. Wait for installation to complete
echo 4. Run this batch file again
echo.
echo Solution 2 - Use different repository:
echo 1. Open R or RStudio
echo 2. Run: install.packages("forecast", repos="https://cloud.r-project.org/")
echo 3. Run this batch file again
echo.
echo Solution 3 - Check internet/firewall:
echo 1. Ensure internet connection is stable
echo 2. Check if firewall/antivirus blocks R
echo 3. Try from a different network
echo.
echo The simple analysis works without any packages!
echo.

echo Thank you for using the COVID-19 ARIMA Analysis Tool!
echo Double-click this batch file anytime to run the analysis.
echo.
echo Press any key to exit...
pause >nul
