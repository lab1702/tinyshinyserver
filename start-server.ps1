# PowerShell script to find Rscript.exe and start the Tiny Shiny Server
# Usage: .\start-server.ps1

Write-Host "Starting Tiny Shiny Server..." -ForegroundColor Green

# Function to find Rscript.exe
function Find-Rscript {
    # Common R installation paths on Windows
    $commonPaths = @(
        "${env:ProgramFiles}\R\R-*\bin\Rscript.exe",
        "${env:ProgramFiles(x86)}\R\R-*\bin\Rscript.exe",
        "${env:LOCALAPPDATA}\Programs\R\R-*\bin\Rscript.exe",
        "${env:APPDATA}\R\R-*\bin\Rscript.exe"
    )
    
    # First, try to find Rscript in PATH
    try {
        $rscriptInPath = Get-Command "Rscript.exe" -ErrorAction Stop
        Write-Host "Found Rscript.exe in PATH: $($rscriptInPath.Source)" -ForegroundColor Yellow
        return $rscriptInPath.Source
    }
    catch {
        Write-Host "Rscript.exe not found in PATH, searching common installation directories..." -ForegroundColor Yellow
    }
    
    # Search common installation paths
    foreach ($path in $commonPaths) {
        $found = Get-ChildItem -Path $path -ErrorAction SilentlyContinue | Sort-Object Name -Descending | Select-Object -First 1
        if ($found) {
            Write-Host "Found Rscript.exe: $($found.FullName)" -ForegroundColor Yellow
            return $found.FullName
        }
    }
    
    # If not found, try registry lookup
    try {
        $rKey = Get-ItemProperty -Path "HKLM:\SOFTWARE\R-core\R" -ErrorAction Stop
        $rVersion = $rKey.PSChildName | Sort-Object -Descending | Select-Object -First 1
        $rPath = (Get-ItemProperty -Path "HKLM:\SOFTWARE\R-core\R\$rVersion").InstallPath
        $rscriptPath = Join-Path $rPath "bin\Rscript.exe"
        
        if (Test-Path $rscriptPath) {
            Write-Host "Found Rscript.exe via registry: $rscriptPath" -ForegroundColor Yellow
            return $rscriptPath
        }
    }
    catch {
        # Registry lookup failed, continue with other methods
    }
    
    return $null
}

# Find Rscript.exe
$rscriptPath = Find-Rscript

if (-not $rscriptPath) {
    Write-Host "ERROR: Rscript.exe not found!" -ForegroundColor Red
    Write-Host "Please ensure R is installed on your system." -ForegroundColor Red
    Write-Host "You can download R from: https://cran.r-project.org/bin/windows/base/" -ForegroundColor Yellow
    exit 1
}

# Check if tiny_shiny_server.R exists
if (-not (Test-Path "tiny_shiny_server.R")) {
    Write-Host "ERROR: tiny_shiny_server.R not found in current directory!" -ForegroundColor Red
    Write-Host "Please ensure you're running this script from the tiny-shiny-server directory." -ForegroundColor Red
    exit 1
}

# Check if config.json exists
if (-not (Test-Path "config.json")) {
    Write-Host "WARNING: config.json not found!" -ForegroundColor Yellow
    Write-Host "The server may not start properly without configuration." -ForegroundColor Yellow
}

Write-Host "Starting server with: $rscriptPath" -ForegroundColor Green
Write-Host "Press Ctrl+C to stop the server" -ForegroundColor Cyan

# Start the server
try {
    & $rscriptPath "tiny_shiny_server.R"
}
catch {
    Write-Host "ERROR: Failed to start server: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}