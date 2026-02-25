#!/usr/bin/env python3
"""
Run University Management System Analysis and Save Logs
Run this first to generate the log file
"""

import subprocess
import datetime
import os
import sys

# Configuration
HASKELL_SCRIPT = "studentGrade.hs"
PROLOG_SCRIPT = "assignment.pro"
LOG_FILE = "analysis_log.txt"
CSV_FILE = "students_export.csv"

def run_command(cmd, description):
    """Run a command and return output"""
    print(f"Running: {description}...")
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            shell=True,
            timeout=30,
            encoding='utf-8',  # Specify UTF-8 encoding
            errors='ignore'     # Ignore characters that can't be encoded
        )
        return {
            'success': result.returncode == 0,
            'stdout': result.stdout,
            'stderr': result.stderr
        }
    except Exception as e:
        return {
            'success': False,
            'stdout': '',
            'stderr': str(e)
        }

def main():
    """Run all analyses and save to log file"""
    
    print("="*60)
    print("UNIVERSITY MANAGEMENT SYSTEM - LOG GENERATOR")
    print("="*60)
    
    # Create log content
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    log_content = []
    log_content.append("="*80)
    log_content.append("UNIVERSITY MANAGEMENT SYSTEM ANALYSIS LOG")
    log_content.append(f"Generated: {timestamp}")
    log_content.append("="*80)
    log_content.append("")
    
    # Check files exist
    log_content.append("CHECKING FILES:")
    log_content.append("-"*40)
    
    hs_exists = os.path.exists(HASKELL_SCRIPT)
    pro_exists = os.path.exists(PROLOG_SCRIPT)
    
    # Use [OK] and [FAIL] instead of Unicode characters
    log_content.append(f"Haskell script ({HASKELL_SCRIPT}): {'[OK]' if hs_exists else '[FAIL] Not found'}")
    log_content.append(f"Prolog script ({PROLOG_SCRIPT}): {'[OK]' if pro_exists else '[FAIL] Not found'}")
    log_content.append("")
    
    # 1. Run Haskell analysis
    log_content.append("="*80)
    log_content.append("HASKELL ANALYSIS")
    log_content.append("="*80)
    
    if hs_exists:
        result = run_command(f"runhaskell {HASKELL_SCRIPT}", "Haskell analysis")
        if result['success']:
            log_content.append("[OK] Haskell analysis successful")
            log_content.append("\n--- Haskell Output ---")
            log_content.append(result['stdout'])
        else:
            log_content.append("[FAIL] Haskell analysis failed")
            if result['stderr']:
                log_content.append(f"Error: {result['stderr']}")
    else:
        log_content.append("[FAIL] Cannot run Haskell - file not found")
    
    log_content.append("")
    
    # 2. Run Prolog demo
    log_content.append("="*80)
    log_content.append("PROLOG ANALYSIS")
    log_content.append("="*80)
    
    if pro_exists:
        result = run_command(f"swipl -s {PROLOG_SCRIPT} -t demo", "Prolog analysis")
        if result['success']:
            log_content.append("[OK] Prolog analysis successful")
            log_content.append("\n--- Prolog Output ---")
            log_content.append(result['stdout'])
        else:
            log_content.append("[FAIL] Prolog analysis failed")
            if result['stderr']:
                log_content.append(f"Error: {result['stderr']}")
    else:
        log_content.append("[FAIL] Cannot run Prolog - file not found")
    
    log_content.append("")
    
    # 3. Check for CSV export
    log_content.append("="*80)
    log_content.append("CSV EXPORT CHECK")
    log_content.append("="*80)
    
    if os.path.exists(CSV_FILE):
        csv_size = os.path.getsize(CSV_FILE)
        log_content.append(f"[OK] CSV file found: {CSV_FILE}")
        log_content.append(f"  Size: {csv_size} bytes")
        
        # Show first few lines of CSV
        try:
            with open(CSV_FILE, 'r', encoding='utf-8', errors='ignore') as f:
                csv_lines = f.readlines()[:5]  # First 5 lines
            log_content.append("\n--- CSV Preview (first 5 lines) ---")
            for line in csv_lines:
                log_content.append(line.strip())
        except Exception as e:
            log_content.append(f"Could not read CSV file: {e}")
    else:
        log_content.append(f"[FAIL] CSV file not found: {CSV_FILE}")
    
    log_content.append("")
    
    # 4. Summary statistics
    log_content.append("="*80)
    log_content.append("SUMMARY")
    log_content.append("="*80)
    
    log_content.append("Analysis complete!")
    log_content.append(f"Timestamp: {timestamp}")
    
    log_content.append("")
    log_content.append("="*80)
    log_content.append("END OF LOG")
    log_content.append("="*80)
    
    # Write to log file with UTF-8 encoding
    full_log = "\n".join(log_content)
    try:
        with open(LOG_FILE, 'w', encoding='utf-8') as f:
            f.write(full_log)
        print(f"\n✅ Log file created: {LOG_FILE}")
        print(f"   Location: {os.path.abspath(LOG_FILE)}")
        print(f"   Size: {len(full_log)} characters")
    except Exception as e:
        # Fallback to ASCII if UTF-8 fails
        with open(LOG_FILE, 'w', encoding='ascii', errors='ignore') as f:
            f.write(full_log)
        print(f"\n⚠️ Log file created with ASCII encoding: {LOG_FILE}")
        print(f"   Location: {os.path.abspath(LOG_FILE)}")
        print(f"   Size: {len(full_log)} characters")
        print(f"   Note: Some Unicode characters were removed")
    
    return LOG_FILE

if __name__ == "__main__":
    log_file = main()