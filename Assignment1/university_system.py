#!/usr/bin/env python3
"""
University Management System - Python Integration
Integrates Haskell and Prolog modules for comprehensive student analysis
"""

import subprocess
import csv
import os
from typing import List, Dict

# ============================================================================#
# CONFIGURATION
# ============================================================================#

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
HASKELL_SCRIPT = os.path.join(BASE_DIR, "studentGrade.hs")
PROLOG_SCRIPT = os.path.join(BASE_DIR, "assignment.pro")
CSV_EXPORT = "students_export.csv"

# ============================================================================#
# DATA MODELS
# ============================================================================#

class Student:
    """Student data structure matching Haskell/Prolog"""
    def __init__(self, student_id: str, name: str, grades: List[float]):
        self.student_id = student_id
        self.name = name
        self.grades = grades
    
    def average(self) -> float:
        return sum(self.grades) / len(self.grades) if self.grades else 0.0

# ============================================================================#
# HASKELL INTEGRATION
# ============================================================================#

def run_haskell_analysis() -> Dict:
    if not os.path.exists(HASKELL_SCRIPT):
        print(f"✗ Haskell file not found: {HASKELL_SCRIPT}")
        return {}
    try:
        result = subprocess.run(
            ['runhaskell', HASKELL_SCRIPT],
            capture_output=True,
            text=True,
            timeout=10
        )
        if result.returncode == 0:
            return parse_haskell_output(result.stdout)
        else:
            print(f"✗ Haskell execution failed: {result.stderr}")
            return {}
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return {}

def parse_haskell_output(output: str) -> Dict:
    results = {'averages': [], 'distinction': [], 'top_student': None}
    current_section = None
    for line in output.strip().splitlines():
        line = line.strip()
        if not line: 
            continue
        if '=== Student Average Grades ===' in line:
            current_section = 'averages'
            continue
        elif '=== Distinction Students' in line:
            current_section = 'distinction'
            continue
        elif '=== Top Performing Student ===' in line:
            current_section = 'top'
            continue

        if '(' in line and ')' in line:
            cleaned = line.strip('()').replace('"', '')
            parts = [p.strip() for p in cleaned.split(',')]
            if len(parts) != 3: 
                continue
            student_id, name, avg = parts
            record = {'id': student_id, 'name': name, 'average': float(avg)}
            if current_section == 'averages':
                results['averages'].append(record)
            elif current_section == 'distinction':
                results['distinction'].append(record)
            elif current_section == 'top':
                results['top_student'] = record
    return results

# ============================================================================#
# PROLOG INTEGRATION
# ============================================================================#

def run_prolog_demo() -> Dict:
    if not os.path.exists(PROLOG_SCRIPT):
        print(f"✗ Prolog file not found: {PROLOG_SCRIPT}")
        return {'raw_output': '', 'parsed_data': {}}
    try:
        result = subprocess.run(
            ['swipl', '-q', '-s', PROLOG_SCRIPT, '-t', 'demo'],
            capture_output=True,
            text=True,
            timeout=45,
            cwd=BASE_DIR
        )
        raw_output = result.stdout
        csv_data = parse_prolog_csv(os.path.join(BASE_DIR, CSV_EXPORT)) if os.path.exists(os.path.join(BASE_DIR, CSV_EXPORT)) else {}
        return {'raw_output': raw_output, 'parsed_data': csv_data}
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return {'raw_output': '', 'parsed_data': {}}

def parse_prolog_csv(csv_file: str) -> Dict:
    results = {'enrollments': [], 'students': {}}
    try:
        with open(csv_file, 'r') as f:
            reader = csv.DictReader(f)
            for row in reader:
                results['enrollments'].append(row)
                sid = row['student_id']
                if sid not in results['students']:
                    results['students'][sid] = {'name': row['student_name'], 'grades': []}
                try:
                    results['students'][sid]['grades'].append(float(row['grade']))
                except ValueError:
                    continue
    except FileNotFoundError:
        pass
    return results

# ============================================================================#
# PYTHON ANALYZER
# ============================================================================#

class UniversityAnalyzer:
    def __init__(self):
        self.haskell_results = {}
        self.prolog_data = {}
        self.python_students = [
            Student("S1001", "Hadif Idham", [85, 90, 78]),
            Student("S1002", "Akmal", [76, 82, 81]),
            Student("S1003", "Hezry", [92, 94, 96, 91, 89, 85, 82]),
            Student("S1004", "Hakimi", [88, 84, 85, 86, 90]),
            Student("S1006", "Faliq", [91, 89, 93, 88, 90, 87, 86, 85]),
        ]

    def integrate_all(self):
        self.haskell_results = run_haskell_analysis()
        prolog_result = run_prolog_demo()
        self.prolog_data = prolog_result.get("parsed_data", {})
        prolog_raw = prolog_result.get("raw_output", "")

        python_top = max(self.python_students, key=lambda s: s.average())
        python_dist = [
            {'name': s.name, 'id': s.student_id, 'average': round(s.average(), 2)}
            for s in self.python_students if s.average() > 80
        ]

        return {
            "python_top": {'name': python_top.name, 'id': python_top.student_id, 'average': round(python_top.average(), 2)},
            "haskell_top": self.haskell_results.get("top_student"),
            "python_distinction": python_dist,
            "haskell_distinction": self.haskell_results.get("distinction", []),
            "prolog_enrollments": self.prolog_data.get("enrollments", []),
            "prolog_raw_output": prolog_raw
        }

# ============================================================================#
# DATA EXPORT
# ============================================================================#

def export_for_haskell(students: List[Student], filename="python_export.csv"):
    with open(filename, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['student_id', 'student_name', 'grades'])
        for s in students:
            writer.writerow([s.student_id, s.name, ';'.join(str(g) for g in s.grades)])

def export_for_prolog(students: List[Student], filename="python_facts.pl"):
    with open(filename, 'w') as f:
        f.write(":- dynamic student/2.\n:- dynamic enrollment/3.\n\n")
        for s in students:
            f.write(f"student('{s.student_id}', '{s.name}').\n")
        for s in students:
            for i, g in enumerate(s.grades):
                f.write(f"enrollment('{s.student_id}', 'CS{101+i}', {g:.0f}).\n")

# ============================================================================#
# MAIN
# ============================================================================#

def main():
    analyzer = UniversityAnalyzer()
    analyzer.integrate_all()
    export_for_haskell(analyzer.python_students)
    export_for_prolog(analyzer.python_students)
    print("Integration complete: Python, Haskell, and Prolog data ready.")

if __name__ == "__main__":
    main()