# ==============================================================
# UNIVERSITY MANAGEMENT SYSTEM (PYTHON VERSION)
# Combines: Prolog logic + Haskell grade analysis
# MySQL Database: XAMPP (phpMyAdmin)
# ==============================================================

import mysql.connector
from collections import defaultdict

# ==============================================================
# CONNECT TO MYSQL
# ==============================================================
def connect_db():
    print("Connecting to MySQL database...")
    try:
        conn = mysql.connector.connect(
            host="localhost",
            user="root",
            password="",
            database="university_db"
        )
        print("Connected successfully!\n")
        return conn
    except:
        print("Connection failed! Check XAMPP/MySQL.")
        return None

# ==============================================================
# LOAD DATA FROM MYSQL
# ==============================================================
def load_data(conn):
    cursor = conn.cursor()

    # students
    cursor.execute("SELECT student_id, student_name FROM students")
    students = cursor.fetchall()

    # enrollments
    cursor.execute("SELECT student_id, course_code, grade FROM enrollments")
    enrollments = cursor.fetchall()

    # prerequisites
    cursor.execute("SELECT course_code, prerequisite_code FROM prerequisites")
    prerequisites = cursor.fetchall()

    # graduation requirements
    cursor.execute("""
        SELECT course_code FROM graduation_requirements
        WHERE program='Computer Science'
    """)
    requirements = cursor.fetchall()

    print("Data loaded from MySQL!\n")

    return students, enrollments, prerequisites, requirements

# ==============================================================
# BUILD DATA STRUCTURE
# ==============================================================
def build_data(students, enrollments):
    student_dict = {sid: name for sid, name in students}

    grades_dict = defaultdict(list)
    for sid, course, grade in enrollments:
        grades_dict[sid].append((course, grade))

    return student_dict, grades_dict

# ==============================================================
# AVERAGE CALCULATION (HASKELL PART)
# ==============================================================
def average(grades):
    if len(grades) == 0:
        return 0
    return sum(grades) / len(grades)

def student_averages(grades_dict):
    avg_dict = {}
    for sid in grades_dict:
        grade_list = [g for c, g in grades_dict[sid]]
        avg_dict[sid] = average(grade_list)
    return avg_dict

# ==============================================================
# DISTINCTION STUDENTS (>80)
# ==============================================================
def distinction_students(student_dict, avg_dict):
    print("=== DISTINCTION STUDENTS (>80) ===")
    for sid, avg in avg_dict.items():
        if avg > 80:
            print(sid, "-", student_dict[sid], "- Avg:", round(avg,2))
    print()

# ==============================================================
# TOP STUDENT
# ==============================================================
def top_student(student_dict, avg_dict):
    top_id = max(avg_dict, key=avg_dict.get)
    print("=== TOP STUDENT ===")
    print(top_id, "-", student_dict[top_id], "- Avg:", round(avg_dict[top_id],2))
    print()

# ==============================================================
# SHOW ALL STUDENTS
# ==============================================================
def list_students(student_dict):
    print("=== STUDENT LIST ===")
    for sid, name in student_dict.items():
        print(sid, "-", name)
    print()

# ==============================================================
# SHOW ENROLLMENTS
# ==============================================================
def show_enrollments(student_dict, grades_dict):
    print("=== ALL ENROLLMENTS ===")
    for sid in student_dict:
        print("\n", student_dict[sid], "(", sid, ")")
        if sid in grades_dict:
            for course, grade in grades_dict[sid]:
                print("   ", course, ":", grade)
        else:
            print("   No courses")
    print()

# ==============================================================
# CHECK PREREQUISITES
# ==============================================================
def has_prerequisites(sid, course, prerequisites, grades_dict):
    completed_courses = [c for c, g in grades_dict.get(sid, [])]

    for c, prereq in prerequisites:
        if c == course and prereq not in completed_courses:
            return False
    return True

# ==============================================================
# COURSE RECOMMENDATION
# ==============================================================
def recommend_courses(student_dict, grades_dict, prerequisites):
    print("=== COURSE RECOMMENDATIONS ===")

    all_courses = set()
    for c, p in prerequisites:
        all_courses.add(c)
        all_courses.add(p)

    for sid in student_dict:
        print("\n", student_dict[sid], "(", sid, ")")
        completed = [c for c, g in grades_dict.get(sid, [])]

        found = False
        for course in all_courses:
            if course not in completed and has_prerequisites(sid, course, prerequisites, grades_dict):
                print("   ", course)
                found = True

        if not found:
            print("   No recommendation")

    print()

# ==============================================================
# GRADUATION CHECK
# ==============================================================
def check_graduation(student_dict, grades_dict, requirements):
    print("=== GRADUATION ELIGIBILITY ===")

    required = [r[0] for r in requirements]

    for sid in student_dict:
        completed = [c for c, g in grades_dict.get(sid, [])]

        missing = [c for c in required if c not in completed]

        if len(missing) == 0:
            print(student_dict[sid], "(", sid, ") ELIGIBLE")
        else:
            print(student_dict[sid], "(", sid, ") NOT ELIGIBLE")
            print("   Missing:", missing)

    print()

# ==============================================================
# EXPORT CSV
# ==============================================================
def export_csv(student_dict, grades_dict):
    with open("students_export.csv", "w") as f:
        f.write("student_id,student_name,course,grade\n")

        for sid in grades_dict:
            for course, grade in grades_dict[sid]:
                f.write(f"{sid},{student_dict[sid]},{course},{grade}\n")

    print("CSV exported for Python/Haskell!\n")

# ==============================================================
# MAIN PROGRAM (DEMO)
# ==============================================================
def main():
    conn = connect_db()
    if not conn:
        return

    students, enrollments, prerequisites, requirements = load_data(conn)

    student_dict, grades_dict = build_data(students, enrollments)
    avg_dict = student_averages(grades_dict)

    list_students(student_dict)
    show_enrollments(student_dict, grades_dict)

    print("=== STUDENT AVERAGES ===")
    for sid, avg in avg_dict.items():
        print(sid, "-", student_dict[sid], ":", round(avg,2))
    print()

    distinction_students(student_dict, avg_dict)
    top_student(student_dict, avg_dict)

    recommend_courses(student_dict, grades_dict, prerequisites)
    check_graduation(student_dict, grades_dict, requirements)

    export_csv(student_dict, grades_dict)

    conn.close()
    print("System finished successfully!")

# ==============================================================
# RUN
# ==============================================================
if __name__ == "__main__":
    main()