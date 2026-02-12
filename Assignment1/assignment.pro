% ----------------------------------------------------------------------
% UNIVERSITY MANAGEMENT SYSTEM - PROLOG + MYSQL (XAMPP)
% Complete implementation with ODBC connection
% ----------------------------------------------------------------------

:- use_module(library(odbc)).
:- use_module(library(lists)).
:- use_module(library(csv)).

% Declare dynamic predicates
:- dynamic student/2.
:- dynamic enrollment/3.
:- dynamic prerequisite/2.
:- dynamic required_course/1.
:- dynamic connection/1.

% ----------------------------------------------------------------------
% DATABASE CONNECTION
% ----------------------------------------------------------------------

% Connect to MySQL via ODBC
connect :-
    writeln('Connecting to MySQL database...'),
    (   odbc_connect('UniversityDB', Conn,
                    [ user(root),
                      password(''),
                      alias(university),
                      open(once)
                    ])
    ->  retractall(connection(_)),
        assert(connection(Conn)),
        writeln('Connected to MySQL successfully!'),
        true
    ;   writeln('Connection failed! Check ODBC setup.'),
        false
    ).

% Disconnect from MySQL
disconnect :-
    connection(Conn),
    odbc_disconnect(Conn),
    retractall(connection(_)),
    writeln('Disconnected from MySQL.').

% ----------------------------------------------------------------------
% LOAD DATA FROM MYSQL
% ----------------------------------------------------------------------

% Load all students
load_students :-
    connection(Conn),
    writeln('   Loading students...'),
    findall(row(ID, Name), 
            odbc_query(Conn, 
                      'SELECT student_id, student_name FROM students',
                      row(ID, Name)),
            Rows),
    forall(member(row(ID, Name), Rows),
           assertz(student(ID, Name))),
    length(Rows, Count),
    format('       Loaded ~d students~n', [Count]).

% Load all enrollments with grades
load_enrollments :-
    connection(Conn),
    writeln('   Loading enrollments...'),
    findall(row(SID, Code, Grade),
            odbc_query(Conn,
                      'SELECT student_id, course_code, grade FROM enrollments',
                      row(SID, Code, Grade)),
            Rows),
    forall(member(row(SID, Code, Grade), Rows),
           assertz(enrollment(SID, Code, Grade))),
    length(Rows, Count),
    format('       Loaded ~d enrollment records~n', [Count]).

% Load all prerequisites
load_prerequisites :-
    connection(Conn),
    writeln('   Loading prerequisites...'),
    findall(row(Course, Prereq),
            odbc_query(Conn,
                      'SELECT course_code, prerequisite_code FROM prerequisites',
                      row(Course, Prereq)),
            Rows),
    forall(member(row(Course, Prereq), Rows),
           assertz(prerequisite(Course, Prereq))),
    length(Rows, Count),
    format('       Loaded ~d prerequisite rules~n', [Count]).

% Load graduation requirements
load_requirements :-
    connection(Conn),
    writeln('   Loading graduation requirements...'),
    findall(row(Course),
            odbc_query(Conn,
                      'SELECT course_code FROM graduation_requirements WHERE program = \'Computer Science\'',
                      row(Course)),
            Rows),
    forall(member(row(Course), Rows),
           assertz(required_course(Course))),
    length(Rows, Count),
    format('       Loaded ~d graduation requirements~n', [Count]).

% Load ALL data from MySQL
load_all :-
    retractall(student(_,_)),
    retractall(enrollment(_,_,_)),
    retractall(prerequisite(_,_)),
    retractall(required_course(_)),
    load_students,
    load_enrollments,
    load_prerequisites,
    load_requirements,
    writeln('All data loaded successfully!').

% ----------------------------------------------------------------------
% BUSINESS LOGIC RULES
% ----------------------------------------------------------------------

% Student completed a course
completed(StudentID, CourseCode) :-
    enrollment(StudentID, CourseCode, _).

% Get student's grade
grade(StudentID, CourseCode, Grade) :-
    enrollment(StudentID, CourseCode, Grade).

% Calculate student's average grade
student_average(StudentID, Average) :-
    findall(Grade, enrollment(StudentID, _, Grade), Grades),
    Grades \= [],
    sum_list(Grades, Sum),
    length(Grades, Count),
    Average is Sum / Count.

% Students with distinction (average > 80)
distinction_student(StudentID, Name, Average) :-
    student(StudentID, Name),
    student_average(StudentID, Average),
    Average > 80.

% Check if student has completed ALL prerequisites for a course
has_prerequisites(StudentID, Course) :-
    \+ (prerequisite(Course, Prereq),
        \+ completed(StudentID, Prereq)).

% Check if student is ELIGIBLE to enroll in a course
eligible(StudentID, Course) :-
    student(StudentID, _),
    has_prerequisites(StudentID, Course),
    \+ completed(StudentID, Course).

% RECOMMEND courses for a student
recommend(StudentID, Course) :-
    student(StudentID, _),
    course_exists(Course),
    has_prerequisites(StudentID, Course),
    \+ completed(StudentID, Course).

% All possible courses
course_exists(Course) :-
    prerequisite(Course, _).
course_exists(Course) :-
    prerequisite(_, Course).
course_exists(Course) :-
    required_course(Course).

% Check GRADUATION eligibility
can_graduate(StudentID) :-
    student(StudentID, Name),
    forall(required_course(C), completed(StudentID, C)),
    format('     ~w (~w) is ELIGIBLE for graduation!~n', [Name, StudentID]).

can_graduate(StudentID) :-
    student(StudentID, Name),
    \+ forall(required_course(C), completed(StudentID, C)),
    format('     ~w (~w) is NOT eligible for graduation~n', [Name, StudentID]),
    missing_courses(StudentID, Missing),
    format('      Missing: ~w~n', [Missing]).

% Find missing courses for graduation
missing_courses(StudentID, MissingList) :-
    findall(C, (required_course(C), \+ completed(StudentID, C)), MissingList).

% Find top performing student
top_student(StudentID, Name, MaxAvg) :-
    findall(Avg, (student(ID, _), student_average(ID, Avg)), Averages),
    max_list(Averages, MaxAvg),
    student_average(StudentID, MaxAvg),
    student(StudentID, Name).

% ----------------------------------------------------------------------
% DISPLAY FUNCTIONS
% ----------------------------------------------------------------------

% List all students
list_students :-
    writeln('==========================================================='),
    writeln('           STUDENT ROSTER'),
    writeln('==========================================================='),
    findall((ID, Name), student(ID, Name), AllStudents),
    sort(AllStudents, Sorted),
    forall(member((ID, Name), Sorted),
           format('   ~w : ~w~n', [ID, Name])),
    length(Sorted, Count),
    format('===========================================================~n   Total: ~d students~n', [Count]),
    nl.

% Show student's enrollments
show_enrollments :-
    writeln('==========================================================='),
    writeln('           ALL STUDENT ENROLLMENTS'),
    writeln('==========================================================='),
    forall(student(StudentID, Name),
           (   format('~n    ~w (ID: ~w)~n', [Name, StudentID]),
               findall((Code, Grade), grade(StudentID, Code, Grade), Courses),
               (   Courses = []
               ->  writeln('      No enrollments found.')
               ;   forall(member((Code, Grade), Courses),
                          format('       ~w: ~d~n', [Code, Grade])),
                   student_average(StudentID, Avg),
                   format('~n      AVERAGE: ~2f~n', [Avg])
               )
           )),
    writeln('==========================================================='),
    nl.

% Show recommendations for a student - NO DUPLICATES
show_recommendations :-
    writeln('==========================================================='),
    writeln('         COURSE RECOMMENDATIONS FOR ALL STUDENTS'),
    writeln('==========================================================='),
    forall(student(StudentID, StudentName),
           (   format('~n    ~w (ID: ~w):~n', [StudentName, StudentID]),
               (   setof(Course, recommend(StudentID, Course), Recommendations)
               ->  forall(member(Course, Recommendations),
                          format('       ~w~n', [Course]))
               ;   writeln('      No recommendations available.')
               )
           )),
    writeln('==========================================================='),
    nl.
% ----------------------------------------------------------------------
% EXPORT TO CSV (For Haskell/Python)
% ----------------------------------------------------------------------

% Export enrollments to CSV
export_to_csv :-
    tell('students_export.csv'),
    write('student_id,student_name,course_code,grade'), nl,
    forall(enrollment(SID, Code, Grade),
           (student(SID, Name),
            format('~w,~w,~w,~w~n', [SID, Name, Code, Grade]))),
    told,
    writeln('  Data exported to students_export.csv for Haskell/Python'),
    nl.

% ----------------------------------------------------------------------
% MAIN DEMO
% ----------------------------------------------------------------------
demo :-
    writeln(''),
    writeln('==========================================================='),
    writeln('     UNIVERSITY MANAGEMENT SYSTEM - PROLOG + MYSQL'),
    writeln('==========================================================='),
    nl,
    
    % Connect and load data
    (   connect,
        load_all
    ->  writeln(' System ready!'),
        nl
    ;   writeln(' Failed to connect. Check ODBC setup.'),
        false
    ),
    
    % Display all students
    list_students,  
    
    % Show all student enrollments
    show_enrollments,
    
    % Show recommendations for all
    show_recommendations,
    
    % Check graduation eligibility
    writeln('==========================================================='),
    writeln('         GRADUATION ELIGIBILITY'),
    writeln('==========================================================='),
    can_graduate('S1003'), nl,  
    can_graduate('S1001'), nl,  
    can_graduate('S1006'), 
    nl,
    
    % Export data for other languages
    writeln('==========================================================='),
    writeln('         EXPORT FOR HASKELL/PYTHON'),
    writeln('==========================================================='),
    export_to_csv,
    
    % Disconnect
    disconnect,
    
    writeln('==========================================================='),
    writeln('           DEMO COMPLETED SUCCESSFULLY'),
    writeln('==========================================================='),
    !.

% ----------------------------------------------------------------------
% INDIVIDUAL QUERIES FOR TESTING
% ----------------------------------------------------------------------

% Quick test
test :-
    connect,
    load_all,
    writeln(''),
    writeln('=== TEST QUERIES ==='),
    writeln(''),
    
    writeln('1. All students:'),
    forall(student(ID, Name), format('   ~w: ~w~n', [ID, Name])),
    nl,
    
    % 2. courses - DYNAMIC NAME FROM DATABASE ✓
    StudentID = 'S1002',
    student(StudentID, StudentName),  %  GET NAME FROM DATABASE
    format('2. ~w\'s courses:~n', [StudentName]),
    forall(grade(StudentID, Code, G), 
       format('   ~w: ~d~n', [Code, G])),
    nl,
    

    StudentID = 'S1002',
    student(StudentID, StudentName),
    format('3. ~w\'s is eligible to graduate:~n', [StudentName]),
    (eligible('S1002', 'CS201') -> writeln('   YES') ; writeln('   NO')),
    nl,
    
    % 4.average
    StudentID1 = 'S1001',
    student(StudentID1, StudentName1),
    format('4. ~w\'s average:~n', [StudentName1]),
    student_average(StudentID1, Avg),

    % OPTION 1: Show as integer (no decimals) - FIXED
    RoundAvg is round(Avg),  %  EVALUATE first
    format('   ~d~n', [RoundAvg]),  %  Now pass the result

    nl,
    
    disconnect.

% Run demo automatically when file is loaded
:- writeln('========================================'),
   writeln('  PROLOG + MYSQL Ready!'),
   writeln('========================================'),
   writeln('Commands:'),
   writeln('  demo.  - Run complete demonstration'),
   writeln('  test.  - Run quick test'),
   writeln('  connect. - Connect to MySQL'),
   writeln('  list_students. - Show all students'),
   writeln('========================================'),
   nl.