% DYNAMIC DEFINITIONS
:- dynamic (capacity/2).	
:- dynamic (has/2).
:- dynamic (access/2).
:- dynamic (instructor/2).
:- dynamic (needs/2).
:- dynamic (c_needs/2).
:- dynamic (course_capacity/2).
:- dynamic (course_room/2).
:- dynamic (course_time/2).
:- dynamic (enroll/2).
:- dynamic (health/2).


% ROOM FACTS
capacity(z06, 15).
capacity(z10, 10).
capacity(z11, 12).

has(z06, nothing).
has(z06, projector).
has(z06, smart_board).
has(z10, projector).
has(z10, nothing).
has(z11, smart_board).
has(z11, nothing).

access(z06, not_accessible).
access(z10, accessible).
access(z11, accessible).
%----------------------------

% COURSE & INSTRUCTOR FACTS
instructor(101, mehmet).
instructor(102, yakup).
instructor(241, yusuf).
instructor(222, fatih).
instructor(341, yakup).

needs(mehmet, projector).
needs(yusuf, smart_board).
needs(yakup, smart_board).

c_needs(101, projector).
c_needs(102, smart_board).
c_needs(241, smart_board).
c_needs(341, smart_board).

course_capacity(101, 7).
course_capacity(102, 10).
course_capacity(241, 9).
course_capacity(222, 8).
course_capacity(341, 13).

course_room(101, z10).
course_room(102, z06).
course_room(241, z11).
course_room(222, z11).
course_room(341, z06).

course_time(101, [13, 14]).
course_time(102, [10, 11]).
course_time(241, [9, 10]).
course_time(222, [15]).
course_time(341, [13, 14]).
%--------------------------------

% STUDENT FACTS
enroll(a, [241, 341]).
enroll(b, [222]).
enroll(c, [102]).
enroll(d, [101]).
enroll(e, [241]).
enroll(f, [341]).
enroll(g, [101, 102]).

health(a, healthy).
health(b, healthy).
health(c, handicapped).
health(d, healthy).
health(e, handicapped).
health(f, healthy).
%-------------------------------

% RULES
scheduling_conflict :-			%Checks is there any conflict between courses
	room_conflict(C1, C2),		%room and time. If there is both room and time
	time_conflict(C1, C2).		%conflict, there is real conflict.

room_conflict(Course1, Course2) :-	%Checks is there any room conflict between
	Course1 \== Course2,			%given courses
	course_room(Course1, Class1),
	course_room(Course2, Class2),
	Class1 == Class2.

time_conflict(C1, C2) :-			%Checks is there any time conflict between
	C1 \== C2,						%given courses
	course_time(C1, TimeList1),
	course_time(C2, TimeList2),
	check_conflict(TimeList1, TimeList2).

check_conflict(TimeList1, TimeList2) :-	%Checks time conflict. Course times is a
	member(E,TimeList1),				%list, so if one of the lists elements is
	memberchk(E,TimeList2),!.			%common, there is time conflict.


fit_room(Course) :-						%Finds the suitable class for given course
	is_meet_class_needs(Course, Room),	
	is_meet_inst_needs(Course, Room),	%Checks if room meets courses needs.
	is_meet_capacity(Course, Room),
	write(Room),nl.						%If so, print the room name.						

room_class_pairs :-
	is_meet_class_needs(Course, Room),	%Finds all Course&Room pairs which
	is_meet_inst_needs(Course, Room),	%fits each other
	is_meet_capacity(Course, Room),
	write(Course),
	write("=>"),
	write(Room),nl.

is_meet_class_needs(Course, Room) :-	%Checks if room meets courses needs.
	c_needs(Course, Needs),
	has(Room, Has),
	Needs == Has.

is_meet_inst_needs(Course, Room) :-		%Checks if room meets instructors needs.
	instructor(Course, Inst),
	needs(Inst, Needs),
	has(Room, Has),
	Needs == Has.

is_meet_capacity(Course, Room) :-		%Checks if room's capacity equal or bigger
	capacity(Room, RoomCap),			%than course's capacity.
	course_capacity(Course, CourseCap),
	RoomCap >= CourseCap.


can_be_enrolled(Student, Course1) :-	%Checks if given student can be enrolled
	health(Student, H),					%given course.
	course_room(Course1, Room),
	access(Room, A),					
	can_access(H, A),
	enroll(Student, TakenCourses),
	member(Course2, TakenCourses),
	\+ (time_conflict(Course1, Course2)).


can_access(handicapped, Accesibility) :-	%If student handicapped, room must be
	Accesibility == accessible.				%accessible.

can_access(healthy, _) :-					%Else, no matter.
	true.


fit_courses(Student) :-					%Finds fit courses for given student.
	health(Student, H),
	course_room(Course1, Room),
	access(Room, A),
	can_access(H, A),
	enroll(Student, TakenCourses),
	member(Course2, TakenCourses),
	Course1 =\= Course2,
	\+ (time_conflict(Course1, Course2)),
	write(Course1),nl.


add_student :-		%Adds student to the system.
	write("Enter students name: "),
	read(Name),nl,
	write("Enter courses name that student enroll: "),
	read(Course),nl,
	write("Is student handicapped(handicapped or healthy): "),
	read(Handicap),nl,

	assertz(health(Name, Handicap)),	%Health fact of student added.
	can_be_enrolled(Name, Course),		%Adds enroll fact of student if and only If 
	
	course_room(Course, Room),
	access(Room, A),					
	can_access(Handicap, A),

	assertz(enroll(Name,[Course])).		%student can be enrolled to a given course,
	

add_course :-		%Adds course to the system
	write("Enter course name: "),
	read(Cname),nl,
	write("Enter courses instructor: "),
	read(Cinstructor),nl,
	write("What is needed for the course(projector, smart_board or nothing):"),
	read(Needed),nl,
	write("What is the capacity of the course: "),
	read(Cap),nl,
	write("Enter courses room: "),
	read(Room),nl,
	write("Enter course time: "),
	read(Time),nl,
	
	assertz(course_capacity(Cname, Cap)),	%Capacity fact of course added
	assertz(c_needs(Cname, Needed)),		%Needs fact of course added

	is_meet_capacity(Cname, Room),			%Checks if given room meets given 
	is_meet_class_needs(Cname, Room),		%courses capacity and needs.

	course_room(Course1, Room),				%Checks if there any scheduling conflict 
	course_time(Course1, TimeList1),		%between added course and room's schedule
	\+ check_conflict([Time], TimeList1),

	instructor(Course2, Cinstructor),		%Checks if there any scheduling conflict
	course_time(Course2, TimeList2),		%on instructor's schedule
	\+ check_conflict([Time], TimeList2),

	assertz(course_room(Cname, Room)),			%If there is no conflict, entered facts
	assertz(instructor(Cname, Cinstructor)),	%added to the system
	assertz(course_time(Cname, [Time])).

add_room :-			%Adds room to the system
	write("Enter room name: "),
	read(Rname),nl,
	write("Enter room capacity: "),
	read(Cap),nl,
	write("What does the room have(projector or smart_board): "),
	read(Have),nl,
	write("Is room accessible for handicapped students(accessible or not_accessible: "),
	read(Accessibility),nl,

	assertz(capacity(Rname, Cap)),			%Capacity fact of room added
	assertz(has(Rname, Have)),				%Has fact of room added
	assertz(access(Rname, Accessibility)).	%Access fact of room added