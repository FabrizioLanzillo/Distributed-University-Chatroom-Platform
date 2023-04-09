- module(mnesiaDB_handler).


-include("chat.hrl").
-export([mnesia_start/0, add_student/2, get_online_students_for_chatroom/1]).

-record(online_students, {course_name, student_pid}).

mnesia_start() ->
	% Create mnesia schema if doesn't exists
	result = mnesia:create_schema([node(), 'TODO']), %TODO
	mnesia:start(),
	case result of
		ok->
			io:format("[MNESIA] Mnesia DB started ~n"),
			% Create table
			mnesia:create_table(courses, [{type, bag}, {ram_copies, [node(), 'TODO']}]); %%TODO
			
		_ ->
			%Print error
			io:format("~p~n", [result])
	end,
	mnesia:info().

add_student(Course, OnlineStudentPid) ->

	A = fun() ->
		io:format("[MNESIA] Check if the pid of the student: ~s is already in a chatroom ~n", [OnlineStudentPid]), %TODO controllare che tipo è il pid
		NewStudent = #online_students{course_name='$1', student_pid = '$2'},
		Guard = {'==', '$2', Online_student_pid},
		NewStudentCheck = mnesia:select(online_students, [{NewStudent, [Guard], [['$1', '$2']]}]),

		io:format("[MNESIA] Check result: ~p~n", [NewStudentCheck]),
		case NewStudentCheck == [] of
			true ->		io:format(" [MNESIA] Student not in any chatrooms, student can be join this chat ~n"),
						mnesia:write(#online_students{course_name=Course, student_pid = OnlineStudentPid});

			false ->	io:format(" [MNESIA] Student is already in a chatroom ~n"),
						false
		end
	end,

	Return = mnesia:transaction(A),
		io:format(" [MNESIA] Chatroom student join response: ~p~n",[Return]),
	Return.

get_online_students_for_chatroom(Course) ->
	
	G = fun() ->
		io:format("[MNESIA] Get all the online students for the course: ~s~n", [Course]),
		OnlineUser = #online_students{course_name='$1', student_pid = '$2'},
		Guard = {'==', '$1', Course},
		mnesia:select(online_students, [{OnlineUser, [Guard], [['$1', '$2']]}])
	end,
	
	mnesia:transaction(G).
