-module(mnesia_manager).

-export([join_course/4, get_online_pid/1, get_online_students/1, logout/4, remove_logged_students_by_hostname/1]).

-record(online_students, {course_id, student_pid, student_name, hostname}).


% Add a student to a course chatroom
join_course(StudentPid, StudentName, Course, Hostname) 
		when is_pid(StudentPid), is_integer(Course) ->
	Fun = fun() ->
		io:format("[mnesia_manager] join_course => Check if the pid of the student: ~p is already in a chatroom ~n", [StudentPid]),
		NewStudent = #online_students{course_id='$1', student_pid = '$2', student_name = '$3', hostname = 'S4'},
		Guard = {'==', '$2', StudentPid},
		NewStudentCheck = mnesia:select(online_students, [{NewStudent, [Guard], ['$2']}]),

		io:format("[mnesia_manager] join_course => Check result: ~p~n", [NewStudentCheck]),
		case NewStudentCheck == [] of 
			true ->
				io:format("[mnesia_manager] join_course => Student not in any chatroom, student can be join this chat ~n"),
				mnesia:write(
					#online_students{
						course_id = Course,
						student_pid = StudentPid,
						student_name = StudentName,
						hostname = Hostname
					}
				);
		
			false ->
				io:format("[mnesia_manager] join_course => Student is already in a chatroom ~n"),
				false  
		end
	end,
	{atomic, Result} = mnesia:transaction(Fun),
	io:format("[mnesia_manager] join_course => Chatroom join request returned response: ~p~n",[Result]),
	Result.



% Get the process ids of the student belonging to a given chatroom
get_online_pid(Course) when is_integer(Course) ->
	Fun = fun() ->
		io:format("[mnesia_manager] get_online_students => Get all the online processes for the course: ~p~n", [Course]),
		OnlineUser = #online_students{course_id='$1', student_pid = '$2', student_name = '$3', hostname = '$4'},
		Guard = {'==', '$1', Course},
		mnesia:select(online_students, [{OnlineUser, [Guard], ['$2']}])
	end,
	
	{atomic, Result} = mnesia:transaction(Fun),
	io:format("[mnesia_manager] get_online_students => Online students: ~p~n", [Result]),
	Result.



% Get the usernames of the students belonging to a given chatroom
get_online_students(Course) when is_integer(Course) -> 
	Fun = fun() ->
		io:format("[mnesia_manager] get_online_students => Get all the online students for the course: ~p~n", [Course]),
		OnlineUser = #online_students{course_id='$1', student_pid = '$2', student_name = '$3', hostname = '$4'},
		Guard = {'==', '$1', Course},
		mnesia:select(online_students, [{OnlineUser, [Guard], ['$3']}])
	end,
	
	{atomic, Result} = mnesia:transaction(Fun),
	io:format("[mnesia_manager] get_online_students => Online students: ~p~n", [Result]),
	Result.



% Remove a student from a given course
logout(StudentPid, StudentName, Course, Hostname) when is_pid(StudentPid), is_integer(Course) -> 
	Fun = fun() ->
		mnesia:delete_object(
			#online_students{
				course_id = Course,
				student_pid = StudentPid,
				student_name = StudentName,		
				hostname = Hostname		
			}
		),
		io:format("[mnesia_manager] logout => Student ~p logout from the chatroom ~p~n", [StudentPid, Course])
	end,

	{atomic, Result} = mnesia:transaction(Fun),
	io:format("[mnesia_manager] logout => transaction result is ~p~n", [Result]),
	Result.



% Remove all the students logged in this server
remove_logged_students_by_hostname(Hostname) ->
	Fun = fun() ->
		io:format("[mnesia_manager] remove_logged_students_by_hostname => Remove all students which logged in host ~p~n", [Hostname]),
		OnlineUser = #online_students{course_id='$1', student_pid = '$2', student_name = '$3', hostname = '$4'},
		Guard = {'==', '$4', Hostname},
		RecordList = mnesia:select(online_students, [{OnlineUser, [Guard], ['$_']}]),
		
		delete_by_hostname(RecordList)
	end,
	{atomic, ok} = mnesia:transaction(Fun),
	io:format("[mnesia_manager] remove_logged_students_by_hostname => transaction succeeded"),
	ok.



% Recursively remove all users connected to the chatroom through the crashed erlang node
delete_by_hostname([]) ->
	ok;

delete_by_hostname([{_, Course, StudentPid, StudentName, Hostname} | T]) ->
	mnesia:delete_object(
		#online_students{
			course_id = Course,
			student_pid = StudentPid,
			student_name = StudentName,		
			hostname = Hostname		
		}
	),
	io:format("[mnesia_manager] delete_by_hostname => Student ~p logout from the chatroom ~p~n", [StudentName, Course]),
	delete_by_hostname(T).

