package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.*;
import it.unipi.dsmt.student_platform.interfaces.CourseEJB;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.sql.DataSource;
import java.sql.*;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

/**
 * EJB object which handles all the business logic related to the courses
 */
@Stateless
public class CourseEJBImpl implements CourseEJB {
	
	// Data source to MySQL database
	@Resource(lookup = "jdbc/StudentPlatformPool")
	private DataSource dataSource;
	
	/**
	 * Given a course id, get all the relevant details of the corresponding course
	 * and if the current user has starred it or not.
	 * @param courseId id of the course
	 * @param userId id of the current user
	 * @return a CourseDTO object with the required data on success, null otherwise
	 */
	@Override
	public @Nullable CourseDTO getCourseDetails (int courseId, String userId) {

		try (Connection connection = dataSource.getConnection()) {
			// Get details of requested course
			String query = "SELECT c.id, c.name, c.description, p.id, p.name, p.surname, ssc.student " +
					"FROM course c " +
					"inner join professor p on c.professor = p.id " +
					"left outer join student_starred_courses ssc " +
						"on c.id = ssc.course and ssc.student = UUID_TO_BIN(?) " +
					"where c.id = ?";
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, userId);
				preparedStatement.setInt(2, courseId);
				
				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned a result set,
					// then wrap it inside a CourseDTO object and return it
					if (resultSet.next()) {
						return new CourseDTO(
								resultSet.getInt("c.id"),
								resultSet.getString("c.name"),
								new ProfessorDTO(
										resultSet.getString("p.name"),
										resultSet.getString("p.surname")
								),
								resultSet.getString("c.description"),
								(resultSet.getString("ssc.student") != null)
						);
					} else {
						return null;
					}
				}
			}
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}


	@Override
	public List<MinimalCourseDTO> searchCourses (String name) {
		List<MinimalCourseDTO> courses = new ArrayList<>();

		try (Connection connection = dataSource.getConnection()) {
			// Get details of requested course
			String query =  "SELECT c.id, c.name, p.name, p.surname " +
					"FROM course c " +
					"INNER JOIN professor p on c.professor = p.id " +
					"WHERE c.name LIKE ? OR p.surname LIKE ?;";

			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, "%" + name + "%");
				preparedStatement.setString(2, "%" + name + "%");

				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned a result set,
					// then wrap it inside a CourseDTO object and return it
					while (resultSet.next()){
						courses.add(new MinimalCourseDTO(
										resultSet.getInt("c.id"),
										resultSet.getString("c.name"),
										new ProfessorDTO(
												resultSet.getString("p.name"),
												resultSet.getString("p.surname")
										)
									)
						);
					}
				}
			}
		}
		catch (SQLException e) {
			throw new RuntimeException(e);
		}
		return courses;
	}


	@Override
	public List<MinimalCourseDTO> searchCoursesForProfessor (String name, String professorId) {
		List<MinimalCourseDTO> courses = new ArrayList<>();

		try (Connection connection = dataSource.getConnection()) {
			// Get details of requested course
			String query =  "SELECT c.id, c.name, p.name, p.surname " +
					"FROM course c " +
					"INNER JOIN professor p on c.professor = p.id " +
					"WHERE c.name LIKE ? AND p.id = UUID_TO_BIN(?);";

			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, "%" + name + "%");
				preparedStatement.setString(2, professorId);

				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned a result set,
					// then wrap it inside a CourseDTO object and return it
					while (resultSet.next()){
						courses.add(new MinimalCourseDTO(
										resultSet.getInt("c.id"),
										resultSet.getString("c.name"),
										new ProfessorDTO(
												resultSet.getString("p.name"),
												resultSet.getString("p.surname")
										)
								)
						);
					}
				}
			}
		}
		catch (SQLException e) {
			throw new RuntimeException(e);
		}
		return courses;
	}


	@Override
	public List<MinimalCourseDTO> getAllCourses () {
		List<MinimalCourseDTO> courses = new ArrayList<>();

		try (Connection connection = dataSource.getConnection()) {
			// Get details of requested course
			String query =  "SELECT c.id, c.name, p.name, p.surname " +
							"FROM course c " +
							"INNER JOIN professor p on c.professor = p.id " +
							"ORDER BY c.name";

			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {

				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned a result set,
					// then wrap it inside a CourseDTO object and return it
					while (resultSet.next()){
						courses.add(new MinimalCourseDTO(
										resultSet.getInt("id"),
										resultSet.getString("name"),
										new ProfessorDTO(
												resultSet.getString("p.name"),
												resultSet.getString("p.surname")
										)
								)
						);
					}
				}
			}
		}
		catch (SQLException e) {
			throw new RuntimeException(e);
		}
		return courses;
	}


	@Override
	public List<MinimalCourseDTO> getAllCoursesForProfessor (String professorId) {
		List<MinimalCourseDTO> courses = new ArrayList<>();

		try (Connection connection = dataSource.getConnection()) {
			// Get details of requested course
			String query =  "SELECT c.id, c.name, p.name, p.surname " +
					"FROM course c " +
					"INNER JOIN professor p on c.professor = p.id " +
					"WHERE p.id = UUID_TO_BIN(?) ORDER BY c.name";

			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {

				preparedStatement.setString(1, professorId);
				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned a result set,
					// then wrap it inside a CourseDTO object and return it
					while (resultSet.next()){
						courses.add(new MinimalCourseDTO(
										resultSet.getInt("id"),
										resultSet.getString("name"),
										new ProfessorDTO(
												resultSet.getString("p.name"),
												resultSet.getString("p.surname")
										)
								)
						);
					}
				}
			}
		}
		catch (SQLException e) {
			throw new RuntimeException(e);
		}
		return courses;
	}


	@Override
	public List<MinimalCourseDTO> getStarredCourses(String id){
		List<MinimalCourseDTO> courses = new ArrayList<>();

		try (Connection connection = dataSource.getConnection()) {
			// Get details of requested course
			String query =  "SELECT c.id, c.name, p.name, p.surname " +
					"FROM student_starred_courses ssc " +
					"INNER JOIN course c on ssc.course = c.id " +
					"INNER JOIN professor p on c.professor = p.id " +
					"WHERE ssc.student = UUID_TO_BIN(?);";

			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, id);

				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned a result set,
					// then wrap it inside a CourseDTO object and return it
					while (resultSet.next()){
						courses.add(new MinimalCourseDTO(
										resultSet.getInt("c.id"),
										resultSet.getString("c.name"),
										new ProfessorDTO(
												resultSet.getString("p.name"),
												resultSet.getString("p.surname")
										)
								)
						);
					}
				}
			}
		}
		catch (SQLException e) {
			throw new RuntimeException(e);
		}
		return courses;
	}

	@Override
	public String getCourseName(int id){

		String couseName = "";

		try (Connection connection = dataSource.getConnection()) {
			// Get details of requested course
			String query =  "SELECT c.name " +
							"FROM course c  " +
							"WHERE c.id = ?;";

			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setInt(1, id);

				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					if(resultSet.next()){
						couseName = resultSet.getString("c.name");
					}
				}
			}
		}
		catch (SQLException e) {
			throw new RuntimeException(e);
		}
		return couseName;
	}
	
	/**
	 * Add a "star" relationship between a student and a course (i.e. "star the course").
	 * @param studentId id of the student
	 * @param courseId id of the course
	 * @return true on success, false otherwise
	 */
	@Override
	public boolean addStarredCourse (@NotNull String studentId, int courseId){
		try (Connection connection = dataSource.getConnection()) {
			// Add relationship between student and course
			String query = "INSERT INTO `student_starred_courses` VALUES (UUID_TO_BIN(?), ?)";

			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, studentId);
				preparedStatement.setInt(2, courseId);

				// Execute query
				return preparedStatement.executeUpdate() == 1;
			}
		} catch (SQLException e) {
			e.printStackTrace();
			return false;
		}
	}
	
	
	/**
	 * Remove a "star" relationship between a student and a course (i.e. "unstar the course").
	 * @param studentId id of the student
	 * @param courseId id of the course
	 * @return true on success, false otherwise
	 */
	@Override
	public boolean removeStarredCourse (@NotNull String studentId, int courseId) {
		try (Connection connection = dataSource.getConnection()) {
			// Remove relationship between student and course
			String query = "DELETE FROM `student_starred_courses` WHERE `student` = UUID_TO_BIN(?) AND `course` = ?";
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, studentId);
				preparedStatement.setInt(2, courseId);
				
				// Execute query
				return 1 == preparedStatement.executeUpdate();
			}
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}
	
	
	
	
	
	/**
	 * Create a new course.
	 * @param course object containing the required data to create a course
	 * @return true on success, false otherwise
	 */
	@Override
	public boolean createCourse(@NotNull CourseCreationDTO course) {
		List<MeetingSlotCreationDTO> meetingSlots = new ArrayList<>();
		// Prepare meeting slots
		for (LocalTime t = course.getStartTime(); t.isBefore(course.getEndTime()); t = t.plusMinutes(30)) {
			meetingSlots.add(
					new MeetingSlotCreationDTO(
							course.getWeekday(),
							t
					)
			);
		}
		
		try (Connection connection = dataSource.getConnection()) {
			insertCourseWithMeetingSlots(connection, course, meetingSlots);
			return true;
		} catch (SQLException e) {
			e.printStackTrace();
			return false;
		}
	}
	
	private void insertCourseWithMeetingSlots(@NotNull Connection connection,
	                                          @NotNull CourseCreationDTO course,
	                                          @NotNull List<MeetingSlotCreationDTO> meetingSlots)
			throws SQLException
	{
		// Configure connection for multiple statements
		connection.setAutoCommit(false);
		// MySQL queries
		String insertCourseQuery = "INSERT INTO `course` (`name`, `professor`, `description`) " +
				"VALUES (?, UUID_TO_BIN(?), ?);";
		String insertMeetingSlotQuery =
				"INSERT INTO `meeting_slot` VALUES (UUID_TO_BIN(UUID()), ?, ?, ?)";
		
		// Generate statements
		try (
				PreparedStatement insertCourseStatement =
						connection.prepareStatement(insertCourseQuery, Statement.RETURN_GENERATED_KEYS);
				PreparedStatement insertMeetingSlotStatement =
						connection.prepareStatement(insertMeetingSlotQuery);
			)
		{
			int courseId;
			
			// Insert course
			insertCourseStatement.setString(1, course.getName());
			insertCourseStatement.setString(2, course.getProfessorId());
			insertCourseStatement.setString(3, course.getDescription());
			int affectedRows = insertCourseStatement.executeUpdate(); // read generated course id
			if (affectedRows <= 0) {
				throw new SQLException("affectedRows == 0");
			}
			
			// Retrieve generated course id
			try (ResultSet keys = insertCourseStatement.getGeneratedKeys()) {
				if (!keys.next()) {
					throw new SQLException("Cannot retrieve generated course id");
				}
				courseId = keys.getInt(1);
			}
			
			// Insert meeting slots
			for (var slot : meetingSlots) {
				insertMeetingSlotStatement.clearParameters();
				insertMeetingSlotStatement.setInt(1, courseId);
				insertMeetingSlotStatement.setInt(2, slot.getWeekday());
				insertMeetingSlotStatement.setTime(3, Time.valueOf(slot.getTime()));
				insertMeetingSlotStatement.executeUpdate();
			}
			
			// Commit statements
			connection.commit();
		} catch (Exception exception) {
			connection.rollback();
			throw exception;
		}
	}
	
	
	
	
	@Override
	public boolean deleteCourse(int id){
		try (Connection connection = dataSource.getConnection()) {
			// Check if username and password is correct
			String query = "DELETE " +
							"FROM course " +
							"WHERE id = ?;";

			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setInt(1, id);

				// Execute query
				return preparedStatement.executeUpdate() == 1;
			}
		}
		catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}
}
