package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.CourseCreationDTO;
import it.unipi.dsmt.student_platform.dto.CourseDTO;
import it.unipi.dsmt.student_platform.dto.MinimalCourseDTO;
import it.unipi.dsmt.student_platform.dto.ProfessorDTO;
import it.unipi.dsmt.student_platform.interfaces.CourseEJB;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

@Stateless
public class CourseEJBImpl implements CourseEJB {

	@Resource(lookup = "jdbc/StudentPlatformPool")
	private DataSource dataSource;

	@Override
	public CourseDTO getCourseDetails (int courseId, String userId) {

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


	@Override
	public boolean createCourse(@NotNull CourseCreationDTO course) {
		try (Connection connection = dataSource.getConnection()) {
			// Check if username and password is correct
			String query = "INSERT INTO `course` (`name`, `professor`, `description`) " +
					"VALUES (?, UUID_TO_BIN(?), ?);";
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, course.getName());
				preparedStatement.setString(2, course.getProfessorId());
				preparedStatement.setString(3, course.getDescription());
				
				// Execute query
				return preparedStatement.executeUpdate() == 1;
				
			}
		} catch (SQLException e) {
			e.printStackTrace();
			return false;
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
