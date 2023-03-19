package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.CourseCreationDTO;
import it.unipi.dsmt.student_platform.dto.CourseDTO;
import it.unipi.dsmt.student_platform.dto.ProfessorDTO;
import it.unipi.dsmt.student_platform.interfaces.CourseEJB;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

@Stateless
public class CourseEJBImpl implements CourseEJB {
	
	@Resource(lookup = "jdbc/StudentPlatformPool")
	private DataSource dataSource;
	
	@Override
	public CourseDTO getCourse (int id) {
		try (Connection connection = dataSource.getConnection()) {
			// Get details of requested course
			String query = "SELECT c.id, c.name, c.description, p.id, p.name, p.surname " +
					"FROM course c " +
					"INNER JOIN professor p on c.professor = p.id " +
					"WHERE c.id = ?;";
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setInt(1, id);
				
				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned a result set,
					// then wrap it inside a CourseDTO object and return it
					if (resultSet.next()) {
						return new CourseDTO(
								resultSet.getInt("c.id"),
                                resultSet.getString("c.name"),
                                new ProfessorDTO(
										resultSet.getString("p.id"),
                                        resultSet.getString("p.name"),
                                        resultSet.getString("p.surname")
                                ),
                                resultSet.getString("c.description")
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
	public boolean createCourse(@NotNull CourseCreationDTO course) {
		try (Connection connection = dataSource.getConnection()) {
			// Check if username and password is correct
			String query = "INSERT INTO `course` (`id`, `name`, `professor`, `description`) " +
					"VALUES (UUID_TO_BIN(UUID()), ?, ?, ?);";
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, course.getName());
				preparedStatement.setString(2, course.getProfessorId());
				preparedStatement.setString(3, course.getDescription());
				
				// Execute query
				return preparedStatement.execute();
			}
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}
	
}
