package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.*;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

/**
 * EJBs responsible for handling all the business logic related to the user management.
 */
@Stateless
public class UserEJBImpl implements UserEJB {
	
	// Data source to MySQL database
	@Resource(lookup = "jdbc/StudentPlatformPool")
	private DataSource dataSource;
	
	/**
	 * Execute login procedure.
	 * @param loginInformation object containing the user's login data.
	 * @return a LoggedUserDTO object if login is successful, null otherwise
	 */
	@Override
	public @Nullable LoggedUserDTO login (@NotNull LoginInformationDTO loginInformation) {
		
		try (Connection connection = dataSource.getConnection()) {
			// Check if username and password is correct
			String query = String.format(
					"SELECT BIN_TO_UUID(id) AS id " +
						"FROM %s WHERE `username` = ? AND `password` = ?",
					loginInformation.getRole().name()
			);
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, loginInformation.getUsername());
				preparedStatement.setString(2, loginInformation.getPassword());
				
				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned some results, then the login is successful!
					if (resultSet.next()) {
						return new LoggedUserDTO(
								resultSet.getString("id"),
								loginInformation.getUsername(),
								loginInformation.getRole()
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

	public List<GeneralUserDTO> searchUsers(String entered_string, UserRole role, int index){
		List<GeneralUserDTO> users = new ArrayList<>();
		try (Connection connection = dataSource.getConnection()) {
			String tableName =
					(role == UserRole.student)
							? UserRole.student.name()
							: UserRole.professor.name();
			String whereCondition =
					(entered_string != null  && !entered_string.equals(""))
							? "WHERE `username` LIKE ?"
							: "";
			// Build query
			String query = String.format(
					"SELECT BIN_TO_UUID(`id`) as id, `username`, `email`, `name`, `surname` " +
					"FROM %s %s ORDER BY username DESC LIMIT 10 OFFSET %d;",
					tableName, whereCondition, index * 10
			);
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Look in the correct table
				if(entered_string != null && !entered_string.equals("")) {
					preparedStatement.setString(1, "%" + entered_string + "%");
				}
				
				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned some results, then the login is successful!
					while (resultSet.next()) {
						GeneralUserDTO usr = new GeneralUserDTO();
						usr.setId(resultSet.getString("id"));
						usr.setUsername(resultSet.getString("username"));
						usr.setEmail(resultSet.getString("email"));
						usr.setName(resultSet.getString("name"));
						usr.setSurname(resultSet.getString("surname"));
						users.add(usr);
					}
					return users;
				}
			}
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	public boolean banUser(String id, UserRole role) {
		try (Connection connection = dataSource.getConnection()) {
			String tableName =
					(role == UserRole.student)
							? UserRole.student.name()
							: UserRole.professor.name();
			
			String query = String.format("DELETE FROM %s WHERE `id` = UUID_TO_BIN(?);", tableName);
			
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Look in the correct table
                preparedStatement.setString(1, id);

                // Execute query
                int ret = preparedStatement.executeUpdate();
				return ret == 1;
			}
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
	}

	@Override
	public boolean createProfessorAccount(@NotNull CreateProfessorDTO createProfessorDTO){
		try(Connection connection = dataSource.getConnection()) {
			String query = "INSERT INTO professor VALUES ( UUID_TO_BIN(UUID()) ,?, ?, ?, ?, ?);";

			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, createProfessorDTO.getUsername());
				preparedStatement.setString(2, createProfessorDTO.getPassword());
				preparedStatement.setString(3, createProfessorDTO.getEmail());
				preparedStatement.setString(4, createProfessorDTO.getName());
				preparedStatement.setString(5, createProfessorDTO.getSurname());

				// Execute query
				return preparedStatement.executeUpdate() == 1;
			}
		}
		catch (SQLException e) {
			return false;
		}
	}
	
	/**
	 * definition of the signup operation. Takes a SignupDTO, perform the query and return the result.
	 * @param signupDTO: SignupDTO type which contains user data to be stored
	 * @return boolean value indicating if the signup operation was successful or not
	 */
	@Override
	public boolean signup(@NotNull SignupDTO signupDTO){
		try(Connection connection = dataSource.getConnection()) {
			//Prepare the query
			String query = "INSERT INTO `student` VALUES ( UUID_TO_BIN(UUID()) ,?, ?, ?, ?, ? ,?, ?);";
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, signupDTO.getUsername());
				preparedStatement.setString(2, signupDTO.getPassword());
				preparedStatement.setString(3, signupDTO.getEmail());
				preparedStatement.setString(4, signupDTO.getName());
				preparedStatement.setString(5, signupDTO.getSurname());
				preparedStatement.setString(6, signupDTO.getDegree());
				preparedStatement.setString(7, signupDTO.getLanguage());
				
				// Execute query
				int result = preparedStatement.executeUpdate();
				// evaluate the return value
				return result == 1;
			}
		}
		catch (SQLException e) {
			return false;
		}
	}
}
