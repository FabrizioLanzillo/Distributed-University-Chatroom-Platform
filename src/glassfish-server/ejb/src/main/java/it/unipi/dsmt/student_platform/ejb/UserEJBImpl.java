package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.dto.LoginInformationDTO;
import it.unipi.dsmt.student_platform.dto.UserDTO;
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

@Stateless
public class UserEJBImpl implements UserEJB {
	
	@Resource(lookup = "jdbc/StudentPlatformPool")
	private DataSource dataSource;
	
	@Override
	public @Nullable LoggedUserDTO login (@NotNull LoginInformationDTO loginInformation) {
		
		try (Connection connection = dataSource.getConnection()) {
			// Check if username and password is correct
			String query = "SELECT `id` FROM ? WHERE `username` = ? AND `password` = ?";
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, loginInformation.getRole().name());
				preparedStatement.setString(2, loginInformation.getUsername());
				preparedStatement.setString(3, loginInformation.getPassword());
				
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

	public List<UserDTO> getUsers(String entered_string, UserRole role){
		List<UserDTO> users = new ArrayList<>();
		try (Connection connection = dataSource.getConnection()) {
			String query = "SELECT `id`, `username`, `email`, `name`, `surname` FROM ? WHERE `username` LIKE ? ;";

			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Look in the correct table
				if (role == UserRole.student) {
					preparedStatement.setString(1, "student");
				} else {
					preparedStatement.setString(1, "professor");
				}
				preparedStatement.setString(2, "%" + entered_string + "%");

				// Execute query
				try (ResultSet resultSet = preparedStatement.executeQuery()) {
					// If the query returned some results, then the login is successful!
					while (resultSet.next()) {
						UserDTO usr = new UserDTO();
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
            String query = "DELETE FROM ? WHERE `id` = ?;";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Look in the correct table
				if (role == UserRole.student) {
					preparedStatement.setString(1, "student");
				} else {
					preparedStatement.setString(1, "professor");
				}
                preparedStatement.setString(2, id);

                // Execute query
                int ret = preparedStatement.executeUpdate();
				return ret == 1;
			}
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
	}
	
}
