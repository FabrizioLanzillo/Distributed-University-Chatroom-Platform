package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.LoginInformationDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;

import javax.sql.DataSource;

@Stateless
public class UserEJBImpl implements UserEJB {
	
	@Resource(lookup = "jdbc/StudentPlatformPool")
	private DataSource dataSource;
	
	public LoginInformationDTO login(@NotNull String username, @NotNull String password, @NotNull UserRole role) {
		/*
		try(Connection connection = dataSource.getConnection()) {
			// Check if username, password and role is correct
			String query = "SELECT username FROM user WHERE username = ? AND password = ? AND role = ?";
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, username);
				preparedStatement.setString(2, password);
				preparedStatement.setString(3, role.name());
				// Execute query
				ResultSet resultSet = preparedStatement.executeQuery();
				// If the query returned some results, then the login is successful!
				if (resultSet.next()) {
					return new UserDTO(username, role);
				}
				else {
					return null;
				}
			}
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
		*/
		return new LoginInformationDTO(username, role); // todo
	}
	
}
