package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.dto.LoginInformationDTO;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.sql.DataSource;
import java.sql.*;

@Stateless
public class UserEJBImpl implements UserEJB {
	
//	@Resource(lookup = "jdbc/StudentPlatformPool")
	private DataSource dataSource;
	
	private @Nullable LoggedUserDTO loginTodo (@NotNull LoginInformationDTO loginInformation) { // todo change name
		
		try (Connection connection = dataSource.getConnection()) {
			// Check if username, password and role is correct
			String query = "SELECT id FROM user WHERE username = ? AND password = ? AND role = ?";
			
			try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
				// Set parameters in prepared statement
				preparedStatement.setString(1, loginInformation.getUsername());
				preparedStatement.setString(2, loginInformation.getPassword());
				preparedStatement.setString(3, loginInformation.getRole().name());
				
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
	
	@Override
	public LoggedUserDTO login (@NotNull LoginInformationDTO loginInformation) { // todo remove
		return new LoggedUserDTO(
				"1",
				loginInformation.getUsername(),
				loginInformation.getRole()
		);
	}
	
}
