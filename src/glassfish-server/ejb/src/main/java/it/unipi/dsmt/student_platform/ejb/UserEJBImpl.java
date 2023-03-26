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
	
}
