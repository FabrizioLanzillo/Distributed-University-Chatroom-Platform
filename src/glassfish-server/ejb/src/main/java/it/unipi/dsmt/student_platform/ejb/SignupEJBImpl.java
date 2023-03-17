package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.interfaces.SignupEJB;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;

@Stateless
public class SignupEJBImpl implements SignupEJB {
    @Override
    public boolean signup(@NotNull String username, @NotNull String password, @NotNull String email, @NotNull String name,
                            @NotNull String surname, @NotNull String degree, @NotNull String language) {
    /*      try(Connection connection = dataSource.getConnection()) {
            String query = "INSERT INTO Users (username, password, email, name, surname, degree, language, role)
                            VALUES(?, ?, ?, ?, ? ,?, ?, ?)");

                try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                    // Set parameters in prepared statement
                    preparedStatement.setString(1, username);
                    preparedStatement.setString(2, password);
                    preparedStatement.setString(3, email);
                    preparedStatement.setString(4, name);
                    preparedStatement.setString(5, surname);
                    preparedStatement.setString(6, degree);
                    preparedStatement.setString(7, language);
                    preparedStatement.setString(8, UserRole.student);

                    // Execute query
                    int result = preparedStatement.executeUpdate();
                    // evaluate the return value
                    if(!result)
                        return false;
                    return true;
                }
            }
            catch (SQLException e) {
                throw new RuntimeException(e);
                return false;
                }
             */
    return true;
    }
}