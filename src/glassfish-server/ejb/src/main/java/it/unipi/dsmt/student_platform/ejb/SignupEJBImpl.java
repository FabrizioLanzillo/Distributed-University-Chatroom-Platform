package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.SignupDTO;
import it.unipi.dsmt.student_platform.interfaces.SignupEJB;
import jakarta.ejb.Stateless;

@Stateless
public class SignupEJBImpl implements SignupEJB {
    @Override
    public boolean signup(SignupDTO signupDTO) {
    /*      try(Connection connection = dataSource.getConnection()) {
            String query = "INSERT INTO Users (username, password, email, name, surname, degree, language, role)
                            VALUES(?, ?, ?, ?, ? ,?, ?, ?)");

                try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                    // Set parameters in prepared statement
                    preparedStatement.setString(1, signupDTO.getUsername());
                    preparedStatement.setString(2, signupDTO.getPassword());
                    preparedStatement.setString(3, signupDTO.getEmail());
                    preparedStatement.setString(4, signupDTO.getName());
                    preparedStatement.setString(5, signupDTO.getSurname());
                    preparedStatement.setString(6, signupDTO.getDegree());
                    preparedStatement.setString(7, signupDTO.getLanguage());
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