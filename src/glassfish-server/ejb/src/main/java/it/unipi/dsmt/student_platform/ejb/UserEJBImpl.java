package it.unipi.dsmt.student_platform.ejb;

import it.unipi.dsmt.student_platform.dto.UserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;

@Stateless
public class UserEJBImpl implements UserEJB {
	@Override
	public UserDTO login(@NotNull String username, @NotNull String password, @NotNull UserRole role) {
		return new UserDTO(username, role);
	}
}
