package it.unipi.dsmt.student_platform.utility;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class AccessController {
	
	public static boolean checkAccess (@NotNull HttpServletRequest request,
	                                   @NotNull HttpServletResponse response,
	                                   @NotNull UserRole requiredRole) throws IOException
	{
		if (requiredRole == UserRole.invalid) {
			throw new IllegalArgumentException("Invalid required role");
		}
		
		LoggedUserDTO loggedUser = (LoggedUserDTO) request.getSession().getAttribute("loggedUser");
		if (loggedUser == null) {
			// User not logged
			UserRedirection.redirectToLogin(request, response);
			return false;
		}
		
		if (loggedUser.getRole() != requiredRole) {
			// User not allowed to access
			UserRedirection.redirectToPortalPage(request, response, loggedUser.getRole());
            return false;
		}
		
		return true;
	}
	
}
