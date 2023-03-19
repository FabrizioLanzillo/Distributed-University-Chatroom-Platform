package it.unipi.dsmt.student_platform.utility;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class AccessController {
	
	/**
	 * Checks if the logged user is allowed to access the requested resource.
	 * It redirects the user to its portal page if the user is not allowed or
	 * to the login page if they are not logged.
	 *
	 * @param request HttpServletRequest object
	 * @param response HttpServletResponse object
	 * @param requiredRole required role to access the requested resource
	 * @return true if the user is allowed to access, false otherwise
	 * @throws IOException if redirecting fails
	 */
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
			ClientRedirector.redirectToLogin(request, response);
			return false;
		}
		
		if (loggedUser.getRole() != requiredRole) {
			// User not allowed to access
			ClientRedirector.redirectToPortalPage(request, response, loggedUser.getRole());
            return false;
		}
		
		return true;
	}
	
}
