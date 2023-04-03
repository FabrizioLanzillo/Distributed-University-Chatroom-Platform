package it.unipi.dsmt.student_platform.utility;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;

public class AccessController {
	
	private static final String LOGGED_USER_ATTRIBUTE = "logged_user";
	
	/**
	 * Checks if the logged user is allowed to access the requested resource.
	 * <br>
	 * It redirects the user to its portal page if the user is not allowed or
	 * to the login page if they are not logged.
	 * In these cases, the method returns null.
	 *
	 * @param request HttpServletRequest object
	 * @param response HttpServletResponse object
	 * @param requiredRole required role to access the requested resource
	 * @return stored instance of LoggedUserDTO if the user is logged AND
	 * they're allowed to access the requested resource, null otherwise
	 * @throws IOException if redirecting fails
	 */
	public static @Nullable LoggedUserDTO checkAccess (
			@NotNull HttpServletRequest request,
			@NotNull HttpServletResponse response,
			@NotNull UserRole requiredRole) throws IOException
	{
		if (requiredRole == UserRole.invalid) {
			throw new IllegalArgumentException("Invalid required role");
		}
		
		LoggedUserDTO loggedUser = getLoggedUserWithRedirect(request, response);
		if (loggedUser == null) {
			return null;
		}
		
		if (loggedUser.getRole() != requiredRole) {
			// User not allowed to access
			ClientRedirector.redirectToPortalPage(request, response, loggedUser.getRole());
            return null;
		}
		
		return loggedUser;
	}
	
	public static @Nullable LoggedUserDTO getLoggedUserWithRedirect (
			@NotNull HttpServletRequest request,
			@NotNull HttpServletResponse response) throws IOException
	{
		LoggedUserDTO loggedUser = getLoggedUser(request);
		if (loggedUser == null) {
			// User not logged
			ClientRedirector.redirectToLogin(request, response);
		}
		return loggedUser;
	}
	
	public static @Nullable LoggedUserDTO getLoggedUser (@NotNull HttpServletRequest request) {
		return (LoggedUserDTO) request.getSession().getAttribute(LOGGED_USER_ATTRIBUTE);
	}
	
	public static void setLoggedUser (@NotNull HttpServletRequest request, @NotNull LoggedUserDTO loggedUser) {
        request.getSession().setAttribute(LOGGED_USER_ATTRIBUTE, loggedUser);
    }
	
}
