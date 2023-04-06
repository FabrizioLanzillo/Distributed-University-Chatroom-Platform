package it.unipi.dsmt.student_platform.dto;

import it.unipi.dsmt.student_platform.enums.UserRole;
import org.jetbrains.annotations.NotNull;

import java.io.Serializable;

/**
 * DTO that stores the necessary information to identify a logged user
 * in the application.
 */
public class LoggedUserDTO implements Serializable {
	private String id;
	private String username;
	private UserRole role;
	
	public LoggedUserDTO (@NotNull String id,
	                      @NotNull String username,
	                      @NotNull UserRole role)
	{
		this.id = id;
		this.username = username;
		this.role = role;
	}
	
	public String getId() {
		return id;
	}
	
	public void setId(String id) {
		this.id = id;
	}
	
	public String getUsername() {
		return username;
	}
	
	public void setUsername(String username) {
		this.username = username;
	}
	
	public UserRole getRole() {
		return role;
	}
	
	public void setRole(UserRole role) {
		this.role = role;
	}
	
	@Override
	public String toString() {
		return "LoggedUserDTO{" +
				"id='" + id + '\'' +
				", username='" + username + '\'' +
				", role=" + role +
				'}';
	}
	
}
