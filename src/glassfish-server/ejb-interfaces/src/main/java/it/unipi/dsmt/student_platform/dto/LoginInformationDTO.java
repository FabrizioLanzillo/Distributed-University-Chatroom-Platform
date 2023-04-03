package it.unipi.dsmt.student_platform.dto;

import it.unipi.dsmt.student_platform.enums.UserRole;
import org.jetbrains.annotations.NotNull;

import java.io.Serializable;

public class LoginInformationDTO implements Serializable {
	private String username;
	private String password;
	private UserRole role;
	
	public LoginInformationDTO (@NotNull String username,
	                            @NotNull String password,
	                            @NotNull UserRole role)
	{
		this.username = username;
		this.password = password;
		this.role = role;
	}
	
	public String getUsername() {
		return username;
	}
	
	public void setUsername(String username) {
		this.username = username;
	}
	
	public String getPassword() {
		return password;
	}
	
	public void setPassword(String password) {
		this.password = password;
	}
	
	public UserRole getRole() {
		return role;
	}
	
	public void setRole(UserRole role) {
		this.role = role;
	}
}
