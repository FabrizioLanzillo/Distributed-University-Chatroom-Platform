package it.unipi.dsmt.student_platform.dto;

import it.unipi.dsmt.student_platform.enums.UserRole;

public class UserDTO {
	private String username;
	private UserRole role;
	
	public UserDTO(String username, UserRole role) {
		this.username = username;
		this.role = role;
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
}
