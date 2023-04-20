package it.unipi.dsmt.student_platform.dto;

import java.io.Serializable;

/**
 * DTO that stores the necessary information to create a professor
 */
public class CreateProfessorDTO implements Serializable {
	private String username;
	private String password;
	private String email;
	private String name;
	private String surname;


	public CreateProfessorDTO(String username, String password, String email, String name, String surname) {
		this.username = username;
		this.password = password;
		this.email = email;
		this.name = name;
		this.surname = surname;
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

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getSurname() {
		return surname;
	}

	public void setSurname(String surname) {
		this.surname = surname;
	}

}
