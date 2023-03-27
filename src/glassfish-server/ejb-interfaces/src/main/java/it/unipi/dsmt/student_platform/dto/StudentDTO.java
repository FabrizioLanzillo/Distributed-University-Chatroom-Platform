package it.unipi.dsmt.student_platform.dto;

import java.io.Serializable;
import org.jetbrains.annotations.NotNull;

public class StudentDTO implements Serializable {

	private String id;
	private String name;
	private String surname;
	private String username;
	private String email;
	private String degree;
	private String language;

	public StudentDTO(@NotNull String id,
					  @NotNull String name,
					  @NotNull String surname,
					  @NotNull String username,
					  @NotNull String email,
					  @NotNull String degree,
					  @NotNull String language){
		this.id = id;
		this.username = username;
		this.name = name;
		this.surname = surname;
		this.degree = degree;
		this.email = email;
		this.language = language;
	}

	public String getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String getSurname() {
		return surname;
	}

	public String getUsername() {
		return username;
	}

	public String getEmail() {
		return email;
	}

	public String getDegree() {
		return degree;
	}

	public String getLanguage() {
		return language;
	}

	public void setId(String id) {
		this.id = id;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setSurname(String surname) {
		this.surname = surname;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public void setDegree(String degree) {
		this.degree = degree;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	@Override
	public String toString() {
		return "StudentDTO{" +
				"id='" + id + '\'' +
				", name='" + name + '\'' +
				", surname='" + surname + '\'' +
				", username='" + username + '\'' +
				", email='" + email + '\'' +
				", degree='" + degree + '\'' +
				", language='" + language + '\'' +
				'}';
	}

}
