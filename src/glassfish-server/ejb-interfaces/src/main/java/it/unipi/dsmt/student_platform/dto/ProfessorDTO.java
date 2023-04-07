package it.unipi.dsmt.student_platform.dto;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;

/**
 * DTO which represents a professor
 */
public class ProfessorDTO implements Serializable {

	private String name;
	private String surname;

	public ProfessorDTO (@NotNull String name, @NotNull String surname){

		this.name = name;
		this.surname = surname;
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
	
	public String getFullName() {
		return getName() + " " + getSurname();
	}

	@Override
	public String toString() {
		return "ProfessorDTO{" +
				"name='" + name + '\'' +
				", surname='" + surname + '\'' +
				'}';
	}
}
