package it.unipi.dsmt.student_platform.dto;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;

public class ProfessorDTO implements Serializable {
	
	private String id;
	private String name;
	private String surname;
	
	public ProfessorDTO (@NotNull String id,
	                     @NotNull String name,
	                     @NotNull String surname)
	{
		this.id = id;
		this.name = name;
		this.surname = surname;
	}
	
	public String getId() {
		return id;
	}
	
	public void setId(String id) {
		this.id = id;
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
				"id='" + id + '\'' +
				", name='" + name + '\'' +
				", surname='" + surname + '\'' +
				'}';
	}
}
