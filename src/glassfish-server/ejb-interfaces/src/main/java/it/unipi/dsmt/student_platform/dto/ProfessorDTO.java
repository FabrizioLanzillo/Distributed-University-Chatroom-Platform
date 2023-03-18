package it.unipi.dsmt.student_platform.dto;

import java.io.Serializable;

public class ProfessorDTO implements Serializable {
	
	private String id;
	private String name;
	private String surname;
	
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
}
