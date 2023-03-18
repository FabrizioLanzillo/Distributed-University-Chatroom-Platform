package it.unipi.dsmt.student_platform.dto;

import java.io.Serializable;

public class CourseDTO implements Serializable {
	
	private String id;
	private ProfessorDTO professor;
	private String details;
	
	public String getId() {
		return id;
	}
	
	public void setId(String id) {
		this.id = id;
	}
	
	public ProfessorDTO getProfessor() {
		return professor;
	}
	
	public void setProfessor(ProfessorDTO professor) {
		this.professor = professor;
	}
	
	public String getDetails() {
		return details;
	}
	
	public void setDetails(String details) {
		this.details = details;
	}
	
}
