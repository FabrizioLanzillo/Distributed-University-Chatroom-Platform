package it.unipi.dsmt.student_platform.dto;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;

public class CourseDTO implements Serializable {
	
	private int id;
	private String name;
	private ProfessorDTO professor;
	private String description;
	
	public CourseDTO (int id,
	                  @NotNull String name,
	                  @NotNull ProfessorDTO professor,
	                  @NotNull String description)
	{
		this.id = id;
		this.name = name;
		this.professor = professor;
		this.description = description;
	}
	
	public int getId() {
		return id;
	}
	
	public void setId(int id) {
		this.id = id;
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public ProfessorDTO getProfessor() {
		return professor;
	}
	
	public void setProfessor(ProfessorDTO professor) {
		this.professor = professor;
	}
	
	public String getDescription() {
		return description;
	}
	
	public void setDescription(String description) {
		this.description = description;
	}
	
	@Override
	public String toString() {
		return "CourseDTO{" +
				"id='" + id + '\'' +
				", name='" + name + '\'' +
				", professor=" + professor +
				", description='" + description + '\'' +
				'}';
	}
}
