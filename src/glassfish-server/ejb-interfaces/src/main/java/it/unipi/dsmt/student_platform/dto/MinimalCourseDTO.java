package it.unipi.dsmt.student_platform.dto;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;

/**
 * DTO that stores the minimal information of a course in order
 * to visualize it in the research or in another part of the webapplication
 */
public class MinimalCourseDTO implements Serializable {

	private int id;
	private String name;
	private ProfessorDTO professor;

	public MinimalCourseDTO(int id, @NotNull String name, @NotNull ProfessorDTO professor){
		this.id = id;
		this.name = name;
		this.professor = professor;
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

	@Override
	public String toString() {
		return "Course = " + name + "<br>" +
				"Professor = " + professor.getSurname() + "<br>";
	}
}
