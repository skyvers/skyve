package org.skyve.impl.tools.javadoc.doctor;

import java.util.ArrayList;
import java.util.List;

public class DocSection {
	
	private static final String SECTION_CSS_CLASS = "h3";
	
	private static final String CHAPTER_CSS_CLASS = "h1";
	
	private static final String SUB_CHAPTER_CSS_CLASS = "h2";
	
	private static final String SUB_TITLE_CSS_CLASS = "h4";
	
	public static enum SectionType {
		Chapter, Section, SubChapter, SubTitle
	}
	
	private String id;
		
	private String sectionTitle;
	
	private SectionType sectionType;

	public SectionType getSectionType() {
		return sectionType;
	}

	public void setSectionType(SectionType sectionType) {
		this.sectionType = sectionType;
	}

	private List<String> htmlContent = new ArrayList<>();
	
	public DocSection(String id) {
		super();
		this.id = id;
		this.sectionType = SectionType.Section;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getSectionTitle() {
		return sectionTitle;
	}

	public void setSectionTitle(String sectionTitle) {
		this.sectionTitle = sectionTitle;
	}

	public List<String> getHtmlContent() {
		return htmlContent;
	}
	
	public String toHTML(){
		
		StringBuilder html = new StringBuilder();
		String cssClass;
		if(SectionType.Chapter.equals(this.sectionType)){
			cssClass = CHAPTER_CSS_CLASS;
		} else if(SectionType.SubChapter.equals(this.sectionType)){
			cssClass = SUB_CHAPTER_CSS_CLASS;
		} else if(SectionType.SubTitle.equals(this.sectionType)){
			cssClass = SUB_TITLE_CSS_CLASS;
		} else{
			cssClass = SECTION_CSS_CLASS;
		}
		
//		html.append("<div id=\"" + this.id + "\" class=\""  + cssClass + "\">");
//		html.append("\t<a name=\"" + this.id + "\">" + this.sectionTitle + "</a>");
//		html.append("</div>");
		html.append("<").append(cssClass.toString()).append(">");
		html.append(this.sectionTitle);
		html.append("</").append(cssClass.toString()).append(">");
		for(String s : htmlContent){
			if(s!=null){
				html.append(s);
			}
		}
		return html.toString();
	}

}
