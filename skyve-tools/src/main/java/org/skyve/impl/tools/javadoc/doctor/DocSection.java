package org.skyve.impl.tools.javadoc.doctor;

import java.util.ArrayList;
import java.util.List;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Represents a titled documentation section with optional HTML content.
 */
public class DocSection {
	private static final String SECTION_CSS_CLASS = "h3";
	
	private static final String CHAPTER_CSS_CLASS = "h1";
	
	private static final String SUB_CHAPTER_CSS_CLASS = "h2";
	
	private static final String SUB_TITLE_CSS_CLASS = "h4";
	
	/**
	 * Defines supported heading levels for section rendering.
	 */
	@SuppressWarnings("java:S115") // Enum names are documentation heading identifiers.
	public enum SectionType {
		/** Top-level chapter heading. */
		Chapter, Section, SubChapter, SubTitle
	}
	
	private String id;
		
	private String sectionTitle;
	
	private SectionType sectionType;

	/**
	 * Returns the heading type used when rendering this section.
	 *
	 * @return section heading type
	 */
	public @Nullable SectionType getSectionType() {
		return sectionType;
	}

	/**
	 * Sets the heading type used when rendering this section.
	 *
	 * @param sectionType heading type
	 */
	public void setSectionType(@Nullable SectionType sectionType) {
		this.sectionType = sectionType;
	}

	private List<String> htmlContent = new ArrayList<>();
	
	/**
	 * Creates a section with default {@link SectionType#Section} heading style.
	 *
	 * @param id section identifier
	 */
	public DocSection(@Nullable String id) {
		super();
		this.id = id;
		this.sectionType = SectionType.Section;
	}

	/**
	 * Returns the section identifier.
	 *
	 * @return section id
	 */
	public @Nullable String getId() {
		return id;
	}

	/**
	 * Sets the section identifier.
	 *
	 * @param id section id
	 */
	public void setId(@Nullable String id) {
		this.id = id;
	}

	/**
	 * Returns the section title text.
	 *
	 * @return section title
	 */
	public @Nullable String getSectionTitle() {
		return sectionTitle;
	}

	/**
	 * Sets the section title text.
	 *
	 * @param sectionTitle section title
	 */
	public void setSectionTitle(@Nullable String sectionTitle) {
		this.sectionTitle = sectionTitle;
	}

	/**
	 * Returns additional HTML fragments appended after the heading.
	 *
	 * @return mutable list of HTML fragments
	 */
	public @Nonnull List<String> getHtmlContent() {
		return htmlContent;
	}
	
	/**
	 * Renders the section heading and attached content as HTML.
	 *
	 * @return HTML string representation
	 */
	public @Nonnull String toHTML(){
		
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
