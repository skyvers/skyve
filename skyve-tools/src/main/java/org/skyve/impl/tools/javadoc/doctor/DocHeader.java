package org.skyve.impl.tools.javadoc.doctor;


/**
 * Builds the visual header block for generated documentation pages.
 */
public class DocHeader {
	/**
	 * Creates a header with a title.
	 *
	 * @param title header title text
	 */
	public DocHeader(String title) {
		super();
		this.title = title;
	}	

	private String title;
	
	private String logoLocation;
	
	private String ribbonBarLocation;

	/**
	 * Returns the header title text.
	 *
	 * @return header title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * Sets the header title text.
	 *
	 * @param title header title
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	/**
	 * Returns the logo image location.
	 *
	 * @return logo location, or {@code null} when unset
	 */
	public String getLogoLocation() {
		return logoLocation;
	}

	/**
	 * Sets the logo image location.
	 *
	 * @param logoLocation logo image URL or path
	 */
	public void setLogoLocation(String logoLocation) {
		this.logoLocation = logoLocation;
	}

	/**
	 * Returns the ribbon-bar image location.
	 *
	 * @return ribbon-bar location, or {@code null} when unset
	 */
	public String getRibbonBarLocation() {
		return ribbonBarLocation;
	}

	/**
	 * Sets the ribbon-bar image location.
	 *
	 * @param ribbonBarLocation ribbon-bar image URL or path
	 */
	public void setRibbonBarLocation(String ribbonBarLocation) {
		this.ribbonBarLocation = ribbonBarLocation;
	}
	
	/**
	 * Renders this header as HTML table markup.
	 *
	 * @return HTML string representation
	 */
	public String toHTML(){
		StringBuilder html =new StringBuilder();
		
		html.append("<table class=\"pageHeader\" width=\"100%\">");
		html.append("\t<thead/>");
		html.append("\t<tbody>");
		
		StringBuilder headerRow = new StringBuilder();
		headerRow.append("\t\t<tr>");
		if(this.logoLocation!=null){
			headerRow.append("<td><img src=\"").append(this.logoLocation).append("\"/></td><td>");
		}
		headerRow.append("<td>").append(this.title).append("</td>");
		
		if(this.ribbonBarLocation!=null){
			headerRow.append("<td><img src=\"").append(this.ribbonBarLocation).append("\"/></td>");
		}
		headerRow.append("</tr>");
		html.append(headerRow.toString());
		
		html.append("\t</tbody>");
		html.append("</table>");
		
		return html.toString();
	}
}
