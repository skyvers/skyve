package org.skyve.impl.tools.javadoc.doctor;


public class DocHeader {
	
	public DocHeader(String title) {
		super();
		this.title = title;
	}	

	private String title;
	
	private String logoLocation;
	
	private String ribbonBarLocation;

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getLogoLocation() {
		return logoLocation;
	}

	public void setLogoLocation(String logoLocation) {
		this.logoLocation = logoLocation;
	}

	public String getRibbonBarLocation() {
		return ribbonBarLocation;
	}

	public void setRibbonBarLocation(String ribbonBarLocation) {
		this.ribbonBarLocation = ribbonBarLocation;
	}
	
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
