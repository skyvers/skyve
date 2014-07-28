package org.skyve.wildcat.tools.javadoc.doctor;

import java.io.PrintStream;

public class DocHead {
	
	public DocHead(String title){
		super();
		this.title = title;
	}
	
	private String title;
	
	private String styleSheet;
	
	public String getStyleSheet() {
		return styleSheet;
	}
	
	public void setStyleSheet(String styleSheet) {
		this.styleSheet = styleSheet;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}
	
	public void generateHTML(PrintStream out){
		out.println("<html>");
		out.println("<head>");
		out.println("<META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">");
		out.println("<title>" + this.title + "</title>");
		out.println("<link rel=\"stylesheet\" href=\"" + this.styleSheet + "\" type=\"text/css\">");
		out.println("</head>");
		out.println("<body>");
	}
	
}
