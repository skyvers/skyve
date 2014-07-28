package org.skyve.wildcat.tools.javadoc.doctor;

import java.io.PrintStream;

import org.skyve.domain.types.DateOnly;

public class DocFooter {

	private String id;
	
	public DocFooter(String id) {
		super();
		this.id = id;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	private String title;

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}
	
	public void generateHTML(PrintStream out) {
		out.println("<div id=\"" + this.id + "\" class=\"footer\">");
		out.println(title + "<br/>Biz Hub Australia Pty Ltd, " + new DateOnly());
		out.println("</div>");
		out.println("</body>");
		out.println("</html>");
	}
}
