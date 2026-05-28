package org.skyve.impl.tools.javadoc.doctor;

import java.io.PrintStream;

import org.skyve.domain.types.DateOnly;

/**
 * Renders the closing footer and end tags for generated documents.
 */
public class DocFooter {
	private String id;
	
	/**
	 * Creates a footer with an HTML element identifier.
	 *
	 * @param id footer element id attribute
	 */
	public DocFooter(String id) {
		super();
		this.id = id;
	}

	/**
	 * Returns the footer element identifier.
	 *
	 * @return footer id
	 */
	public String getId() {
		return id;
	}

	/**
	 * Sets the footer element identifier.
	 *
	 * @param id footer id
	 */
	public void setId(String id) {
		this.id = id;
	}

	private String title;

	/**
	 * Returns the footer title text.
	 *
	 * @return footer title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * Sets the footer title text.
	 *
	 * @param title footer title
	 */
	public void setTitle(String title) {
		this.title = title;
	}
	
	/**
	 * Writes the footer markup and closes the HTML document.
	 *
	 * @param out destination stream
	 */
	public void generateHTML(PrintStream out) {
		out.println("<div id=\"" + this.id + "\" class=\"footer\">");
		out.println(title + "<br/>Biz Hub Australia Pty Ltd, " + new DateOnly());
		out.println("</div>");
		out.println("</body>");
		out.println("</html>");
	}
}
