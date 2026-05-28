package org.skyve.impl.tools.javadoc.doctor;

import java.io.PrintStream;

/**
 * Renders the opening HTML head and body elements for generated documents.
 */
public class DocHead {
	/**
	 * Creates a head fragment with the supplied page title.
	 *
	 * @param title HTML title element value
	 */
	public DocHead(String title){
		super();
		this.title = title;
	}
	
	private String title;
	
	private String styleSheet;
	
	/**
	 * Returns the stylesheet URL referenced by this head.
	 *
	 * @return stylesheet URL, or {@code null} when unset
	 */
	public String getStyleSheet() {
		return styleSheet;
	}
	
	/**
	 * Sets the stylesheet URL referenced by this head.
	 *
	 * @param styleSheet stylesheet URL
	 */
	public void setStyleSheet(String styleSheet) {
		this.styleSheet = styleSheet;
	}

	/**
	 * Returns the HTML title value.
	 *
	 * @return page title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * Sets the HTML title value.
	 *
	 * @param title page title
	 */
	public void setTitle(String title) {
		this.title = title;
	}
	
	/**
	 * Writes opening HTML document tags and metadata to the output stream.
	 *
	 * @param out destination stream
	 */
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
