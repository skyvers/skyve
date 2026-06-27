package org.skyve.impl.tools.javadoc.doctor;

import java.util.ArrayList;
import java.util.List;

/**
 * Renders tabular documentation content with optional list fallback.
 */
public class DocTable {
	private String id;

	/**
	 * Returns the table identifier.
	 *
	 * @return table id
	 */
	public String getId() {
		return id;
	}

	/**
	 * Sets the table identifier.
	 *
	 * @param id table id
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * Creates a table with the supplied identifier.
	 *
	 * @param id table id
	 */
	public DocTable(String id) {
		super();
		this.id = id;
	}

	private String title;

	/**
	 * Returns the table title.
	 *
	 * @return table title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * Sets the table title.
	 *
	 * @param title table title
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	private List<String> htmlContent = new ArrayList<>();

	/**
	 * Returns additional HTML fragments emitted before table content.
	 *
	 * @return mutable list of HTML fragments
	 */
	public List<String> getHtmlContent() {
		return htmlContent;
	}

	/**
	 * Returns configured column headers.
	 *
	 * @return header array
	 */
	public String[] getHeaders() {
		return headers;
	}

	/**
	 * Sets configured column headers.
	 *
	 * @param headers header values
	 */
	public void setHeaders(String[] headers) {
		this.headers = headers;
	}

	private String[] headers;

	private List<String[]> rows = new ArrayList<>();

	/**
	 * Replaces the table header values.
	 *
	 * @param headers header values
	 */
	public void setHeaderValues(String... headers) {
		this.headers = headers;
	}

	/**
	 * Appends a row of values to the table.
	 *
	 * @param values row values aligned with headers
	 */
	public void setRowValues(String... values) {
		this.rows.add(values);
	}

	/**
	 * Renders this table as HTML.
	 *
	 * <p>Empty columns are omitted. When enabled and exactly one non-empty
	 * column remains, content is rendered as an unordered list.
	 *
	 * @param displayAsListIfOneColumnOnly whether to use list fallback
	 * @return HTML string representation
	 */
	@SuppressWarnings({"java:S3776", "java:S6541"}) // complexity OK
	public String toHTML(boolean displayAsListIfOneColumnOnly) {

		// exclude empty columns - if only one column then display as an
		// unordered list
		boolean[] cols = new boolean[this.headers.length];
		for (String[] row : this.rows) {
			for (int i = 0; i < this.headers.length; i++) {
				// turn col true if already true or if the row value is not null
				// or empty
				cols[i] = cols[i] || (row[i] != null && (! row[i].isEmpty()));
			}
		}
		// count how many non-null columns
		int colCount = 0;
		int firstNonEmpty = this.headers.length;
		for (int i = 0; i < this.headers.length; i++) {
			if (cols[i]) {
				colCount++;
				if (i < firstNonEmpty) {
					firstNonEmpty = i;
				}
			}
		}

		StringBuilder html = new StringBuilder();
		
		//Section title
		DocSection section = new DocSection(this.id);
		section.setSectionTitle(this.title);
		html.append(section.toHTML());
		for (String s : htmlContent) {
			html.append(s);
		}

		if (displayAsListIfOneColumnOnly && colCount == 1) {

			DocList table = new DocList(false);
			for (String[] row : this.rows) {
				table.getItems().add(row[firstNonEmpty]);
			}
			html.append(table.toHTML());
		} else {

			html.append("<table cellspacing=\"1\" class=\"dataTable\">");
			html.append("\t<thead>");
			html.append("\t<tr class=\"tablerowodd\">");

			// check if all values of a column are empty, do not output the
			// column
			int hdrPos = 0;
			for (String hdr : this.headers) {
				if (cols[hdrPos++]) {
					html.append("\t\t<th>" + hdr + "</th>");
				}
			}
			html.append("\t</tr>");
			html.append("\t</thead>");

			int counter = 0;
			html.append("\t<tbody>");
			for (String[] row : this.rows) {
				StringBuilder r = new StringBuilder();

				if (counter == 0) {
					r.append("\t\t<tr class=\"tableroweven\">");
					counter++;
				} else {
					r.append("\t\t<tr class=\"tablerowodd\">");
					counter = 0;
				}
				int rowPos = 0;
				for (String value : row) {
					if (cols[rowPos++]) {
						r.append("<td>").append((value == null ? "" : value)).append("</td>");
					}
				}
				r.append("</tr>");

				html.append(r.toString());
			}
			html.append("\t</tbody>");
			html.append("</table>");
		}
		return html.toString();

	}
}
