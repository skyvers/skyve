package org.skyve.wildcat.tools.javadoc.doctor;

import java.util.ArrayList;
import java.util.List;

public class DocTable {

	private String id;

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public DocTable(String id) {
		super();
		this.id = id;
	}

	private String title;

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	private List<String> htmlContent = new ArrayList<>();

	public List<String> getHtmlContent() {
		return htmlContent;
	}

	public String[] getHeaders() {
		return headers;
	}

	public void setHeaders(String[] headers) {
		this.headers = headers;
	}

	private String[] headers;

	private List<String[]> rows = new ArrayList<>();

	public void setHeaderValues(String... headers) {
		this.headers = headers;
	}

	public void setRowValues(String... values) {
		this.rows.add(values);
	}

	public String toHTML(boolean displayAsListIfOneColumnOnly) {

		// exclude empty columns - if only one column then display as an
		// unordered list
		boolean[] cols = new boolean[this.headers.length];
		for (String[] row : this.rows) {
			for (int i = 0; i < this.headers.length; i++) {
				// turn col true if already true or if the row value is not null
				// or empty
				cols[i] = cols[i] || (row[i] != null && row[i].length() > 0);
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
