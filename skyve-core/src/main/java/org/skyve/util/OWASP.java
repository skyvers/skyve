package org.skyve.util;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.owasp.encoder.Encode;
import org.owasp.html.HtmlPolicyBuilder;
import org.owasp.html.PolicyFactory;
import org.owasp.html.Sanitizers;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/**
 * Utility methods inspired by the OWASP project.
 * @author mike
 */
public class OWASP {
	private static final PolicyFactory TEXT_SANITIZER = new HtmlPolicyBuilder().toFactory();
	private static final PolicyFactory BASIC_SANITIZER = Sanitizers.FORMATTING;
	private static final PolicyFactory SIMPLE_SANITIZER = BASIC_SANITIZER.and(Sanitizers.BLOCKS);
	private static final PolicyFactory RELAXED_SANITIZER = SIMPLE_SANITIZER.and(Sanitizers.TABLES).and(Sanitizers.IMAGES).and(Sanitizers.LINKS).and(Sanitizers.STYLES);

	// from https://github.com/OWASP/java-html-sanitizer/blob/main/src/main/java/org/owasp/html/Encoding.java
	private static final Map<String, String> UNESCAPE_REPLACEMENTS = new TreeMap<>();
	static {
		UNESCAPE_REPLACEMENTS.put("&#" + ((int) '\"') + ";", "\"");
		UNESCAPE_REPLACEMENTS.put("&amp;", "&");
		UNESCAPE_REPLACEMENTS.put("&#" + ((int) '\'') + ";", "'");
		UNESCAPE_REPLACEMENTS.put("&#" + ((int) '+') + ";", "+");
		UNESCAPE_REPLACEMENTS.put("&lt;", "<");
		UNESCAPE_REPLACEMENTS.put("&#" + ((int) '=') + ";", "=");
		UNESCAPE_REPLACEMENTS.put("&gt;", ">");
		UNESCAPE_REPLACEMENTS.put("&#" + ((int) '@') + ";", "@");
		UNESCAPE_REPLACEMENTS.put("&#" + ((int) '`') + ";", "`");
	}

	private OWASP() {
		// nothing to see here
	}
	
	public static String sanitise(Sanitisation sanitise, String html) {
		String result = html;

		if ((html != null) && (sanitise != null)) {
			switch (sanitise) {
			case text:
				result = unescapeHtmlChars(TEXT_SANITIZER.sanitize(html));
				break;
			case basic:
				result = unescapeHtmlChars(BASIC_SANITIZER.sanitize(html));
				break;
			case simple:
				result = unescapeHtmlChars(SIMPLE_SANITIZER.sanitize(html));
				break;
			case relaxed:
				result = unescapeHtmlChars(RELAXED_SANITIZER.sanitize(html));
				break;
			default:
			}
		}

		return result;
	}

	public static String unescapeHtmlChars(String html) {
		String result = html;
		if (html != null) {
			for (String entity : UNESCAPE_REPLACEMENTS.keySet()) {
				result = result.replace(entity, UNESCAPE_REPLACEMENTS.get(entity));
			}
		}
		return result;
	}
	
	public static String escapeHtml(String html) {
		return escapeHtml(html, true);
	}

	private static String escapeHtml(String html, boolean unescapeFirst) {
		String result = html;
		if (html != null) {
			if (unescapeFirst) {
				result = unescapeHtmlChars(html);
			}
			result = Encode.forHtml(result);
		}
		return result;
	}
	
	public static String escapeJsonString(String value) {
		if (value == null) {
			return null;
		}

		return value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n");
	}

	public static String escapeJsString(String value) {
		return escapeJsString(value, true, true);
	}
	
	public static String escapeJsString(String value, boolean escapeDoubleQuotes, boolean escapeNewLines) {
		if (value == null) {
			return null;
		}

		String result = value.replace("\\", "\\\\").replace("'", "\\'");
		if (escapeDoubleQuotes) {
			result = result.replace("\"", "&quot;");
		}
		if (escapeNewLines) {
			result = result.replace("\n", "<br/>");
		}
		else {
			result = result.replace("\n", "");
		}
		return result;
	}

	public static String sanitiseAndEscapeHtml(Sanitisation sanitise, String html) {
		String result = sanitise(sanitise, html);
		return escapeHtml(result, false);
	}

	public static void sanitiseAndEscapeListModelRows(List<Bean> rows,
														List<MetaDataQueryColumn> columns,
														boolean escape) {
		for (Bean row : rows) {
			for (MetaDataQueryColumn column : columns) {
				// Don't sanitise columns that are not projected
				if ((column instanceof MetaDataQueryProjectedColumn) && (! ((MetaDataQueryProjectedColumn) column).isProjected()))  {
					continue;
				}
				
				String key = column.getBinding();
				if (key == null) {
					key = column.getName();
				}

				// escape and sanitise string values if needed
				boolean escapeColumn = escape && column.isEscape();
				Sanitisation sanitiseColumn = column.getSanitise();
				if (escapeColumn || ((sanitiseColumn != null) && (! Sanitisation.none.equals(sanitiseColumn)))) {
					Object value = BindUtil.get(row, key);
					if (value instanceof String) {
						String string = (String) value;
						string = OWASP.sanitise(sanitiseColumn, string);
						if (escapeColumn) {
							string = OWASP.escapeHtml(string);
						}
						BindUtil.set(row, key, string);
					}
				}
			}
		}
	}
}
