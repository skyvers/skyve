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
import org.skyve.impl.util.SafeFileName;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.view.TextOutput.Sanitisation;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides sanitisation and context-specific output escaping utilities.
 *
 * <p>Sanitisation restricts the content permitted in a value, while escaping encodes a value for a specific output
 * context. Callers must apply the appropriate operation at the appropriate boundary; a value escaped for HTML, JSON,
 * or JavaScript is not interchangeable with a value escaped for another context.
 *
 * <p>Thread-safe: this class is stateless and may be used concurrently.
 *
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

	/**
	 * Prevents instantiation of this utility class.
	 */
	private OWASP() {
		// nothing to see here
	}

	/**
	 * Sanitises HTML according to the requested policy.
	 *
	 * <p>{@link Sanitisation#text} removes all markup, {@link Sanitisation#basic} permits inline formatting,
	 * {@link Sanitisation#simple} additionally permits block elements, and {@link Sanitisation#relaxed} additionally
	 * permits tables, images, links, and styles. A {@code null} policy or {@link Sanitisation#none} returns the input
	 * unchanged. This method sanitises content but does not escape it for an output context.
	 *
	 * @param sanitise the sanitisation policy, or {@code null} to leave the input unchanged
	 * @param html the content to sanitise; may be {@code null}
	 * @return the sanitised content, the unchanged content when no policy applies, or {@code null} when {@code html} is
	 *         {@code null}
	 */
	@SuppressWarnings("javasecurity:S5131") // false positive: result is not left assigned unless Sanitise is null
	public static @Nullable String sanitise(@Nullable Sanitisation sanitise, @Nullable String html) {
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

	/**
	 * Replaces the HTML entities emitted by the configured sanitisation policies with their corresponding characters.
	 *
	 * <p>This is a targeted reversal for the entities in the internal replacement table, not a general-purpose HTML
	 * entity decoder.
	 *
	 * @param html the content containing supported HTML entities; may be {@code null}
	 * @return the content with supported entities replaced, or {@code null} when {@code html} is {@code null}
	 */
	public static @Nullable String unescapeHtmlChars(@Nullable String html) {
		if (html == null) {
			return null;
		}

		String result = html;
		for (String entity : UNESCAPE_REPLACEMENTS.keySet()) {
			result = result.replace(entity, UNESCAPE_REPLACEMENTS.get(entity));
		}
		return result;
	}

	/**
	 * Escapes content for placement in an HTML text context.
	 *
	 * <p>Supported pre-existing HTML entities are decoded before encoding to avoid escaping them a second time. This
	 * method performs output encoding only; it does not sanitise the permitted HTML content.
	 *
	 * @param html the content to escape; may be {@code null}
	 * @return the HTML-escaped content, or {@code null} when {@code html} is {@code null}
	 */
	public static @Nullable String escapeHtml(@Nullable String html) {
		return escapeHtml(html, true);
	}

	/**
	 * Escapes content for an HTML text context, optionally decoding supported entities first.
	 *
	 * @param html the content to escape; may be {@code null}
	 * @param unescapeFirst {@code true} to decode supported HTML entities before escaping
	 * @return the HTML-escaped content, or {@code null} when {@code html} is {@code null}
	 */
	private static @Nullable String escapeHtml(@Nullable String html, boolean unescapeFirst) {
		String result = html;
		if (html != null) {
			if (unescapeFirst) {
				result = unescapeHtmlChars(html);
			}
			result = Encode.forHtml(result);
		}
		return result;
	}
	
	/**
	 * Escapes string content for placement between JSON quotation marks.
	 *
	 * <p>The returned value does not include the surrounding quotation marks. Quotation marks, reverse solidus, and
	 * every control character from U+0000 through U+001F are escaped according to RFC 8259.
	 *
	 * @param value the unescaped string content; may be {@code null}
	 * @return the escaped string content, or {@code null} when {@code value} is {@code null}
	 */
	public static @Nullable String escapeJsonString(@Nullable String value) {
		if (value == null) {
			return null;
		}

		StringBuilder escaped = new StringBuilder(value.length());
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			switch (c) {
			case '"':
				escaped.append("\\\"");
				break;
			case '\\':
				escaped.append("\\\\");
				break;
			case '\b':
				escaped.append("\\b");
				break;
			case '\f':
				escaped.append("\\f");
				break;
			case '\n':
				escaped.append("\\n");
				break;
			case '\r':
				escaped.append("\\r");
				break;
			case '\t':
				escaped.append("\\t");
				break;
			default:
				if (c < 0x20) {
					escaped.append("\\u00")
							.append(Character.forDigit(c >>> 4, 16))
							.append(Character.forDigit(c & 0x0f, 16));
				}
				else {
					escaped.append(c);
				}
				break;
			}
		}
		return escaped.toString();
	}

	/**
	 * Escapes string content for placement between JavaScript quotation marks.
	 *
	 * <p>The returned value does not include the surrounding quotation marks and is safe for either a single-quoted or
	 * double-quoted JavaScript string literal. This method does not encode executable JavaScript source and must not be
	 * used to insert untrusted content outside a string literal.
	 *
	 * @param value the unescaped string content; may be {@code null}
	 * @return the escaped string content, or {@code null} when {@code value} is {@code null}
	 */
	public static @Nullable String escapeJsString(@Nullable String value) {
		return (value == null) ? null : Encode.forJavaScriptBlock(value);
	}

	/**
	 * Applies the JavaScript presentation escaping defaults.
	 *
	 * <p>This escapes reverse solidus and apostrophe characters, replaces quotation marks with HTML entities, and
	 * converts line feeds to {@code <br/>}. It is retained for existing presentation behavior. New JavaScript string
	 * output boundaries should use {@link #escapeJs(String)}.
	 *
	 * @param value the content to transform; may be {@code null}
	 * @return the transformed content, or {@code null} when {@code value} is {@code null}
	 */
	public static @Nullable String escapeJsStringWithHtmlFormatting(@Nullable String value) {
		return escapeJsStringWithHtmlFormatting(value, true, true);
	}

	/**
	 * Applies configurable JavaScript presentation escaping.
	 *
	 * <p>Reverse solidus and apostrophe characters are always escaped. When {@code escapeDoubleQuotes} is set,
	 * quotation marks become {@code &quot;}. Line feeds become {@code <br/>} when {@code escapeNewLines} is set and
	 * are removed otherwise. This method is not a general JavaScript contextual encoder; new JavaScript string output
	 * boundaries should use {@link #escapeJs(String)}.
	 *
	 * @param value the content to transform; may be {@code null}
	 * @param escapeDoubleQuotes {@code true} to replace quotation marks with HTML entities
	 * @param escapeNewLines {@code true} to replace line feeds with HTML line breaks; {@code false} to remove them
	 * @return the transformed content, or {@code null} when {@code value} is {@code null}
	 */
	public static @Nullable String escapeJsStringWithHtmlFormatting(@Nullable String value,
													boolean escapeDoubleQuotes,
													boolean escapeNewLines) {
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

	/**
	 * Sanitises content according to the requested policy and escapes it for an HTML text context.
	 *
	 * <p>The sanitiser output is encoded directly without first decoding supported entities.
	 *
	 * @param sanitise the sanitisation policy, or {@code null} to skip sanitisation
	 * @param html the content to sanitise and escape; may be {@code null}
	 * @return the sanitised and HTML-escaped content, or {@code null} when {@code html} is {@code null}
	 */
	public static @Nullable String sanitiseAndEscapeHtml(@Nullable Sanitisation sanitise, @Nullable String html) {
		String result = sanitise(sanitise, html);
		return escapeHtml(result, false);
	}

	/**
	 * Sanitises and optionally HTML-escapes string values in query result rows according to their column metadata.
	 *
	 * <p>Side effects: mutates each matching string binding in {@code rows}. Non-projected projected columns are skipped.
	 * A column is HTML-escaped only when both {@code escape} and the column's escape flag are set; its sanitisation policy
	 * is applied independently when configured.
	 *
	 * <p>Complexity: O(r * c) binding inspections where r is the number of rows and c is the number of columns.
	 *
	 * @param rows the mutable query result rows; must not be {@code null}
	 * @param columns the metadata describing bindings and output policies; must not be {@code null}
	 * @param escape {@code true} to honour each column's HTML escape flag
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public static void sanitiseAndEscapeListModelRows(@Nonnull List<Bean> rows,
														@Nonnull List<MetaDataQueryColumn> columns,
														boolean escape) {
		for (Bean row : rows) {
			for (MetaDataQueryColumn column : columns) {
				// Don't sanitise columns that are not projected
				if ((column instanceof MetaDataQueryProjectedColumn p) && (! p.isProjected()))  {
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
					if (key == null) {
						throw new MetaDataException("A query column to escape or sanitise has neither a binding nor a name");
					}
					Object value = BindUtil.get(row, key);
					if (value instanceof String string) {
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

	/**
	 * Sanitises the input string to be safe for use in browser-accessible content paths.
	 *
	 * @param input the original file name string; may be {@code null}
	 * @return a sanitised, safe file name string; never {@code null}
	 */
	public static @Nonnull String sanitiseFileName(@Nullable String input) {
		return SafeFileName.sanitise(input);
	}

	/**
	 * Strips ASCII control characters from {@code value} to prevent log injection attacks.
	 *
	 * <p>Replaces each character in the ranges U+0000–U+001F and U+007F (DEL) with an
	 * underscore ({@code _}). This covers CR, LF, tab, NUL, and all other ASCII control
	 * codes that could be used to forge log entries.
	 *
	 * @param value the string to sanitise; may be {@code null}
	 * @return the sanitised string, or {@code null} if {@code value} was {@code null}
	 */
	public static @Nullable String sanitiseLog(@Nullable String value) {
		return value == null ? null : value.replaceAll("[\u0000-\u001f\u007f]", "_");
	}
}
