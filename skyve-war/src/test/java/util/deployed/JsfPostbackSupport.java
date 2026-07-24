package util.deployed;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Extracts JSF state and constructs normal or PrimeFaces Ajax postback form values. */
public final class JsfPostbackSupport {
	private static final String VIEW_STATE = "jakarta.faces.ViewState";
	private static final Pattern INPUT = Pattern.compile("<input\\b[^>]*>", Pattern.CASE_INSENSITIVE);
	private static final Pattern ATTRIBUTE = Pattern.compile("([a-z_:][-a-z0-9_:.]*)\\s*=\\s*([\"'])(.*?)\\2",
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

	private JsfPostbackSupport() {
		// Utility class.
	}

	/** Returns the hidden JSF view-state value from rendered HTML. */
	public static String extractViewState(String html) {
		Matcher inputs = INPUT.matcher(html);
		while (inputs.find()) {
			Map<String, String> attributes = attributes(inputs.group());
			String name = attributes.get("name");
			if (VIEW_STATE.equals(name) || "javax.faces.ViewState".equals(name)) {
				String value = attributes.get("value");
				if (value != null) {
					return htmlText(value);
				}
			}
		}
		throw new IllegalArgumentException("Rendered HTML does not contain a JSF view state");
	}

	/** Builds the common values for a full JSF form postback. */
	public static Map<String, String> normalPostback(String formId, String viewState, String sourceId) {
		Map<String, String> result = new LinkedHashMap<>();
		result.put(formId, formId);
		result.put(VIEW_STATE, viewState);
		result.put("jakarta.faces.source", sourceId);
		result.put(sourceId, sourceId);
		return result;
	}

	/** Builds PrimeFaces partial-request values on top of a normal postback. */
	public static Map<String, String> ajaxPostback(String formId,
															String viewState,
															String sourceId,
															String execute,
															String render) {
		Map<String, String> result = normalPostback(formId, viewState, sourceId);
		result.put("jakarta.faces.partial.ajax", "true");
		result.put("jakarta.faces.partial.execute", execute);
		result.put("jakarta.faces.partial.render", render);
		return result;
	}

	private static Map<String, String> attributes(String input) {
		Map<String, String> result = new LinkedHashMap<>();
		Matcher attributes = ATTRIBUTE.matcher(input);
		while (attributes.find()) {
			result.put(attributes.group(1).toLowerCase(), attributes.group(3));
		}
		return result;
	}

	private static String htmlText(String value) {
		return value.replace("&quot;", "\"").replace("&#39;", "'").replace("&lt;", "<").replace("&gt;", ">")
				.replace("&amp;", "&");
	}

	/** Encodes form values for an HTTP request body without shell or URL concatenation. */
	public static String encode(Map<String, String> values) {
		StringBuilder result = new StringBuilder(values.size() * 32);
		for (Map.Entry<String, String> entry : values.entrySet()) {
			if (result.length() > 0) {
				result.append('&');
			}
			result.append(java.net.URLEncoder.encode(entry.getKey(), StandardCharsets.UTF_8)).append('=')
					.append(java.net.URLEncoder.encode(entry.getValue(), StandardCharsets.UTF_8));
		}
		return result.toString();
	}

	/** Decodes one encoded form component for diagnostic assertions. */
	static String decode(String value) {
		return URLDecoder.decode(value, StandardCharsets.UTF_8);
	}
}
