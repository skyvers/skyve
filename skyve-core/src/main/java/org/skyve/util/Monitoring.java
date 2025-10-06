package org.skyve.util;

import java.lang.management.ManagementFactory;
import java.lang.management.OperatingSystemMXBean;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;

/**
 * System monitoring functions.
 */
public class Monitoring {
	private static final Logger LOGGER = LoggerFactory.getLogger(Monitoring.class);
	private static final float MiB = 1024 * 1024.0F;

	private Monitoring() {
		// prevent instantiation
	}

	public static double systemLoadAverage() {
		OperatingSystemMXBean os = ManagementFactory.getOperatingSystemMXBean();
		return os.getSystemLoadAverage();
	}

	public static float totalMemoryInMiB() {
		Runtime runtime = Runtime.getRuntime();
		return runtime.totalMemory() / MiB;
	}

	public static float freeMemoryInMiB() {
		Runtime runtime = Runtime.getRuntime();
		return runtime.freeMemory() / MiB;
	}

	public static float maxMemoryInMiB() {
		Runtime runtime = Runtime.getRuntime();
		return runtime.maxMemory() / MiB;
	}

	public static short percentageUsedMemory() {
		Runtime runtime = Runtime.getRuntime();
		long total = runtime.totalMemory();
		long free = runtime.freeMemory();
		return (short) (((total - free) / (double) total) * 10000d);
	}

	private static final ConcurrentHashMap<String, RequestMeasurements> REQUEST_MEASUREMENTS = new ConcurrentHashMap<>();
	private static final ResourceMeasurements RESOURCE_MEASUREMENTS = new ResourceMeasurements();

	// Reference start time for all measurements (in milliseconds since epoch)
	private static final long MONITORING_START_TIME = System.currentTimeMillis();

	public static class RequestKey {
		private char type;
		private String moduleName;
		private String documentName;
		private String component;

		public static final RequestKey NONE = new RequestKey(' ', null, null, null);

		private RequestKey(char type, String moduleName, String documentName, String component) {
			this.type = type;
			this.moduleName = moduleName;
			this.documentName = documentName;
			this.component = component;
		}

		public static @Nonnull RequestKey from(@Nonnull HttpServletRequest request) {
			RequestKey result = NONE;

			// Check for PrimeFaces AJAX requests first
			String isAjax = request.getParameter("jakarta.faces.partial.ajax");
			if ("true".equals(isAjax)) {
				LOGGER.debug("PRIMEFACES AJAX REQUEST CAUGHT - not monitored");
			}

			String a = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("a")));
			if ("e".equals(a)) {
				String m = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("m")));
				String d = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("d")));
				if ((m != null && d != null)) {
					String i = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("i")));
					result = (i == null) ? create(m, d) : edit(m, d);
				}
				LOGGER.debug("a=e REQUEST CAUGHT");
			} else if ("l".equals(a)) {
				String m = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("m")));
				String q = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("q")));
				if (m != null) {
					if (q != null) {
						String d = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("d")));
						if (d == null) {
							result = queryist(m, q);
						} else {
							result = modelList(m, d, q);
						}
					}
				}
				LOGGER.debug("a=l REQUEST CAUGHT");
			} else if ("m".equals(a)) {
				LOGGER.debug("a=m REQUEST CAUGHT");
			}
			// etc etc
			else { // SC
				String path = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getServletPath()));
				if ("/smartedit".equals(path)) {
					String m = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter(AbstractWebContext.MODULE_NAME)));
					String d = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter(AbstractWebContext.DOCUMENT_NAME)));
					if ((m != null) && (d != null)) {
						result = smartEdit(m, d);
					}
				} else if ("/map".equals(path)) {
					String module = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("_mod")));
					String query = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("_q")));
					String geometryBinding = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_geo")));

					// Alternative parameter pattern: geoBinding from '_m' parameter
					if (geometryBinding == null) {
						geometryBinding = OWASP.sanitise(Sanitisation.text,
								UtilImpl.processStringValue(request.getParameter("_m")));
					}

					// If we're in PrimeFaces (detected by skyveUxUi attribute), extract module/document from referer
					if (module == null) {
						Object uxUi = request.getAttribute("skyveUxUi");
						if (uxUi != null) {
							// We're in PrimeFaces - extract module and document from referer
							String referer = request.getHeader("Referer");
							if (referer != null) {
								// Parse referer URL to extract module and document
								// Expected format: .../edit?m=<module>&d=<document>&...
								String moduleFromReferer = extractParameterFromUrl(referer, "m");
								String documentFromReferer = extractParameterFromUrl(referer, "d");
								if (moduleFromReferer != null) {
									module = OWASP.sanitise(Sanitisation.text, moduleFromReferer);
									// Use document as query
									if (query == null && documentFromReferer != null) {
										query = OWASP.sanitise(Sanitisation.text, documentFromReferer);
									}
								}
							}
						}
					}

					if (module != null) {
						result = map(module, query, geometryBinding);
					}
				} else if ("/chart".equals(path)) {
					String modelName = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("_m")));
					String dataSource = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("ds")));

					// Use model name if available, otherwise use datasource
					String chartIdentifier = modelName != null ? modelName : dataSource;
					if (chartIdentifier != null) {
						result = chart(chartIdentifier);
					}
				} else if ("/content".equals(path)) {
					LOGGER.debug("CONTENT REQUEST CAUGHT: {} - not monitored", path);
				} else if ("/sse/stream".equals(path) || path.startsWith("/sse")) {
					// Ignore SSE requests - return NONE to skip monitoring
					return NONE;
				} else if ("/smartlist".equals(path)) {
					String dataSource = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_dataSource")));
					result = smartClientList(dataSource);
				} else if ("/smartsnap".equals(path)) {
					LOGGER.debug("SMARTCLIENT SNAPSHOT REQUEST CAUGHT: {} - not monitored", path);
				} else if ("/smarttag".equals(path)) {
					// SmartClient Tag operations
					LOGGER.debug("SMART TAG REQUEST CAUGHT: {} - not monitored", path);
				} else if ("/smartcomplete".equals(path)) {
					// SmartClient Autocomplete operations
					String attributeName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_attr")));
					String completeType = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_a")));
					result = smartComplete(attributeName, completeType);
				} else if ("/smartsearch".equals(path)) {
					// SmartClient Text Search operations
					LOGGER.debug("SMART SEARCH REQUEST CAUGHT: {} - not monitored", path);
				} else if ("/smartgen".equals(path)) {
					// SmartClient Generator operations
					LOGGER.debug("SMART GEN REQUEST CAUGHT: {} - not monitored", path);
				} else if (path.startsWith("/dynamic.")) {
					// Dynamic image requests
					LOGGER.debug("DYNAMIC IMAGE REQUEST CAUGHT: {} - not monitored", path);
				} else if (path.startsWith("/report/")) {
					// Report operations
					String moduleName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_mod")));
					String documentName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_doc")));
					String reportName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_n")));
					result = reportOperation(moduleName, documentName, reportName);
				} else if (path.startsWith("/bizexport.")) {
					// Bizport export operations
					String actionName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_n")));
					String document = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_doc")));
					result = bizportExport(actionName, document);
				} else if ("/download".equals(path)) {
					// Download operations
					String actionName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_n")));
					String document = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_doc")));
					result = downloadOperation(actionName, document);
				} else if ("/resources".equals(path) || "/images/resources".equals(path)) {
					// Customer resource operations
					LOGGER.debug("CUSTOMER RESOURCE REQUEST CAUGHT: {} - not monitored", path);
				} else if ("/image".equals(path)) {
					// jASPER IMAGE - caught but not monitored
					LOGGER.debug("JASPER IMAGE REQUEST CAUGHT: {} - not monitored", path);
				} else if ("/login".equals(path) || "/loggedOut".equals(path) || "/pages/login.jsp".equals(path)
						|| "/pages/loggedOut.jsp".equals(path)) {
					// Login/Logout operations - caught but not monitored
					LOGGER.debug("LOGIN/LOGOUT REQUEST CAUGHT: {} - not monitored", path);
					return NONE;
				} else if ("/health".equals(path)) {
					// Health check operations - caught but not monitored
					LOGGER.debug("HEALTH REQUEST CAUGHT: {} - not monitored", path);
					return NONE;
				} else if ("/meta".equals(path)) {
					// Metadata operations - caught but not monitored
					LOGGER.debug("META REQUEST CAUGHT: {} - not monitored", path);
					return NONE;
				} else {
					// Check if this is a standard page request (GET method to .xhtml)
					String method = request.getMethod();
					if ("GET".equals(method) && (path.endsWith(".xhtml"))) {
						LOGGER.debug("PAGE REQUEST CAUGHT: {} - not monitored", path);
					}
				}

				if (result == NONE) {
					LOGGER.warn("REQUEST SLIPPED THROUGH");
				}
			}

			return result;
		}

		public static RequestKey chart(String modelName) {
			return new RequestKey('H', null, null, modelName);
		}

		public static RequestKey create(String moduleName, String documentName) {
			return new RequestKey('C', moduleName, documentName, null);
		}

		public static RequestKey edit(String moduleName, String documentName) {
			return new RequestKey('E', moduleName, documentName, null);
		}

		public static RequestKey smartEdit(String moduleName, String documentName) {
			return new RequestKey('U', moduleName, documentName, null);
		}

		public static RequestKey documentList(String moduleName, String documentName) {
			return new RequestKey('Q', moduleName, documentName, null);
		}

		public static RequestKey queryist(String moduleName, String queryName) {
			return new RequestKey('Q', moduleName, null, queryName);
		}

		public static RequestKey modelList(String moduleName, String documentName, String modelName) {
			return new RequestKey('Q', moduleName, documentName, modelName);
		}

		public static RequestKey smartComplete(String attributeName, String completeType) {
			return new RequestKey('O', null, null, attributeName + "_" + completeType);
		}

		public static RequestKey smartClientList(String dataSource) {
			// Parse datasource to determine if it's a model, query, or document
			if (dataSource != null && dataSource.contains("_")) {
				String[] parts = dataSource.split("_");
				if (parts.length >= 2) {
					String module = parts[0];
					String secondPart = parts[1];

					// Check if it's a query (starts with 'q')
					if (secondPart.startsWith("q")) {
						String queryName = secondPart;
						return new RequestKey('Q', module, null, queryName);
					}
					// Check if it has a model part (3+ parts with double underscore pattern)
					else if (parts.length >= 3 && dataSource.contains("__")) {
						String documentName = secondPart;
						String modelName = dataSource.substring(dataSource.indexOf("__") + 2);
						return new RequestKey('Q', module, documentName, modelName);
					}
					// Otherwise it's a document
					else {
						String documentName = secondPart;
						return new RequestKey('Q', module, documentName, null);
					}
				}
			}

			// Fallback if parsing fails
			return new RequestKey('L', null, null, dataSource);
		}

		public static RequestKey map(String moduleName, String query, String geometryBinding) {
			return new RequestKey('P', moduleName, query, geometryBinding);
		}

		public static RequestKey reportOperation(String moduleName, String documentName, String reportName) {
			return new RequestKey('J', moduleName, documentName, reportName);
		}

		public static RequestKey bizportExport(String actionName, String document) {
			StringBuilder component = new StringBuilder();
			if (actionName != null) {
				component.append(actionName);
			}
			if (document != null) {
				if (component.length() > 0) {
					component.append("_");
				}
				component.append(document);
			}
			if (component.length() == 0) {
				component.append("unknown");
			}
			return new RequestKey('B', null, null, component.toString());
		}

		public static RequestKey downloadOperation(String actionName, String document) {
			StringBuilder component = new StringBuilder();
			if (actionName != null) {
				component.append(actionName);
			}
			if (document != null) {
				if (component.length() > 0) {
					component.append("_");
				}
				component.append(document);
			}
			if (component.length() == 0) {
				component.append("unknown");
			}
			return new RequestKey('W', null, null, component.toString());
		}

		@Override
		public String toString() {
			if (this == NONE) {
				return null;
			}

			StringBuilder result = new StringBuilder(128);
			result.append(type).append(moduleName);
			if (documentName != null) {
				result.append('.').append(documentName);
			}
			if (component != null) {
				result.append('^').append(component);
			}
			return result.toString();
		}

		public static @Nonnull RequestKey fromString(@Nonnull String keyCode) {
			if (keyCode == null || keyCode.length() < 1) {
				throw new IllegalArgumentException("KeyCode cannot be null or empty");
			}

			char type = keyCode.charAt(0);
			String remaining = keyCode.substring(1);

			int dotIndex = remaining.indexOf('.');
			int caretIndex = remaining.indexOf('^');

			String module = null;
			String document = null;
			String component = null;

			// Case 1: Has caret but no dot - format: {type}{module}^{component} or {type}^{component}
			if (caretIndex >= 0 && dotIndex < 0) {
				if (caretIndex > 0) {
					module = remaining.substring(0, caretIndex);
				}
				component = remaining.substring(caretIndex + 1);
			}
			// Case 2: Has dot but no caret - format: {type}{module}.{document}
			else if (dotIndex >= 0 && caretIndex < 0) {
				module = remaining.substring(0, dotIndex);
				document = remaining.substring(dotIndex + 1);
			}
			// Case 3: Has both dot and caret - format: {type}{module}.{document}^{component}
			else if (dotIndex >= 0 && caretIndex >= 0) {
				// Determine which comes first
				if (dotIndex < caretIndex) {
					// Normal case: module.document^component
					module = remaining.substring(0, dotIndex);
					document = remaining.substring(dotIndex + 1, caretIndex);
					component = remaining.substring(caretIndex + 1);
				} else {
					// Edge case: module^component.with.dots
					module = remaining.substring(0, caretIndex);
					component = remaining.substring(caretIndex + 1);
				}
			}
			// Case 4: No dot and no caret - format: {type}{module}
			else {
				if (remaining.length() > 0) {
					module = remaining;
				}
			}

			return new RequestKey(type, module, document, component);
		}

		public @Nonnull DomainValue toDomainValue() {
			return new DomainValue(toString());
		}

		public char getType() {
			return type;
		}

		public String getModuleName() {
			return moduleName;
		}

		public String getDocumentName() {
			return documentName;
		}

		public String getComponent() {
			return component;
		}
	}

	public static void measure(@Nonnull HttpServletRequest request,
			@Nonnull LocalDateTime currentDateTime,
			short memPctPre,
			int millis,
			int cpuDelta,
			short ramDelta, double sysLoad) {
		RESOURCE_MEASUREMENTS.updateMeasurements(currentDateTime, sysLoad, memPctPre);
		String requestKey = RequestKey.from(request).toString();
		if (requestKey != null) {
			LOGGER.debug("Request key: {}", requestKey);
			RequestMeasurements rm = REQUEST_MEASUREMENTS.computeIfAbsent(requestKey, k -> new RequestMeasurements());
			rm.updateMeasurements(currentDateTime, millis, cpuDelta, ramDelta);
			LOGGER.debug("Request measurements: {}", rm);
		}
		LOGGER.debug("Resource measurements: {}", RESOURCE_MEASUREMENTS);
	}

	public static @Nonnull ResourceMeasurements getResourceMeasurements() {
		return RESOURCE_MEASUREMENTS;
	}

	/**
	 * Gets the monitoring start time in milliseconds since epoch.
	 * This can be used as the reference time for charting relative timestamps.
	 * 
	 * @return the start time when monitoring began collecting data
	 */
	public static long getMonitoringStartTime() {
		return MONITORING_START_TIME;
	}

	public static @Nonnull Set<String> getRequestKeyCodes() {
		return REQUEST_MEASUREMENTS.keySet();
	}

	public static @Nullable RequestMeasurements getRequestMeasurements(String requestKeyCode) {
		return REQUEST_MEASUREMENTS.get(requestKeyCode);
	}

	public static @Nonnull List<DomainValue> toDomainValues() {
		Set<String> keys = REQUEST_MEASUREMENTS.keySet();
		List<DomainValue> result = new ArrayList<>(keys.size());
		for (String key : keys) {
			result.add(RequestKey.fromString(key).toDomainValue());
		}

		return result;
	}

	/**
	 * Extract a parameter value from a URL string.
	 * Handles both query parameters (?param=value) and URL fragments (#param=value).
	 */
	private static String extractParameterFromUrl(String url, String parameterName) {
		if (url == null || parameterName == null) {
			return null;
		}

		// Look for parameter in query string (after ?)
		String paramPattern = parameterName + "=";
		int paramIndex = url.indexOf("?" + paramPattern);
		if (paramIndex == -1) {
			paramIndex = url.indexOf("&" + paramPattern);
		}
		if (paramIndex == -1) {
			// Look for parameter in fragment (after #)
			paramIndex = url.indexOf("#" + paramPattern);
		}

		if (paramIndex != -1) {
			int valueStart = paramIndex + paramPattern.length() + 1; // +1 for ? or & or #
			if (valueStart < url.length()) {
				int valueEnd = url.indexOf('&', valueStart);
				if (valueEnd == -1) {
					valueEnd = url.indexOf('#', valueStart);
				}
				if (valueEnd == -1) {
					valueEnd = url.length();
				}
				return url.substring(valueStart, valueEnd);
			}
		}

		return null;
	}
}
