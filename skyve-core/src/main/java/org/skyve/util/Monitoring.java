package org.skyve.util;

import java.lang.management.ManagementFactory;
import java.lang.management.OperatingSystemMXBean;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
				String servletPath = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getServletPath()));
				String sourceComponent = OWASP.sanitise(Sanitisation.text,
						UtilImpl.processStringValue(request.getParameter("jakarta.faces.source")));

				if (servletPath != null) {
					if (sourceComponent != null && !sourceComponent.isEmpty()) {
						result = primeFacesAjax(servletPath, sourceComponent);
					} else {
						result = primeFacesAjax(servletPath);
					}
				}
			}

			if (result != NONE) {
				return result;
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
					String binding = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("_b")));
					String docName = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("_doc")));
					if (binding != null && docName != null) {
						result = contentRequest(docName, binding);
					}
				} else if ("/sse/stream".equals(path) || path.startsWith("/sse")) {
					// Ignore SSE requests - return NONE to skip monitoring
					return NONE;
				} else if ("/smartlist".equals(path)) {
					String dataSource = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_dataSource")));
					result = smartClientList(dataSource);
				} else if ("/smartsnap".equals(path)) {
					// SmartClient Snap operations
					String action = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("a")));
					String snapshotName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("n")));
					String dataSource = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("d")));
					if (action != null) {
						String requestDescription = Stream.of(action, snapshotName, dataSource)
								.filter(Objects::nonNull) // keep only non-nulls
								.collect(Collectors.joining("_"));
						result = smartClientSnap(requestDescription);
					}
				} else if ("/smarttag".equals(path)) {
					// SmartClient Tag operations
					String action = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("a")));
					String tagName = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("n")));
					String dataSource = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("d")));

					// Build description based on action type
					String actionDescription;
					switch (action != null ? action : "") {
						case "L":
							actionDescription = "list_tags";
							break;
						case "T":
							actionDescription = "tag_items";
							break;
						case "U":
							actionDescription = "untag_items";
							break;
						case "C":
							actionDescription = "clear_tagged";
							break;
						case "N":
							actionDescription = "new_tag";
							break;
						default:
							actionDescription = action != null ? action : "unknown";
					}

					// Build complete request description
					StringBuilder requestDescription = new StringBuilder(actionDescription);

					// Add tagName for new tag operations
					if ("N".equals(action) && tagName != null) {
						requestDescription.append("_").append(tagName);
					}

					// Add dataSource for tag/untag operations where available
					if (("T".equals(action) || "U".equals(action)) && dataSource != null) {
						requestDescription.append("_").append(dataSource);
					}

					result = smartClientTag(requestDescription.toString());
				} else if ("/smartcomplete".equals(path)) {
					// SmartClient Autocomplete operations
					String attributeName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_attr")));
					String completeType = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_a")));
					result = smartComplete(attributeName, completeType);
				} else if ("/smartsearch".equals(path)) {
					// SmartClient Text Search operations
					String query = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("query")));
					result = smartSearch(
							"search" + (query != null ? ":" + query.substring(0, Math.min(20, query.length())) : ""));
				} else if ("/smartgen".equals(path)) {
					// SmartClient Generator operations
					String moduleName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_mod")));
					String documentName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_doc")));
					if (moduleName != null && documentName != null) {
						result = smartClientGenerate(moduleName, documentName);
					}
				} else if (path.startsWith("/dynamic.")) {
					// Dynamic image requests
					String dynamicImageName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_n")));
					String document = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_doc")));
					result = dynamicImage(dynamicImageName, document);
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
					String resourceName = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_n")));
					String resourceDocument = OWASP.sanitise(Sanitisation.text,
							UtilImpl.processStringValue(request.getParameter("_doc")));
					result = customerResource(resourceName, resourceDocument);
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
						result = pageRequest(path);
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

		public static RequestKey smartSearch(String query) {
			return new RequestKey('S', null, null, query);
		}

		private static RequestKey smartClientSnap(String requestDescription) {

			return new RequestKey('Z', null, null, "snap_" + requestDescription);
		}

		public static RequestKey smartClientGenerate(String moduleName, String documentName) {
			return new RequestKey('G', moduleName, documentName, null);
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

		public static RequestKey smartClientTag(String requestDescription) {
			return new RequestKey('T', null, null, "tag_" + requestDescription);
		}

		public static RequestKey contentRequest(String documentName, String binding) {
			return new RequestKey('R', null, documentName, binding);
		}

		public static RequestKey primeFacesAjax(String servletPath) {
			return new RequestKey('A', null, null, servletPath);
		}

		public static RequestKey primeFacesAjax(String servletPath, String sourceComponent) {
			return new RequestKey('A', null, null, servletPath + ":" + sourceComponent);
		}

		public static RequestKey pageRequest(String servletPath) {
			return new RequestKey('N', null, null, servletPath);
		}

		public static RequestKey dynamicImage(String dynamicImageName, String document) {
			StringBuilder component = new StringBuilder();
			if (dynamicImageName != null) {
				component.append(dynamicImageName);
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
			return new RequestKey('D', null, null, component.toString());
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

		public static RequestKey customerResource(String resourceName, String resourceDocument) {
			StringBuilder component = new StringBuilder();
			if (resourceName != null) {
				component.append(resourceName);
			}
			if (resourceDocument != null) {
				if (component.length() > 0) {
					component.append("_");
				}
				component.append(resourceDocument);
			}
			if (component.length() == 0) {
				component.append("unknown");
			}
			return new RequestKey('V', null, null, component.toString());
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
			int dotIndex = keyCode.indexOf('.');
			int caretIndex = keyCode.indexOf('^');
			String module = (dotIndex > 0) ? keyCode.substring(1, dotIndex) : null;
			String document = null;
			String component = null;
			if (module == null) {
				module = keyCode.substring(1, caretIndex);
				component = keyCode.substring(caretIndex + 1);
			} else {
				document = keyCode.substring(dotIndex + 1, caretIndex);
				if (caretIndex > 0) {
					component = keyCode.substring(caretIndex + 1);
				}
			}
			return new RequestKey(keyCode.charAt(0), module, document, component);
		}

		public @Nonnull DomainValue toDomainValue() {
			return new DomainValue(toString());
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
