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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;

/**
 * System monitoring functions.
 */
public class Monitoring {
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
	
	public static int percentageUsedMemory() {
		Runtime runtime = Runtime.getRuntime();
		long total = runtime.totalMemory();
		long free = runtime.freeMemory();
		return (int) ((total - free) / (double) total * 100d);
	}
	
	private static final ConcurrentHashMap<String, RequestMeasurements> REQUEST_MEASUREMENTS = new ConcurrentHashMap<>();
	private static final ResourceMeasurements RESOURCE_MEASUREMENTS = new ResourceMeasurements();
	
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
			
			String a = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("a")));
			if ("e".equals(a)) {
				String m = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("m")));
				String d = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("d")));
				if ((m != null && d != null)) {
					String i = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("i")));
					result = (i == null) ? create(m, d) : edit(m, d);
				}
System.out.println("a=e REQUEST SLIPPED THROUGH");
			}
			else if ("l".equals(a)) {
				String m = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("m")));
				String q = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("q")));
				if (m != null) {
					if (q != null) {
						String d = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter("d")));
						if (d == null) {
							result = queryist(m, q);
						}
						result = modelList(m, d, q);
					}
				}
System.out.println("a=l REQUEST SLIPPED THROUGH");
			}
			else if ("m".equals(a)) {
System.out.println("a=m REQUEST SLIPPED THROUGH");
			}
			// etc etc
			else { // SC
				String path = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getServletPath()));
				if ("/smartedit".equals(path)) {
					String m = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter(AbstractWebContext.MODULE_NAME)));
					String d = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter(AbstractWebContext.DOCUMENT_NAME)));
					if ((m != null) && (d != null)) {
						String i = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(request.getParameter(AbstractWebContext.ID_NAME)));
						result = (i == null) ? create(m, d) : edit(m, d);
					}
				}
System.out.println("REQUEST SLIPPED THROUGH");
			}
			
			return result;
		}
		
		public static RequestKey create(String moduleName, String documentName) {
			return new RequestKey('C', moduleName, documentName, null);
		}

		public static RequestKey edit(String moduleName, String documentName) {
			return new RequestKey('E', moduleName, documentName, null);
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
			}
			else {
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
								double cpu,
								int ram,
								int millis,
								double cpuDelta,
								int ramDelta) {
		RESOURCE_MEASUREMENTS.updateMeasurements(currentDateTime, cpu, ram);
		String requestKey = RequestKey.from(request).toString();
		if (requestKey != null) {
System.out.println(requestKey);
			RequestMeasurements rm = REQUEST_MEASUREMENTS.computeIfAbsent(requestKey, k -> new RequestMeasurements());
			rm.updateMeasurements(currentDateTime, millis, cpuDelta, ramDelta);
System.out.println(rm);
		}
System.out.println(RESOURCE_MEASUREMENTS);
	}
	
	public static @Nonnull ResourceMeasurements getResourceMeasurements() {
		return RESOURCE_MEASUREMENTS;
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
}
