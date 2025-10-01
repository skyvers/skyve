package modules.admin.MonitoringDashboard;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.metadata.model.document.SingletonCachedBizlet;
import org.skyve.util.Monitoring;

import modules.admin.domain.MonitoringDashboard;

public class MonitoringDashboardBizlet extends SingletonCachedBizlet<MonitoringDashboard> {

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, MonitoringDashboard bean) throws Exception {
		List<DomainValue> results = new ArrayList<>();

		if (MonitoringDashboard.documentNamePropertyName.equals(attributeName)) {
			CORE.getCustomer()
					.getModules()
					.stream()
					.forEach(m -> m.getDocumentRefs()
							.forEach((dName, dRef) -> results.add(new DomainValue(m.getName() + "." + dName))));
			return results;
		} else if (MonitoringDashboard.queryNamePropertyName.equals(attributeName)) {
			CORE.getCustomer()
					.getModules()
					.stream()
					.forEach(m -> m.getMetadataQueries()
							.forEach(query -> results.add(new DomainValue(m.getName() + "." + query.getName()))));
			return results;
		}
		// Request stats dynamic domain values
		/*else if ("rsModuleName".equals(attributeName)) {
			String requestType = bean.getRsRequestType() != null ? bean.getRsRequestType().toCode() : null;
			return getFilteredRequestValues(requestType, RequestValueType.MODULE);
		} else if ("rsDocumentName".equals(attributeName)) {
			String requestType = bean.getRsRequestType() != null ? bean.getRsRequestType().toCode() : null;
			return getFilteredRequestValues(requestType, RequestValueType.DOCUMENT);
		} else if ("rsComponentName".equals(attributeName)) {
			String requestType = bean.getRsRequestType() != null ? bean.getRsRequestType().toCode() : null;
			return getFilteredRequestValues(requestType, RequestValueType.COMPONENT);
		}*/

		return super.getDynamicDomainValues(attributeName, bean);
	}

	private enum RequestValueType {
		MODULE, DOCUMENT, COMPONENT
	}

	@Override
	public List<String> complete(String attributeName, String value, MonitoringDashboard bean) throws Exception {
		// Request stats autocomplete values
		if ("rsModuleName".equals(attributeName)) {
			String requestType = bean.getRsRequestType() != null ? bean.getRsRequestType().toCode() : null;
			return getFilteredRequestStrings(requestType, RequestValueType.MODULE, value);
		} else if ("rsDocumentName".equals(attributeName)) {
			String requestType = bean.getRsRequestType() != null ? bean.getRsRequestType().toCode() : null;
			return getFilteredRequestStrings(requestType, RequestValueType.DOCUMENT, value);
		} else if ("rsComponentName".equals(attributeName)) {
			String requestType = bean.getRsRequestType() != null ? bean.getRsRequestType().toCode() : null;
			return getFilteredRequestStrings(requestType, RequestValueType.COMPONENT, value);
		}
		return super.complete(attributeName, value, bean);
	}

	/**
	 * Get domain values for request stats fields filtered by request type
	 */
	private List<DomainValue> getFilteredRequestValues(String requestType, RequestValueType valueType) {
		List<DomainValue> results = new ArrayList<>();

		if (requestType == null || "all".equals(requestType)) {
			// If no request type selected or "all" selected, return all available values
			return getAllRequestValues(valueType);
		}

		// Get all request key codes from monitoring
		Set<String> requestKeyCodes = Monitoring.getRequestKeyCodes();

		// Filter by request type (first character of the key code)
		char typeChar = requestType.charAt(0);
		Set<String> filteredKeys = requestKeyCodes.stream()
				.filter(keyCode -> keyCode.charAt(0) == typeChar)
				.collect(Collectors.toSet());

		// Extract the specific values based on type
		for (String keyCode : filteredKeys) {
			Monitoring.RequestKey requestKey = Monitoring.RequestKey.fromString(keyCode);

			final String value = switch (valueType) {
				case MODULE -> requestKey.getModuleName();
				case DOCUMENT -> requestKey.getDocumentName();
				case COMPONENT -> requestKey.getComponent();
			};

			// Add non-null, unique values
			if (value != null && results.stream().noneMatch(dv -> value.equals(dv.getCode()))) {
				results.add(new DomainValue(value));
			}
		}

		return results;
	}

	/**
	 * Get all available values for a request value type (used when no specific request type is selected)
	 */
	private List<DomainValue> getAllRequestValues(RequestValueType valueType) {
		List<DomainValue> results = new ArrayList<>();
		Set<String> requestKeyCodes = Monitoring.getRequestKeyCodes();

		for (String keyCode : requestKeyCodes) {
			Monitoring.RequestKey requestKey = Monitoring.RequestKey.fromString(keyCode);

			final String value = switch (valueType) {
				case MODULE -> requestKey.getModuleName();
				case DOCUMENT -> requestKey.getDocumentName();
				case COMPONENT -> requestKey.getComponent();
			};

			// Add non-null, unique values
			if (value != null && results.stream().noneMatch(dv -> value.equals(dv.getCode()))) {
				results.add(new DomainValue(value));
			}
		}

		return results;
	}

	/**
	 * Get filtered request values as strings for autocomplete functionality
	 */
	private List<String> getFilteredRequestStrings(String requestType, RequestValueType valueType, String searchValue) {
		List<String> results = new ArrayList<>();
		
		if (requestType == null || "all".equals(requestType)) {
			// If no request type selected or "all" selected, return all available values
			return getAllRequestStrings(valueType, searchValue);
		}

		// Get all request key codes from monitoring
		Set<String> requestKeyCodes = Monitoring.getRequestKeyCodes();

		// Filter by request type (first character of the key code)
		char typeChar = requestType.charAt(0);
		Set<String> filteredKeys = requestKeyCodes.stream()
				.filter(keyCode -> keyCode.charAt(0) == typeChar)
				.collect(Collectors.toSet());

		// Extract the specific values based on type
		for (String keyCode : filteredKeys) {
			Monitoring.RequestKey requestKey = Monitoring.RequestKey.fromString(keyCode);

			final String value = switch (valueType) {
				case MODULE -> requestKey.getModuleName();
				case DOCUMENT -> requestKey.getDocumentName();
				case COMPONENT -> requestKey.getComponent();
			};

			// Add non-null, unique values that match the search criteria
			if (value != null && !results.contains(value)) {
				// Filter based on search value (case-insensitive contains)
				if (searchValue == null || searchValue.trim().isEmpty() || 
					value.toLowerCase().contains(searchValue.toLowerCase())) {
					results.add(value);
				}
			}
		}

		return results;
	}

	/**
	 * Get all available string values for a request value type with search filtering
	 */
	private List<String> getAllRequestStrings(RequestValueType valueType, String searchValue) {
		List<String> results = new ArrayList<>();
		Set<String> requestKeyCodes = Monitoring.getRequestKeyCodes();

		for (String keyCode : requestKeyCodes) {
			Monitoring.RequestKey requestKey = Monitoring.RequestKey.fromString(keyCode);

			final String value = switch (valueType) {
				case MODULE -> requestKey.getModuleName();
				case DOCUMENT -> requestKey.getDocumentName();
				case COMPONENT -> requestKey.getComponent();
			};

			// Add non-null, unique values that match the search criteria
			if (value != null && !results.contains(value)) {
				// Filter based on search value (case-insensitive contains)
				if (searchValue == null || searchValue.trim().isEmpty() || 
					value.toLowerCase().contains(searchValue.toLowerCase())) {
					results.add(value);
				}
			}
		}

		return results;
	}
}
