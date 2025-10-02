package modules.admin.MonitoringDashboard;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateTime;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.SingletonCachedBizlet;
import org.skyve.util.Monitoring;
import org.skyve.web.WebContext;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.RequestType;

public class MonitoringDashboardBizlet extends SingletonCachedBizlet<MonitoringDashboard> {

	@Override
	public MonitoringDashboard preExecute(ImplicitActionName actionName, MonitoringDashboard bean, Bean parentBean,
			WebContext webContext) throws Exception {
		if (ImplicitActionName.Edit.equals(actionName) || ImplicitActionName.New.equals(actionName)) {
			// Set the monitoring start time
			bean.setMonitoringStartTime(new DateTime(Monitoring.getMonitoringStartTime()));
		}
		return super.preExecute(actionName, bean, parentBean, webContext);
	}

	@Override
	public void preRerender(String source, MonitoringDashboard bean, WebContext webContext) throws Exception {
		if (MonitoringDashboard.rsModuleNamePropertyName.equals(source)
				&& (bean.getRsDocumentName() != null || bean.getRsComponentName() != null)) {
			bean.setRsDocumentName(null);
			bean.setRsComponentName(null);
		} else if (MonitoringDashboard.rsDocumentNamePropertyName.equals(source) && bean.getRsComponentName() != null) {
			bean.setRsComponentName(null);
		}
		super.preRerender(source, bean, webContext);
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, MonitoringDashboard bean) throws Exception {
		List<DomainValue> results = new ArrayList<>();

		if (MonitoringDashboard.documentNamePropertyName.equals(attributeName)) {
			// Get all request key codes from monitoring to check what documents have data
			Set<String> requestKeyCodes = Monitoring.getRequestKeyCodes();
			Set<String> documentsWithData = requestKeyCodes.stream()
					.map(keyCode -> {
						Monitoring.RequestKey requestKey = Monitoring.RequestKey.fromString(keyCode);
						String moduleName = requestKey.getModuleName();
						String documentName = requestKey.getDocumentName();
						return (moduleName != null && documentName != null) ? moduleName + "." + documentName : null;
					})
					.filter(docRef -> docRef != null)
					.collect(Collectors.toSet());

			// Only add documents that have monitoring data
			CORE.getCustomer()
					.getModules()
					.stream()
					.forEach(m -> m.getDocumentRefs()
							.forEach((dName, dRef) -> {
								String docRef = m.getName() + "." + dName;
								if (documentsWithData.contains(docRef)) {
									results.add(new DomainValue(docRef));
								}
							}));
			return results;
		} else if (MonitoringDashboard.queryNamePropertyName.equals(attributeName)) {
			// Get all request key codes from monitoring to check what queries have data
			Set<String> requestKeyCodes = Monitoring.getRequestKeyCodes();
			Set<String> queriesWithData = requestKeyCodes.stream()
					.map(keyCode -> {
						Monitoring.RequestKey requestKey = Monitoring.RequestKey.fromString(keyCode);
						String moduleName = requestKey.getModuleName();
						String component = requestKey.getComponent();
						// For queries, the component field contains the query name
						return (moduleName != null && component != null) ? moduleName + "." + component : null;
					})
					.filter(queryRef -> queryRef != null)
					.collect(Collectors.toSet());

			// Only add queries that have monitoring data
			CORE.getCustomer()
					.getModules()
					.stream()
					.forEach(m -> m.getMetadataQueries()
							.forEach(query -> {
								String queryRef = m.getName() + "." + query.getName();
								if (queriesWithData.contains(queryRef)) {
									results.add(new DomainValue(queryRef));
								}
							}));
			return results;
		} else if (MonitoringDashboard.requestTypePropertyName.equals(attributeName) ||
				MonitoringDashboard.rsRequestTypePropertyName.equals(attributeName)) {
			// Get all request key codes from monitoring to check what request types have data
			Set<String> requestKeyCodes = Monitoring.getRequestKeyCodes();
			Set<Character> requestTypesWithData = requestKeyCodes.stream()
					.map(keyCode -> keyCode.charAt(0)) // First character is the request type
					.collect(Collectors.toSet());

			// Only add request types that have monitoring data
			for (Character requestTypeChar : requestTypesWithData) {
				RequestType type = RequestType.fromCode(requestTypeChar.toString());
				results.add(type.toDomainValue());
			}
			if (MonitoringDashboard.requestTypePropertyName.equals(attributeName)) {
				results.add(RequestType.all.toDomainValue());
			}
			return results;
		}

		return super.getDynamicDomainValues(attributeName, bean);
	}

	private enum RequestValueType {
		MODULE, DOCUMENT, COMPONENT
	}

	@Override
	public List<String> complete(String attributeName, String value, MonitoringDashboard bean) throws Exception {
		MonitoringDashboard workingBean = bean;
		// Request stats autocomplete values
		if ("rsModuleName".equals(attributeName)) {
			String requestType = workingBean.getRsRequestType() != null ? workingBean.getRsRequestType().toCode() : null;
			return getFilteredRequestStrings(requestType, RequestValueType.MODULE, value, workingBean);
		} else if ("rsDocumentName".equals(attributeName)) {
			String requestType = workingBean.getRsRequestType() != null ? workingBean.getRsRequestType().toCode() : null;
			return getFilteredRequestStrings(requestType, RequestValueType.DOCUMENT, value, workingBean);
		} else if ("rsComponentName".equals(attributeName)) {
			String requestType = workingBean.getRsRequestType() != null ? workingBean.getRsRequestType().toCode() : null;
			return getFilteredRequestStrings(requestType, RequestValueType.COMPONENT, value, workingBean);
		}
		return super.complete(attributeName, value, workingBean);
	}

	/**
	 * Get filtered request values as strings for autocomplete functionality
	 */
	private List<String> getFilteredRequestStrings(String requestType, RequestValueType valueType, String searchValue,
			MonitoringDashboard bean) {
		List<String> results = new ArrayList<>();

		// If no request type selected or "all" selected, return empty list
		if (requestType == null || "all".equals(requestType)) {
			return results;
		}

		// Get current form selections for filtering
		String selectedModule = bean.getRsModuleName();
		String selectedDocument = bean.getRsDocumentName();

		// For documents and components, require module selection
		if ((valueType == RequestValueType.DOCUMENT || valueType == RequestValueType.COMPONENT) &&
				(selectedModule == null || selectedModule.trim().isEmpty())) {
			return results; // Return empty if no module selected
		}

		// For components, can also filter by document if selected
		// (but component filtering works with just module too)

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

			// Apply module/document filtering based on value type
			boolean includeThisKey = true;

			switch (valueType) {
				case MODULE:
					// No additional filtering for modules
					break;
				case DOCUMENT:
					// Only include if module matches selected module
					if (!selectedModule.equals(requestKey.getModuleName())) {
						includeThisKey = false;
					}
					break;
				case COMPONENT:
					// Include if module matches, and optionally document matches
					if (!selectedModule.equals(requestKey.getModuleName())) {
						includeThisKey = false;
					} else if (selectedDocument != null && !selectedDocument.trim().isEmpty() &&
							!selectedDocument.equals(requestKey.getDocumentName())) {
						includeThisKey = false;
					}
					break;
				default:
					break;
			}

			if (includeThisKey) {
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
		}

		return results;
	}

}
