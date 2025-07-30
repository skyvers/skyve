package org.skyve.impl.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.impl.metadata.repository.ConvertibleMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.ReloadableMetaData;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(namespace = XMLMetaData.ROUTER_NAMESPACE)
@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE, propOrder = {"uxuis", "unsecuredUrlPrefixes", "properties"})
public class Router implements ConvertibleMetaData<Router>, DecoratedMetaData, ReloadableMetaData {
	private static final long serialVersionUID = 670690452538129424L;

	private long lastModifiedMillis = Long.MAX_VALUE;
	private long lastCheckedMillis = System.currentTimeMillis();
	
	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	
	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	@XmlTransient
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}

	@Override
	public long getLastCheckedMillis() {
		return lastCheckedMillis;
	}

	@Override
	@XmlTransient
	public void setLastCheckedMillis(long lastCheckedMillis) {
		this.lastCheckedMillis = lastCheckedMillis;
	}

	private String uxuiSelectorClassName;
	private TaggingUxUiSelector uxuiSelector;
	
	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE, name = "uxui")
	private List<UxUiMetadata> uxuis = new ArrayList<>();

	@XmlTransient
	private Map<String, UxUiMetadata> uxuiMap = new TreeMap<>();

	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE, name = "unsecured")
	private Set<String> unsecuredUrlPrefixes = new TreeSet<>();

	public Set<String> getUnsecuredUrlPrefixes() {
		return unsecuredUrlPrefixes;
	}

	public String getUxuiSelectorClassName() {
		return uxuiSelectorClassName;
	}
	@XmlAttribute(name = "uxuiSelectorClassName")
	public void setUxuiSelectorClassName(String uxuiSelectorClassName) {
		this.uxuiSelectorClassName = uxuiSelectorClassName;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
	
	public TaggingUxUiSelector getUxuiSelector() throws Exception {
		if (uxuiSelector == null) {
			Class<?> type = Thread.currentThread().getContextClassLoader().loadClass(uxuiSelectorClassName);
			uxuiSelector = (TaggingUxUiSelector) type.getDeclaredConstructor().newInstance();
		}
		return uxuiSelector;
	}

	public List<UxUiMetadata> getUxUis() {
		return uxuis;
	}

	public String selectOutcomeUrl(String uxui, RouteCriteria criteria) {
		Route route = selectRoute(uxui, criteria);
		return (route == null) ? null : route.getOutcomeUrl();
	}
	
	public Route selectRoute(String uxui, RouteCriteria criteria) {
		UxUiMetadata selectedUxUi = uxuiMap.get(uxui);
		if (selectedUxUi != null) {
			for (Route route : selectedUxUi.getRoutes()) {
				List<RouteCriteria> routeCriteria = route.getCriteria();
				if (routeCriteria.isEmpty()) { // if no criteria defined then its a match
					return route;
				}
				for (RouteCriteria routeCriterion : routeCriteria) {
					if (routeCriterion.matches(criteria)) {
						return route;
					}
				}
			}
		}
		
		return null;
	}
	
	public boolean isUnsecured(String urlPrefixToTest) {
		if (urlPrefixToTest != null) {
			for (String unsecuredURLPrefix : unsecuredUrlPrefixes) {
	        	if (urlPrefixToTest.startsWith(unsecuredURLPrefix)) {
	        		return true;
	        	}
	        }
		}
        return false;
	}
	
	@Override
	public Router convert(String metaDataName) {
		// populate the UX/UI map
		for (UxUiMetadata uxui : uxuis) {
			uxuiMap.put(uxui.getName(), uxui);
		}
		
		// trim the unsecured URL prefixes if required
		for (String unsecuredUrlPrefix : unsecuredUrlPrefixes) {
			String trimmed = Util.processStringValue(unsecuredUrlPrefix);
			if (trimmed == null) {
				unsecuredUrlPrefixes.remove(unsecuredUrlPrefix);
			}
			else if (trimmed.length() < unsecuredUrlPrefix.length()) {
				unsecuredUrlPrefixes.remove(unsecuredUrlPrefix);
				unsecuredUrlPrefixes.add(trimmed);
			}
		}
		
		return this;
	}
	
	/**
	 * Merges a router into this router.
	 * If the route exists already then the route criteria are added.
	 * If the route does not exist already, it is inserted in the order defined at the top of the list.
	 * Thus the broadest routes should go in the global router and the module router should be mainly for public pages. 
	 * @param routerToMerge	Router to merge into this router.
	 */
	public void merge(Router routerToMerge) {
		// Set the last modified to the latest timestamp of any router to be merged
		setLastModifiedMillis(Math.max(getLastModifiedMillis(), routerToMerge.getLastModifiedMillis()));

		// Merge UX/UIs
		final List<UxUiMetadata> uxuiMetadata = routerToMerge.getUxUis();
		final List<UxUiMetadata> newUxUis = new ArrayList<>();
		for (UxUiMetadata uxuiMetadatum : uxuiMetadata) {
			final UxUiMetadata existingUxuiMetadatum = uxuis.stream()
					.filter(uxui -> Objects.equals(uxui.getName(), uxuiMetadatum.getName()))
					.findFirst().orElse(null);
			if (existingUxuiMetadatum == null) {
				newUxUis.add(uxuiMetadatum);
			}
			else {
				// Module routers should not mask the generic routes from the global router.
				// To this end we ensure that at least the module criteria is defined.
				List<Route> routesToMerge = uxuiMetadatum.getRoutes();
				for (Route route : routesToMerge) {
					for (RouteCriteria criteria : route.getCriteria()) {
						if (criteria.getModuleName() == null) {
							throw new MetaDataException("Merged router with UX/UI " + uxuiMetadatum.getName() + " and outcomeUrl" + route.getOutcomeUrl() + 
															" has a criteria without a module name defined." + 
															" Module routers should be for the module when adding to existing UX/UI definitions." + 
															" Add the non-module route to the global router.");
						}
					}
				}

				// Insert the routes at the front and merge the properties in
				existingUxuiMetadatum.getRoutes().addAll(0, uxuiMetadatum.getRoutes());
				existingUxuiMetadatum.getProperties().putAll(uxuiMetadatum.getProperties());
			}
		}
		uxuis.addAll(newUxUis);

		// Add any extra unsecured URL prefixes
		unsecuredUrlPrefixes.addAll(routerToMerge.getUnsecuredUrlPrefixes());
	}
}
