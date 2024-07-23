package org.skyve.impl.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.impl.metadata.repository.ConvertableMetaData;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.ROUTER_NAMESPACE)
@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE, propOrder = {"uxuis", "unsecuredUrlPrefixes"})
public class Router implements ConvertableMetaData<Router> {
	private static final long serialVersionUID = 670690452538129424L;

	private long lastModifiedMillis = Long.MAX_VALUE;
	
	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	@XmlTransient
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
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
				for (RouteCriteria routeCriterium : routeCriteria) {
					if (routeCriterium.matches(criteria)) {
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
	public Router convert(String metaDataName, ProvidedRepository repository) {
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
				for (int i = 0; i < uxuiMetadatum.getRoutes().size(); i++) {
					final Route routeToMerge = uxuiMetadatum.getRoutes().get(i);
					final Route existingRoute = existingUxuiMetadatum.getRoutes().stream()
							.filter(route -> Objects.equals(route.getOutcomeUrl(), routeToMerge.getOutcomeUrl()))
							.findFirst().orElse(null);
					if (existingRoute == null) {
						existingUxuiMetadatum.getRoutes().add(i, routeToMerge);
					}
					else {
						existingRoute.getCriteria().addAll(routeToMerge.getCriteria());
						existingRoute.getProperties().putAll(routeToMerge.getProperties());
					}
				}
			}
		}
		uxuis.addAll(newUxUis);

		// Add any extra unsecured URL prefixes
		unsecuredUrlPrefixes.addAll(routerToMerge.getUnsecuredUrlPrefixes());
	}
}
