package org.skyve.impl.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.PersistentMetaData;
import org.skyve.impl.util.XMLMetaData;

@XmlRootElement(namespace = XMLMetaData.ROUTER_NAMESPACE)
@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE)
public class Router implements PersistentMetaData<Router> {
	private static final long serialVersionUID = 670690452538129424L;

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

	@XmlTransient
	private List<RouteCriteria> unsecuredRoutes = new ArrayList<>();

	List<RouteCriteria> getUnsecuredRoutes() {
		return unsecuredRoutes;
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
			uxuiSelector = (TaggingUxUiSelector) type.newInstance();
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
	
	public boolean isUnsecured(RouteCriteria routeToTest) {
		if (routeToTest != null) {
			for (RouteCriteria unsecuredRoute : unsecuredRoutes) {
	        	if (unsecuredRoute.matches(routeToTest)) {
	        		return true;
	        	}
	        }
		}
        return false;
	}
	
	@Override
	public Router convert(String metaDataName, AbstractRepository repository) {
		for (UxUiMetadata uxui : uxuis) {
			uxuiMap.put(uxui.getName(), uxui);
		}
		
		return this;
	}
}
