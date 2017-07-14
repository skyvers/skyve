package org.skyve.impl.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.PersistentMetaData;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;

@XmlRootElement(namespace = XMLMetaData.ROUTER_NAMESPACE)
@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE)
public class Router implements MetaData, PersistentMetaData<Router> {
	private static final long serialVersionUID = 670690452538129424L;

	private String uxuiSelectorClassName;
	private TaggingUxUiSelector uxuiSelector;
	
	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE, name = "uxui", required = true)
	private List<UxUiMetadata> uxuis = new ArrayList<>();

	@XmlTransient
	private Map<String, UxUiMetadata> uxuiMap = new TreeMap<>();
	
	public String getUxuiSelectorClassName() {
		return uxuiSelectorClassName;
	}
	@XmlAttribute(name = "uxuiSelectorClassName", required = true)
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
	
	@Override
	public Router convert(String metaDataName) {
		// TODO look at conversion 
		
		for (UxUiMetadata uxui : uxuis) {
			uxuiMap.put(uxui.getName(), uxui);
		}
		
		return this;
	}
}
