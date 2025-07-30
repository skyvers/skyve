package org.skyve.impl.metadata.repository.customer;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE,
			name = "modules",
			propOrder = {"homeModule", "modules", "properties"})
public class CustomerModulesMetaData implements DecoratedMetaData {
	private static final long serialVersionUID = 5127869840728920122L;

	private List<CustomerModuleMetaData> modules = new ArrayList<>();
	private String homeModule;
	
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "module", required = true)
	public List<CustomerModuleMetaData> getModules() {
		return modules;
	}
	
	public String getHomeModule() {
		return homeModule;
	}

	@XmlAttribute(required = true)
	public void setHomeModule(String homeModule) {
		this.homeModule = UtilImpl.processStringValue(homeModule);
	}
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
