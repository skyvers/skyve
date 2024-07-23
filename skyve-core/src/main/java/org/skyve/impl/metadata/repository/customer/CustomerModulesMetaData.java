package org.skyve.impl.metadata.repository.customer;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "modules")
public class CustomerModulesMetaData {
	private List<CustomerModuleMetaData> modules = new ArrayList<>();
	private String homeModule;
	
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
}
