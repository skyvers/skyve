package org.skyve.web;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public enum UserAgentType {
	phone(true), tablet(true), desktop(false), other(false);
	
	private boolean mobile;
	private UserAgentType(boolean mobile) {
		this.mobile = mobile;
	}
	
	public boolean isMobile() {
		return mobile;
	}
}

