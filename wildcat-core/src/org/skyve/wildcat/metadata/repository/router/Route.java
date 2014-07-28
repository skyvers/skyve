package org.skyve.wildcat.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaData;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.ROUTER_NAMESPACE)
public class Route implements MetaData {
	private static final long serialVersionUID = 2672586575464268486L;

	private String outcomeUrl;

	@XmlElement(namespace = XMLUtil.ROUTER_NAMESPACE)
	private List<RouteCriteria> criteria = new ArrayList<>();
	
	public String getOutcomeUrl() {
		return outcomeUrl;
	}
	@XmlAttribute(name = "outcome", required = true)
	public void setOutcomeUrl(String outcomeUrl) {
		this.outcomeUrl = UtilImpl.processStringValue(outcomeUrl);
	}
	
	public List<RouteCriteria> getCriteria() {
		return criteria;
	}
}
