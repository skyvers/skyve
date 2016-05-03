package org.skyve.impl.tools.test.wail.language.step;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.wail.XMLUtil;
import org.skyve.impl.tools.test.wail.language.Step;
import org.skyve.impl.util.UtilImpl;

@XmlType(namespace = XMLUtil.WAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.WAIL_NAMESPACE)
public class Call implements Step {
	private String identifier;

	public String getIdentifier() {
		return identifier;
	}

	@XmlAttribute(required = true)
	public void setIdentifier(String identifier) {
		this.identifier = UtilImpl.processStringValue(identifier);
	}

	@Override
	public void execute(StringBuilder script) {
		script.append("call ").append(identifier).append('\n');
	}
}
