package org.skyve.impl.tools.test.sail.language.step;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.Step;
import org.skyve.impl.util.UtilImpl;

@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
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
