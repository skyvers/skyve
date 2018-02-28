package org.skyve.impl.tools.test.sail.language.step;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.Step;

@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class Selenese implements Step {
	private String selenese;

	public String getSelenese() {
		return selenese;
	}

	@XmlValue
	public void setSelenese(String selenese) {
		this.selenese = selenese;
	}

	@Override
	public void execute(StringBuilder script) {
	}
}
