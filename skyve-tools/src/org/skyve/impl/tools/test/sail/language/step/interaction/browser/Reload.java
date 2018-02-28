package org.skyve.impl.tools.test.sail.language.step.interaction.browser;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.Step;

@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class Reload implements Step {
	@Override
	public void execute(StringBuilder script) {
		script.append("reload\n");
	}
}
