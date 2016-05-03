package org.skyve.impl.tools.test.wail.language.step.interaction;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.wail.XMLUtil;
import org.skyve.impl.tools.test.wail.language.Step;

@XmlType(namespace = XMLUtil.WAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.WAIL_NAMESPACE)
public class Menu implements Step {
	@Override
	public void execute(StringBuilder script) {
		script.append("menu\n");
	}
}
