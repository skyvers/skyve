package org.skyve.wildcat.tools.test.wail.language.step.interaction;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.tools.test.wail.XMLUtil;
import org.skyve.wildcat.tools.test.wail.language.Step;

@XmlType(namespace = XMLUtil.WAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.WAIL_NAMESPACE)
public class Action implements Step {
	@Override
	public void execute(StringBuilder script) {
		script.append("action\n");
	}
}
