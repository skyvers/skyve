package org.skyve.impl.tools.test.sail.language.step.interaction;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.Step;

/**
 * Click on a menu item within the current module.
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class Menu implements Step {
	@Override
	public void execute(StringBuilder script) {
		script.append("menu\n");
	}
}
