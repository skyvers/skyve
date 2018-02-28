package org.skyve.impl.tools.test.sail.language.step.interaction.actions;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.Step;

/**
 * Cancel implicit action
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class Cancel implements Step {
	@Override
	public void execute(StringBuilder script) {
	}
}
