package org.skyve.impl.tools.test.sail.language.step.interaction;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.Step;

/**
 * Generate a random instance from the test system and scatter the values into the view.
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class TestDataEnter implements Step {
	@Override
	public void execute(StringBuilder script) {
	}
}
