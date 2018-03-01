package org.skyve.impl.tools.test.sail.language.step.interaction.navigation;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.execution.Executor;

/**
 * Navigate to a tree view.
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class NavigateCalendar extends NavigateList {
	@Override
	public void execute(Executor executor) {
		executor.execute(this);
	}
}
