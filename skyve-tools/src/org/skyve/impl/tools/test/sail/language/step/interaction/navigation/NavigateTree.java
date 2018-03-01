package org.skyve.impl.tools.test.sail.language.step.interaction.navigation;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.execution.Executor;
import org.skyve.impl.util.UtilImpl;

/**
 * Navigate to a calendar view.
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class NavigateTree extends NavigateList {
	private String startBinding;
	private String endBinding;

	public String getStartBinding() {
		return startBinding;
	}

	@XmlAttribute(name = "startBinding")
	public void setStartBinding(String startBinding) {
		this.startBinding = UtilImpl.processStringValue(startBinding);
	}

	public String getEndBinding() {
		return endBinding;
	}

	@XmlAttribute(name = "endBinding")
	public void setEndBinding(String endBinding) {
		this.endBinding = UtilImpl.processStringValue(endBinding);
	}

	@Override
	public void execute(Executor executor) {
		executor.execute(this);
	}
}
