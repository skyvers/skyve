package org.skyve.impl.tools.test.sail.language.step.interaction.navigation;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.execution.Executor;
import org.skyve.impl.tools.test.sail.language.Step;
import org.skyve.impl.util.UtilImpl;

/**
 * Navigate to a link.
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class NavigateLink implements Step {
	private String href;
	
	public String getHref() {
		return href;
	}

	@XmlAttribute(name = "href", required = true)
	public void setHref(String href) {
		this.href = UtilImpl.processStringValue(href);
	}

	@Override
	public void execute(Executor executor) {
		executor.execute(this);
	}
}
