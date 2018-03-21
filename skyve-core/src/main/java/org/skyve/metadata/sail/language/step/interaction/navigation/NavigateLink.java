package org.skyve.metadata.sail.language.step.interaction.navigation;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Navigate to a link.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
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
		executor.executeNavigateLink(this);
	}

	@Override
	public String getIdentifier(AutomationContext context) {
		return href;
	}
}
