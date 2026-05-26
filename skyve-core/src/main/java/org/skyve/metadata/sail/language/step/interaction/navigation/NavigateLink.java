package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Follows the hyperlink specified by {@code href}, navigating the browser to the
 * target URL or Skyve view without relying on a module/document binding.
 *
 * @see NavigateList
 * @see NavigateEdit
 * @see org.skyve.metadata.sail.execution.Executor#executeNavigateLink
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateLink implements Step {

	private String href;
	
	/**
	 * Returns the href.
	 * @return the result
	 */
	public String getHref() {
		return href;
	}

	/**
	 * Sets the href.
	 * @param href the href
	 */
	@XmlAttribute(name = "href", required = true)
	public void setHref(String href) {
		this.href = UtilImpl.processStringValue(href);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeNavigateLink(this);
	}

	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return href;
	}
}
