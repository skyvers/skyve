package org.skyve.metadata.sail.language.step.interaction;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Selects the tab identified by {@code path} in the current view's tab widget.
 *
 * <p>The path is a sequence of tab-pane titles joined by {@code "::"}, allowing
 * nested tabs to be targeted in a single step (e.g., {@code "Outer::Inner"}).
 *
 * @see org.skyve.metadata.sail.execution.Executor#executeTabSelect
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TabSelect implements Step {

	// Tab names separated by "::" to make a path
	private String tabPath;

	/**
	 * Returns the tabPath.
	 * @return the result
	 */
	public String getTabPath() {
		return tabPath;
	}

	/**
	 * Sets the tabPath.
	 * @param tabPath the tabPath
	 */
	@XmlAttribute(name = "path", required = true)
	public void setTabPath(String tabPath) {
		this.tabPath = UtilImpl.processStringValue(tabPath);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeTabSelect(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return tabPath + " Tab";
	}
}
