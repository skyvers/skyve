package org.skyve.metadata.sail.language.step.interaction;

import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Select the tab with the given tabPath.
 * The tabPath can either be the title of the tab, or if the tab pane is nested in another tab,
 * something like outerTabTitle/innerTabTitle.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TabSelect implements Step {
	// Tab names separated by "::" to make a path
	private String tabPath;

	public String getTabPath() {
		return tabPath;
	}

	@XmlAttribute(name = "path", required = true)
	public void setTabPath(String tabPath) {
		this.tabPath = UtilImpl.processStringValue(tabPath);
	}

	@Override
	public void execute(Executor executor) {
		executor.executeTabSelect(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return tabPath + " Tab";
	}
}
