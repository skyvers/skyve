package org.skyve.impl.tools.test.sail.language.step.interaction;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.Step;

/**
 * Select the tab with the given tabPath.
 * The tabPath can either be the title of the tab, or if the tab pane is nested in another tab,
 * something like outerTabTitle/innerTabTitle.
 * 
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class TabSelect implements Step {
	private String tabPath;

	public String getTabPath() {
		return tabPath;
	}

	@XmlAttribute(name = "path", required = true)
	public void setTabPath(String tabPath) {
		this.tabPath = tabPath;
	}

	@Override
	public void execute(StringBuilder script, int indentationDepth) {
	}
}
