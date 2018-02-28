package org.skyve.impl.tools.test.sail.language.step.interaction.actions;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.Step;

/**
 * All explicit actions - BizImport, BizExport, Upload, Download, Report, Server etc
 * @author mike
 */
@XmlType(namespace = XMLUtil.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class Action implements Step {
	private String actionName;
	
	public String getActionName() {
		return actionName;
	}

	@XmlAttribute(name = "name", required = true)
	public void setActionName(String actionName) {
		this.actionName = actionName;
	}

	@Override
	public void execute(StringBuilder script, int indentationDepth) {
	}
}
