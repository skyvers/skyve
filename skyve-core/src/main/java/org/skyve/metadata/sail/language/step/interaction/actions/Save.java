package org.skyve.metadata.sail.language.step.interaction.actions;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

/**
 * Save implicit action
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Save implements Step {
	private Boolean createView;

	public Boolean getCreateView() {
		return createView;
	}
	
	@XmlAttribute(name = "createView")
	public void setCreateView(Boolean createView) {
		this.createView = createView;
	}

	@Override
	public void execute(Executor executor) {
		executor.executeSave(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return ImplicitActionName.Save.toString();
	}
}
