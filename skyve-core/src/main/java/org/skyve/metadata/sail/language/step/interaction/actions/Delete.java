package org.skyve.metadata.sail.language.step.interaction.actions;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.execution.Executor;

/**
 * Delete implicit action
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Delete extends AbstractAction {
	@Override
	public void execute(Executor executor) {
		executor.executeDelete(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return ImplicitActionName.Delete.toString();
	}
}
