package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Cancel implicit action
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Cancel extends AbstractAction {
	@Override
	public void execute(Executor executor) {
		executor.executeCancel(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return ImplicitActionName.Cancel.toString();
	}
}
