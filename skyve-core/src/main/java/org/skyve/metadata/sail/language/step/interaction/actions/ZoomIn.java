package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.ExecutionOptions;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents the ZoomIn implicit action.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ZoomIn extends AbstractAction {

	private String binding;
	private Boolean confirm; // Need to press OK/Yes/Continue on the confirmation dialog
	
	public String getBinding() {
		return binding;
	}

	@XmlAttribute(name = "binding", required = true)
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	public Boolean getConfirm() {
		return confirm;
	}

	@XmlAttribute(name = "confirm")
	public void setConfirm(Boolean confirm) {
		this.confirm = confirm;
	}

	@Override
	public void execute(Executor executor, ExecutionOptions options) {
		executor.executeZoomIn(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return binding;
	}
}
