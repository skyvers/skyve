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
 * Set the control representing the given binding to the given value.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class DataEnter implements Step {
	private String binding;
	private String value;
	private Boolean navigateToWidget;
	
	public String getBinding() {
		return binding;
	}

	@XmlAttribute(name = "binding", required = true)
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	public String getValue() {
		return value;
	}

	@XmlAttribute(name = "value", required = true)
	public void setValue(String value) {
		this.value = UtilImpl.processStringValue(value);
	}

	public Boolean getNavigateToWidget() {
		return navigateToWidget;
	}

	@XmlAttribute(name = "navigateToWidget")
	public void setNavigateToWidget(Boolean navigateToWidget) {
		this.navigateToWidget = navigateToWidget;
	}

	@Override
	public void execute(Executor executor) {
		executor.executeDataEnter(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return binding;
	}
}
