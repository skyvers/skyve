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
 * Enters the given {@code value} string into the UI control bound to {@code binding}
 * in the current view, simulating a user typing into the widget.
 *
 * <p>If {@code navigateToWidget} is {@code true}, the executor scrolls the control
 * into the viewport before entering the value, which may be necessary for widgets
 * that are only activated on focus.
 *
 * @see org.skyve.metadata.sail.language.step.TestValue
 * @see org.skyve.metadata.sail.execution.Executor#executeDataEnter
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class DataEnter implements Step {

	private String binding;
	private String value;
	private Boolean navigateToWidget;
	
	/**
	 * Returns the binding.
	 * @return the result
	 */
	public String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding.
	 * @param binding the binding
	 */
	@XmlAttribute(name = "binding", required = true)
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	/**
	 * Returns the value.
	 * @return the result
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Sets the value.
	 * @param value the value
	 */
	@XmlAttribute(name = "value", required = true)
	public void setValue(String value) {
		this.value = UtilImpl.processStringValue(value);
	}

	/**
	 * Returns the navigateToWidget.
	 * @return the result
	 */
	public Boolean getNavigateToWidget() {
		return navigateToWidget;
	}

	/**
	 * Sets the navigateToWidget.
	 * @param navigateToWidget the navigateToWidget
	 */
	@XmlAttribute(name = "navigateToWidget")
	public void setNavigateToWidget(Boolean navigateToWidget) {
		this.navigateToWidget = navigateToWidget;
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeDataEnter(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return binding;
	}
}
