package org.skyve.metadata.sail.language.step.interaction.lookup;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Opens the edit view for the record currently selected in the lookup-description
 * widget identified by {@code binding}.
 *
 * <p>Precondition: the widget must already have a selected record; if the widget
 * is empty, the executor's behaviour is implementation-defined.
 *
 * @see LookupDescriptionPick
 * @see LookupDescriptionNew
 * @see org.skyve.metadata.sail.execution.Executor#executeLookupDescriptionEdit
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class LookupDescriptionEdit implements Step {

	private String binding;
	
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
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeLookupDescriptionEdit(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return binding + ".edit";
	}
}
