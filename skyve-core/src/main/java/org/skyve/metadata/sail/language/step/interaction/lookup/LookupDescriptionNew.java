package org.skyve.metadata.sail.language.step.interaction.lookup;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Invokes the "add new" action on the lookup-description widget identified by
 * {@code binding}, opening a new edit view so that a new related record can be
 * created and linked to the current bean.
 *
 * @see LookupDescriptionPick
 * @see LookupDescriptionEdit
 * @see org.skyve.metadata.sail.execution.Executor#executeLookupDescriptionNew
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class LookupDescriptionNew implements Step {

	private String binding;
	
	/**
	 * Returns the binding.
	 * @return the result
	 */
	public @Nonnull String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding.
	 * @param binding the binding
	 */
	@XmlAttribute(name = "binding", required = true)
	public void setBinding(@Nonnull String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeLookupDescriptionNew(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return binding + ".new";
	}
}
