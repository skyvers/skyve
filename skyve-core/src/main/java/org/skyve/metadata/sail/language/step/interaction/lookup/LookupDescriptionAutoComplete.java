package org.skyve.metadata.sail.language.step.interaction.lookup;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Types the given {@code search} string into the lookup-description widget identified
 * by {@code binding} and selects the first matching suggestion from the auto-complete
 * dropdown.
 *
 * <p>Precondition: the widget must support auto-complete suggestions; if the search
 * produces no results the executor fails the step.
 *
 * <p>With {@code pick}, the step picks the first suggestion, in the order shown, whose text contains
 * {@code pick} (case-sensitive) instead of the first suggestion, and fails if no suggestion does.
 *
 * <p>{@code {run}} and {@code {var:name}} in {@code search} and {@code pick} are replaced by the run token
 * and the captured variables.
 *
 * @see LookupDescriptionPick
 * @see LookupDescriptionNew
 * @see org.skyve.metadata.sail.execution.Executor#executeLookupDescriptionAutoComplete
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class LookupDescriptionAutoComplete implements Step {

	private String binding;
	private String search;
	private String pick;
	
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
	 * Returns the search.
	 * @return the result
	 */
	public @Nonnull String getSearch() {
		return search;
	}

	/**
	 * Sets the search.
	 * @param search the search
	 */
	@XmlAttribute(name = "search", required = true)
	public void setSearch(@Nonnull String search) {
		this.search = UtilImpl.processStringValue(search);
	}

	/**
	 * Returns the text that the suggestion to pick contains.
	 * @return the text, or {@code null} to pick the first suggestion
	 */
	public @Nullable String getPick() {
		return pick;
	}

	/**
	 * Sets the text that the suggestion to pick contains.
	 * @param pick the text, or {@code null} to pick the first suggestion
	 */
	@XmlAttribute(name = "pick")
	public void setPick(@Nullable String pick) {
		this.pick = UtilImpl.processStringValue(pick);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeLookupDescriptionAutoComplete(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return binding + ".search";
	}
}
