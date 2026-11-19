package org.skyve.metadata.sail.language.step.interaction;

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
 * Enters the given {@code value} string into the UI control bound to {@code binding}
 * in the current view, simulating a user typing into the widget.
 *
 * <p>If {@code navigateToWidget} is {@code true}, the executor scrolls the control
 * into the viewport before entering the value, which may be necessary for widgets
 * that are only activated on focus.
 *
 * <p>For a {@code listMembership} or {@code checkMembership} widget, {@code value} is a
 * {@code |}-separated list of the items that must end up selected, for example
 * {@code value="Admin|Basic"}; every other item is deselected. Items are matched by their
 * displayed description. An empty selection cannot be expressed because a blank value is
 * treated as absent.
 *
 * <p>For a combo or radio, {@code match} says how {@code value} names the option: by its zero-based
 * position ({@link Match#index}, the default, so that older scripts are unchanged), by its {@link Match#code}
 * or by its displayed {@link Match#label}. For a combo, the position counts the empty first entry when there
 * is one. A code or label must name exactly one option. {@code match} other than {@code index} fails the step
 * on any other widget.
 *
 * <p>{@code {run}} and {@code {var:name}} in {@code value} are replaced by the run token and the captured
 * variables; see {@code ContextualExecutor.substituteTokens(String)}.
 *
 * @see org.skyve.metadata.sail.language.step.TestValue
 * @see org.skyve.metadata.sail.execution.Executor#executeDataEnter
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class DataEnter implements Step {
	/**
	 * How a {@link DataEnter} value names the option of a combo or radio.
	 */
	@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
	@SuppressWarnings("java:S115") // Enum names are SAIL XML values.
	public enum Match {
		/** The value is the option's zero-based position. */
		index,
		/** The value is the option's code. */
		code,
		/** The value is the option's displayed label. */
		label
	}

	private String binding;
	private String value;
	private Boolean navigateToWidget;
	private Match match;
	
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
	 * Returns the value.
	 * @return the result
	 */
	public @Nonnull String getValue() {
		return value;
	}

	/**
	 * Sets the value to enter. {@code {run}} and {@code {var:name}} are substituted, and for membership
	 * widgets the value is a {@code |}-separated list of the items to select.
	 * @param value the value
	 */
	@XmlAttribute(name = "value", required = true)
	public void setValue(@Nonnull String value) {
		this.value = UtilImpl.processStringValue(value);
	}

	/**
	 * Returns the navigateToWidget.
	 * @return the result
	 */
	public @Nullable Boolean getNavigateToWidget() {
		return navigateToWidget;
	}

	/**
	 * Sets the navigateToWidget.
	 * @param navigateToWidget the navigateToWidget
	 */
	@XmlAttribute(name = "navigateToWidget")
	public void setNavigateToWidget(@Nullable Boolean navigateToWidget) {
		this.navigateToWidget = navigateToWidget;
	}

	/**
	 * Returns how the value names a combo or radio option, as written in the script.
	 * @return the match, or {@code null} for the default; see {@link #getEffectiveMatch()}
	 */
	public @Nullable Match getMatch() {
		return match;
	}

	/**
	 * Sets how the value names a combo or radio option.
	 * @param match the match, or {@code null} for {@link Match#index}
	 */
	@XmlAttribute(name = "match")
	public void setMatch(@Nullable Match match) {
		this.match = match;
	}

	/**
	 * Returns how the value names a combo or radio option, after applying the default.
	 * @return {@code match} if set, otherwise {@link Match#index}
	 */
	public @Nonnull Match getEffectiveMatch() {
		return (match == null) ? Match.index : match;
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeDataEnter(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return binding;
	}
}
