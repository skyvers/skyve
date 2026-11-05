package org.skyve.metadata.sail.language.step.interaction.lookup;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.step.interaction.grids.GridRowStep;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Selects the candidate at the zero-based {@code row} index from the picker dialog
 * of the lookup-description widget identified by {@code binding}.
 *
 * <p>The candidate can instead be located by its text with {@code rowContaining}: the first candidate,
 * in the order the drop-down shows, whose text contains the value (case-sensitive). Exactly one of
 * {@code row} and {@code rowContaining} is required, as for the grid row steps (see {@link GridRowStep}).
 * {@code {run}} and {@code {var:name}} in {@code rowContaining} are substituted.
 *
 * <p>Precondition: the picker dialog must already be open (e.g., after a
 * {@link LookupDescriptionAutoComplete} step triggered results) or the executor
 * opens it automatically.
 *
 * @see LookupDescriptionAutoComplete
 * @see LookupDescriptionEdit
 * @see org.skyve.metadata.sail.execution.Executor#executeLookupDescriptionPick
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class LookupDescriptionPick implements GridRowStep {

	private String binding;
	private Integer row;
	private String rowContaining;
	
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
	 * Returns the zero-based index of the candidate to pick.
	 * @return the row index, or {@code null} when the candidate is located by {@link #getRowContaining()}
	 */
	@Override
	public @Nullable Integer getRow() {
		return row;
	}

	/**
	 * Sets the zero-based index of the candidate to pick.
	 * Exactly one of {@code row} and {@code rowContaining} must be set.
	 * @param row the row index, or {@code null} to locate the candidate by {@code rowContaining}
	 */
	@XmlAttribute(name = "row")
	public void setRow(@Nullable Integer row) {
		this.row = row;
	}

	/**
	 * Returns the text that the candidate to pick contains.
	 * @return the text, or {@code null} when the candidate is located by {@link #getRow()}
	 */
	@Override
	public @Nullable String getRowContaining() {
		return rowContaining;
	}

	/**
	 * Sets the text that the candidate to pick contains; the first such candidate is picked.
	 * Exactly one of {@code row} and {@code rowContaining} must be set.
	 * @param rowContaining the text, or {@code null} to locate the candidate by {@code row}
	 */
	@XmlAttribute(name = "rowContaining")
	public void setRowContaining(@Nullable String rowContaining) {
		this.rowContaining = UtilImpl.processStringValue(rowContaining);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		GridRowStep.validateRow(this);
		executor.executeLookupDescriptionPick(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return binding + ".pick";
	}
}
