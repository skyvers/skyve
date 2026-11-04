package org.skyve.metadata.sail.language.step.interaction.grids;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Selects the row at the specified zero-based {@code row} index in the datagrid
 * identified by {@code binding} without opening an inline editor or zooming.
 *
 * <p>Use this step when a subsequent action requires a row to be highlighted/selected
 * first, but no data entry on that row is needed.
 *
 * <p>The row can instead be located by the text of one of its cells with {@code rowContaining};
 * exactly one of {@code row} and {@code rowContaining} is required (see {@link GridRowStep}).
 *
 * @see DataGridEdit
 * @see DataGridZoom
 * @see org.skyve.metadata.sail.execution.Executor#executeDataGridSelect
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class DataGridSelect implements GridRowStep {

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
	 * Returns the zero-based index of the target row.
	 * @return the row index, or {@code null} when the row is located by {@link #getRowContaining()}
	 */
	@Override
	public @Nullable Integer getRow() {
		return row;
	}

	/**
	 * Sets the zero-based index of the target row.
	 * Exactly one of {@code row} and {@code rowContaining} must be set.
	 * @param row the row index, or {@code null} to locate the row by {@code rowContaining}
	 */
	@XmlAttribute(name = "row")
	public void setRow(@Nullable Integer row) {
		this.row = row;
	}

	/**
	 * Returns the text that a cell of the target row contains.
	 * @return the text, or {@code null} when the row is located by {@link #getRow()}
	 */
	@Override
	public @Nullable String getRowContaining() {
		return rowContaining;
	}

	/**
	 * Sets the text that a cell of the target row contains; the first such row is the target.
	 * Exactly one of {@code row} and {@code rowContaining} must be set.
	 * {@code {run}} in the text is replaced by the run token.
	 * @param rowContaining the text, or {@code null} to locate the row by {@code row}
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
		executor.executeDataGridSelect(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return binding + ".select";
	}
}
