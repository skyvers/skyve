package org.skyve.metadata.sail.language.step.interaction.grids;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlTransient;

/**
 * A grid step that acts on one row, located either by its zero-based {@code row} index
 * or by the text of one of its cells ({@code rowContaining}).
 *
 * <p>Invariant: exactly one of {@link #getRow()} and {@link #getRowContaining()} is set
 * on a step that is about to be executed. The XSD cannot express this, so every
 * implementation calls {@link #validateRow(GridRowStep)} from its
 * {@link Step#execute(org.skyve.metadata.sail.execution.Executor)} before dispatching.
 *
 * <p>{@code rowContaining} selects the first row, in the grid's displayed order, that has
 * a cell whose text contains the given value (case-sensitive). Executors must pass it through
 * {@code ContextualExecutor.substituteRunToken(String)} before matching, so that
 * {@code {run}} resolves to the run token.
 *
 * @see DataGridZoom
 * @see DataGridEdit
 * @see DataGridRemove
 * @see DataGridSelect
 * @see ListGridZoom
 * @see ListGridSelect
 */
@XmlTransient
public interface GridRowStep extends Step {
	/**
	 * Returns the zero-based index of the target row.
	 *
	 * @return the row index, or {@code null} when the row is located by {@link #getRowContaining()}
	 */
	public @Nullable Integer getRow();

	/**
	 * Returns the text that a cell of the target row contains.
	 *
	 * @return the text to match, or {@code null} when the row is located by {@link #getRow()}
	 */
	public @Nullable String getRowContaining();

	/**
	 * Checks that {@code step} locates its row in exactly one way.
	 *
	 * @param step the grid step to check
	 * @throws MetaDataException if neither or both of {@code row} and {@code rowContaining} are set
	 */
	public static void validateRow(@Nonnull GridRowStep step) {
		boolean hasRow = (step.getRow() != null);
		boolean hasRowContaining = (step.getRowContaining() != null);
		if (hasRow == hasRowContaining) {
			throw new MetaDataException("<" + step.getClass().getSimpleName() + " /> requires exactly one of [row] or [rowContaining]");
		}
	}
}
