package org.skyve.metadata.sail.language.step.interaction.grids;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Clicks a link shown in a row of the data grid bound to {@code binding}, for example
 * {@code <dataGridLink binding="portalUsers" rowContaining="SAIL {run}" text="Revoke Access"/>}.
 *
 * <p>The row is located by its zero-based {@code row} index or by the text of one of its cells ({@code rowContaining});
 * exactly one is required (see {@link GridRowStep}). The link is the first displayed link in that row whose visible text
 * contains {@code text} (case-sensitive), so the same link in every row is told apart by its row.
 *
 * <p>A link that runs an action (an action reference) is pressed as the action steps press a button:
 * {@code confirm} answers its confirmation and any server-side questions (positively when it is {@code true} or absent,
 * negatively when it is {@code false}, which fails if nothing asks), and unless {@code testSuccess} is {@code false} the
 * step then checks that no error is shown. The context is unchanged, as the view stays.
 * A link that goes somewhere else (an edit view, a list or a URL) is followed as {@code navigateLink} follows one: the
 * top context loses its document and view type, and a script pushes a context for the page it reaches.
 *
 * <p>Executors pass {@code text} and {@code rowContaining} through {@code ContextualExecutor.substituteTokens(String)},
 * so that {@code {run}} and {@code {var:name}} resolve.
 *
 * @see DataGridZoom
 * @see org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink
 * @see org.skyve.metadata.sail.execution.Executor#executeDataGridLink
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class DataGridLink implements GridRowStep {

	private String binding;
	private Integer row;
	private String rowContaining;
	private String text;
	private Boolean confirm; // null/true answers the confirmation positively, false negatively
	private Boolean testSuccess; // null/true checks for errors afterwards

	/**
	 * Returns the binding of the data grid.
	 * @return the binding
	 */
	public @Nonnull String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding of the data grid.
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
	 * {@code {run}} and {@code {var:name}} in the text are replaced by the run token and the captured variables.
	 * @param rowContaining the text, or {@code null} to locate the row by {@code row}
	 */
	@XmlAttribute(name = "rowContaining")
	public void setRowContaining(@Nullable String rowContaining) {
		this.rowContaining = UtilImpl.processStringValue(rowContaining);
	}

	/**
	 * Returns the text that the visible text of the link contains (case-sensitive).
	 * @return the text
	 */
	public @Nonnull String getText() {
		return text;
	}

	/**
	 * Sets the text that the visible text of the link contains (case-sensitive).
	 * {@code {run}} and {@code {var:name}} in the text are replaced by the run token and the captured variables.
	 * @param text the text
	 */
	@XmlAttribute(name = "text", required = true)
	public void setText(@Nonnull String text) {
		this.text = UtilImpl.processStringValue(text);
	}

	/**
	 * Returns how to answer the confirmation that the link's action can show.
	 * @return {@code true} or {@code null} to answer positively, {@code false} to answer negatively
	 */
	public @Nullable Boolean getConfirm() {
		return confirm;
	}

	/**
	 * Sets how to answer a confirmation dialog or server-side question shown after the link is clicked, as for
	 * {@link org.skyve.metadata.sail.language.step.interaction.actions.Action#setConfirm(Boolean)}.
	 * @param confirm {@code true} or {@code null} to answer positively, {@code false} to answer negatively
	 */
	@XmlAttribute(name = "confirm")
	public void setConfirm(@Nullable Boolean confirm) {
		this.confirm = confirm;
	}

	/**
	 * Returns whether to check that no error is shown after the link's action.
	 * @return {@code false} to skip the check; {@code true} or {@code null} to make it
	 */
	public @Nullable Boolean getTestSuccess() {
		return testSuccess;
	}

	/**
	 * Sets whether to check that no error is shown after the link's action. Set it to {@code false} when the next step is
	 * a {@code <testFailure/>}.
	 * @param testSuccess {@code false} to skip the check; {@code true} or {@code null} to make it
	 */
	@XmlAttribute(name = "testSuccess")
	public void setTestSuccess(@Nullable Boolean testSuccess) {
		this.testSuccess = testSuccess;
	}

	/**
	 * Checks that the row is located in exactly one way and that the step has a text, then dispatches to
	 * {@link Executor#executeDataGridLink(DataGridLink)}.
	 * @param executor the executor
	 * @throws MetaDataException if neither or both of {@code row} and {@code rowContaining} are set, or there is no text
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		GridRowStep.validateRow(this);
		if (text == null) {
			throw new MetaDataException("<DataGridLink /> requires a [text]");
		}
		executor.executeDataGridLink(this);
	}

	/**
	 * Returns the binding of the data grid.
	 * @param context the context
	 * @return the binding
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return binding;
	}
}
