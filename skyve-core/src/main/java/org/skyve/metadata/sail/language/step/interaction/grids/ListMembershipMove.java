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
 * Moves a member of the ordered list membership bound to {@code binding} to the zero-based position {@code to} in its
 * member list, as a user drags it, for example
 * {@code <listMembershipMove binding="members" rowContaining="Third {run}" to="0"/>}.
 *
 * <p>The member is located by its zero-based {@code row} index in the member list or by text its displayed description
 * contains ({@code rowContaining}, case-sensitive); exactly one is required (see {@link GridRowStep}). {@code to} is the
 * member's position once it has moved, counted in the members as they are shown, so {@code to="0"} makes it the first
 * member. Executors fail the step on a membership that is not ordered (its collection has no ordinal, so its members
 * cannot be dragged), and when {@code to} is past the last member.
 *
 * <p>Executors pass {@code rowContaining} through {@code ContextualExecutor.substituteTokens(String)}, so that
 * {@code {run}} and {@code {var:name}} resolve.
 *
 * @see DataGridMove
 * @see org.skyve.metadata.sail.execution.Executor#executeListMembershipMove
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ListMembershipMove implements GridRowStep {

	private String binding;
	private Integer row;
	private String rowContaining;
	private Integer to;

	/**
	 * Returns the binding of the list membership.
	 * @return the binding
	 */
	public @Nonnull String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding of the list membership.
	 * @param binding the binding
	 */
	@XmlAttribute(name = "binding", required = true)
	public void setBinding(@Nonnull String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	/**
	 * Returns the zero-based index, in the member list, of the member to move.
	 * @return the index, or {@code null} when the member is located by {@link #getRowContaining()}
	 */
	@Override
	public @Nullable Integer getRow() {
		return row;
	}

	/**
	 * Sets the zero-based index, in the member list, of the member to move.
	 * Exactly one of {@code row} and {@code rowContaining} must be set.
	 * @param row the index, or {@code null} to locate the member by {@code rowContaining}
	 */
	@XmlAttribute(name = "row")
	public void setRow(@Nullable Integer row) {
		this.row = row;
	}

	/**
	 * Returns the text that the displayed description of the member to move contains.
	 * @return the text, or {@code null} when the member is located by {@link #getRow()}
	 */
	@Override
	public @Nullable String getRowContaining() {
		return rowContaining;
	}

	/**
	 * Sets the text that the displayed description of the member to move contains (case-sensitive); the first such
	 * member, in the order shown, is moved.
	 * Exactly one of {@code row} and {@code rowContaining} must be set.
	 * {@code {run}} and {@code {var:name}} in the text are replaced by the run token and the captured variables.
	 * @param rowContaining the text, or {@code null} to locate the member by {@code row}
	 */
	@XmlAttribute(name = "rowContaining")
	public void setRowContaining(@Nullable String rowContaining) {
		this.rowContaining = UtilImpl.processStringValue(rowContaining);
	}

	/**
	 * Returns the zero-based position the member is moved to.
	 * @return the position
	 */
	public @Nonnull Integer getTo() {
		return to;
	}

	/**
	 * Sets the zero-based position the member is moved to, in the members as they are shown.
	 * @param to the position, 0 or more
	 */
	@XmlAttribute(name = "to", required = true)
	public void setTo(@Nonnull Integer to) {
		this.to = to;
	}

	/**
	 * Checks that the member is located in exactly one way and that {@code to} is set and not negative, then dispatches
	 * to {@link Executor#executeListMembershipMove(ListMembershipMove)}.
	 * @param executor the executor
	 * @throws MetaDataException if neither or both of {@code row} and {@code rowContaining} are set, or {@code to} is
	 * 			missing or negative
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		GridRowStep.validateRow(this);
		if ((to == null) || (to.intValue() < 0)) {
			throw new MetaDataException("<ListMembershipMove /> requires [to], a position of 0 or more");
		}
		executor.executeListMembershipMove(this);
	}

	/**
	 * Returns the binding of the list membership.
	 * @param context the context
	 * @return the binding
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return binding;
	}
}
