package org.skyve.metadata.sail.language.step;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Asserts how many rows of a list grid or data grid are shown, optionally counting only
 * the rows that have a cell containing {@code rowContaining}.
 *
 * <p>The grid is either the data grid bound to {@code binding} in the current edit view, or
 * a list grid identified like {@link org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect}
 * by {@code module}, {@code document}, {@code query} and {@code model} (each defaulting from the
 * current context). {@code binding} cannot be combined with the list grid attributes.
 *
 * <p>The step passes when {@code minRows <= count <= maxRows}. An absent {@code minRows} means 0
 * and an absent {@code maxRows} means unbounded, except that when both are absent the step asserts
 * at least one row. For example, {@code <testGridRows rowContaining="jane{run}" maxRows="0"/>}
 * asserts that no row mentions the user.
 *
 * <p>When {@code timeoutMillis} is set, the executor refreshes the grid and counts again until the
 * assertion holds or the timeout elapses, which waits for asynchronous jobs.
 * {@code {run}} in {@code rowContaining} is replaced by the run token.
 *
 * @see org.skyve.metadata.sail.language.step.interaction.grids.GridRowStep
 * @see org.skyve.metadata.sail.execution.Executor#executeTestGridRows
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestGridRows implements Step {

	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	private String binding;
	private String rowContaining;
	private Integer minRows;
	private Integer maxRows;
	private Integer timeoutMillis;

	/**
	 * Returns the module of the list grid.
	 * @return the module name, or {@code null} to use the current context's module
	 */
	public @Nullable String getModuleName() {
		return moduleName;
	}

	/**
	 * Sets the module of the list grid.
	 * @param moduleName the module name, or {@code null} to use the current context's module
	 */
	@XmlAttribute(name = "module")
	public void setModuleName(@Nullable String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	/**
	 * Returns the document of the list grid.
	 * @return the document name, or {@code null} to use the current context's document
	 */
	public @Nullable String getDocumentName() {
		return documentName;
	}

	/**
	 * Sets the document of the list grid.
	 * @param documentName the document name, or {@code null} to use the current context's document
	 */
	@XmlAttribute(name = "document")
	public void setDocumentName(@Nullable String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	/**
	 * Returns the query of the list grid.
	 * @return the query name, or {@code null}
	 */
	public @Nullable String getQueryName() {
		return queryName;
	}

	/**
	 * Sets the query of the list grid.
	 * @param queryName the query name, or {@code null}
	 */
	@XmlAttribute(name = "query")
	public void setQueryName(@Nullable String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	/**
	 * Returns the model of the list grid.
	 * @return the model name, or {@code null}
	 */
	public @Nullable String getModelName() {
		return modelName;
	}

	/**
	 * Sets the model of the list grid.
	 * @param modelName the model name, or {@code null}
	 */
	@XmlAttribute(name = "model")
	public void setModelName(@Nullable String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	/**
	 * Returns the binding of the data grid.
	 * @return the binding, or {@code null} when the step tests a list grid
	 */
	public @Nullable String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding of the data grid.
	 * @param binding the binding, or {@code null} to test a list grid
	 */
	@XmlAttribute(name = "binding")
	public void setBinding(@Nullable String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	/**
	 * Returns the text that a counted row must have in one of its cells.
	 * @return the text, or {@code null} to count every row
	 */
	public @Nullable String getRowContaining() {
		return rowContaining;
	}

	/**
	 * Sets the text that a counted row must have in one of its cells.
	 * @param rowContaining the text, or {@code null} to count every row; {@code {run}} is replaced by the run token
	 */
	@XmlAttribute(name = "rowContaining")
	public void setRowContaining(@Nullable String rowContaining) {
		this.rowContaining = UtilImpl.processStringValue(rowContaining);
	}

	/**
	 * Returns the minimum number of counted rows.
	 * @return the minimum, or {@code null}; see {@link #getEffectiveMinRows()}
	 */
	public @Nullable Integer getMinRows() {
		return minRows;
	}

	/**
	 * Sets the minimum number of counted rows.
	 * @param minRows the minimum, or {@code null}
	 */
	@XmlAttribute(name = "minRows")
	public void setMinRows(@Nullable Integer minRows) {
		this.minRows = minRows;
	}

	/**
	 * Returns the maximum number of counted rows.
	 * @return the maximum, or {@code null} for no maximum
	 */
	public @Nullable Integer getMaxRows() {
		return maxRows;
	}

	/**
	 * Sets the maximum number of counted rows.
	 * @param maxRows the maximum, or {@code null} for no maximum
	 */
	@XmlAttribute(name = "maxRows")
	public void setMaxRows(@Nullable Integer maxRows) {
		this.maxRows = maxRows;
	}

	/**
	 * Returns how long to keep refreshing the grid until the assertion holds.
	 * @return the timeout in milliseconds, or {@code null} to count once
	 */
	public @Nullable Integer getTimeoutMillis() {
		return timeoutMillis;
	}

	/**
	 * Sets how long to keep refreshing the grid until the assertion holds.
	 * @param timeoutMillis the timeout in milliseconds, or {@code null} to count once
	 */
	@XmlAttribute(name = "timeoutMillis")
	public void setTimeoutMillis(@Nullable Integer timeoutMillis) {
		this.timeoutMillis = timeoutMillis;
	}

	/**
	 * Returns the minimum number of counted rows after applying the defaults.
	 * @return {@code minRows} if set, 1 if neither {@code minRows} nor {@code maxRows} is set, otherwise 0
	 */
	public int getEffectiveMinRows() {
		if (minRows != null) {
			return minRows.intValue();
		}
		return (maxRows == null) ? 1 : 0;
	}

	/**
	 * Checks the attribute combination, then dispatches to {@link Executor#executeTestGridRows(TestGridRows)}.
	 * @param executor the executor
	 * @throws MetaDataException if {@code binding} is combined with a list grid attribute,
	 * 			a row count or the timeout is negative, or {@code minRows} exceeds {@code maxRows}
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		if ((binding != null) && ((moduleName != null) || (documentName != null) || (queryName != null) || (modelName != null))) {
			throw new MetaDataException("<TestGridRows /> [binding] cannot be combined with [module], [document], [query] or [model]");
		}
		if (((minRows != null) && (minRows.intValue() < 0)) ||
				((maxRows != null) && (maxRows.intValue() < 0)) ||
				((timeoutMillis != null) && (timeoutMillis.intValue() < 0))) {
			throw new MetaDataException("<TestGridRows /> [minRows], [maxRows] and [timeoutMillis] cannot be negative");
		}
		if ((minRows != null) && (maxRows != null) && (minRows.intValue() > maxRows.intValue())) {
			throw new MetaDataException("<TestGridRows /> [minRows] cannot exceed [maxRows]");
		}
		executor.executeTestGridRows(this);
	}

	/**
	 * Returns the data grid binding, or the list grid identifier used by the list grid steps.
	 * @param context the context
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		if (binding != null) {
			return binding;
		}
		return NavigateList.listGridIdentifier(context, moduleName, queryName, documentName, modelName);
	}
}
