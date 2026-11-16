package org.skyve.metadata.sail.language.step;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.step.interaction.grids.GridRowStep;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Reads the text that a grid cell shows and keeps it in the named variable, for example
 * {@code <captureCell query="qToDos" rowContaining="SAIL {run}" column="reference" variable="reference"/>}.
 * Later steps use it as {@code {var:reference}}.
 *
 * <p>This is for values that a list or a data grid shows but no widget of a view does, such as a reference number
 * shown only in a list. The cell is in the column whose binding is {@code column}, of the row that {@code row} (zero-based)
 * or {@code rowContaining} names, as the other row steps find it (see {@link GridRowStep}).
 *
 * <p>The grid is a data grid of the current view, named by its {@code binding}, or else a list grid named as the other
 * list grid steps name it, by {@code module}, {@code document}, {@code query} and {@code model}, each defaulting from the
 * context. {@code binding} cannot be combined with those attributes.
 *
 * <p>The value is kept as {@link CaptureValue} keeps it: the displayed text, trimmed, optionally picked out by
 * {@code pattern}; a blank value, or a pattern that does not match, fails the step.
 *
 * <p>Invariants, checked by {@link #execute(Executor)} because the XSD cannot express them: exactly one of {@code row} and
 * {@code rowContaining}; {@code binding} or the list attributes, not both; a valid variable name and pattern.
 *
 * @see CaptureValue
 * @see TestGridRows
 * @see org.skyve.metadata.sail.execution.Executor#executeCaptureCell
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class CaptureCell implements CaptureStep, GridRowStep {
	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	private String binding;
	private Integer row;
	private String rowContaining;
	private String column;
	private String variable;
	private String pattern;

	/**
	 * Returns the module of the list grid.
	 * @return the module, or {@code null} for the context's
	 */
	public @Nullable String getModuleName() {
		return moduleName;
	}

	/**
	 * Sets the module of the list grid.
	 * @param moduleName the module, or {@code null} for the context's
	 */
	@XmlAttribute(name = "module")
	public void setModuleName(@Nullable String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	/**
	 * Returns the document of the list grid.
	 * @return the document, or {@code null} for the context's
	 */
	public @Nullable String getDocumentName() {
		return documentName;
	}

	/**
	 * Sets the document of the list grid (its default query, or the owner of its model).
	 * @param documentName the document, or {@code null} for the context's
	 */
	@XmlAttribute(name = "document")
	public void setDocumentName(@Nullable String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	/**
	 * Returns the query of the list grid.
	 * @return the query, or {@code null}
	 */
	public @Nullable String getQueryName() {
		return queryName;
	}

	/**
	 * Sets the query of the list grid.
	 * @param queryName the query, or {@code null}
	 */
	@XmlAttribute(name = "query")
	public void setQueryName(@Nullable String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	/**
	 * Returns the model of the list grid.
	 * @return the model, or {@code null}
	 */
	public @Nullable String getModelName() {
		return modelName;
	}

	/**
	 * Sets the model of the list grid.
	 * @param modelName the model, or {@code null}
	 */
	@XmlAttribute(name = "model")
	public void setModelName(@Nullable String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	/**
	 * Returns the binding of the data grid.
	 * @return the binding, or {@code null} when the step reads a list grid
	 */
	public @Nullable String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding of the data grid in the current view.
	 * @param binding the binding, or {@code null} to read a list grid
	 */
	@XmlAttribute(name = "binding")
	public void setBinding(@Nullable String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	/**
	 * Returns the zero-based index of the row.
	 * @return the row index, or {@code null} when the row is located by {@link #getRowContaining()}
	 */
	@Override
	public @Nullable Integer getRow() {
		return row;
	}

	/**
	 * Sets the zero-based index of the row.
	 * Exactly one of {@code row} and {@code rowContaining} must be set.
	 * @param row the row index, or {@code null} to locate the row by {@code rowContaining}
	 */
	@XmlAttribute(name = "row")
	public void setRow(@Nullable Integer row) {
		this.row = row;
	}

	/**
	 * Returns the text that a cell of the row contains.
	 * @return the text, or {@code null} when the row is located by {@link #getRow()}
	 */
	@Override
	public @Nullable String getRowContaining() {
		return rowContaining;
	}

	/**
	 * Sets the text that a cell of the row contains; the first such row is read.
	 * Exactly one of {@code row} and {@code rowContaining} must be set.
	 * {@code {run}} and {@code {var:name}} in the text are replaced by the run token and the captured variables.
	 * @param rowContaining the text, or {@code null} to locate the row by {@code row}
	 */
	@XmlAttribute(name = "rowContaining")
	public void setRowContaining(@Nullable String rowContaining) {
		this.rowContaining = UtilImpl.processStringValue(rowContaining);
	}

	/**
	 * Returns the binding of the column to read.
	 * @return the column's binding
	 */
	public @Nonnull String getColumn() {
		return column;
	}

	/**
	 * Sets the binding of the column to read, as {@code listGridFilter} names a column.
	 * @param column the column's binding
	 */
	@XmlAttribute(name = "column", required = true)
	public void setColumn(@Nonnull String column) {
		this.column = UtilImpl.processStringValue(column);
	}

	/**
	 * Returns the name of the variable that receives the value.
	 * @return the variable name
	 */
	@Override
	public @Nonnull String getVariable() {
		return variable;
	}

	/**
	 * Sets the name of the variable that receives the value.
	 * @param variable the variable name, matching {@value CaptureStep#VARIABLE_NAME_PATTERN}
	 */
	@XmlAttribute(name = "variable", required = true)
	public void setVariable(@Nonnull String variable) {
		this.variable = UtilImpl.processStringValue(variable);
	}

	/**
	 * Returns the regular expression that picks the value out of the cell's text.
	 * @return the pattern, or {@code null} to keep the whole text
	 */
	@Override
	public @Nullable String getPattern() {
		return pattern;
	}

	/**
	 * Sets the regular expression that picks the value out of the cell's text: its first group, or the whole match when
	 * it has no group.
	 * @param pattern the Java regular expression, or {@code null} to keep the whole text
	 */
	@XmlAttribute(name = "pattern")
	public void setPattern(@Nullable String pattern) {
		this.pattern = UtilImpl.processStringValue(pattern);
	}

	/**
	 * Checks the step's invariants, then dispatches to {@link Executor#executeCaptureCell(CaptureCell)}.
	 * @param executor the executor
	 * @throws MetaDataException if neither or both of {@code row} and {@code rowContaining} are set, {@code binding} is
	 * 								combined with a list attribute, or the variable name or pattern is invalid
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		GridRowStep.validateRow(this);
		if ((binding != null) && ((moduleName != null) || (documentName != null) || (queryName != null) || (modelName != null))) {
			throw new MetaDataException("<CaptureCell /> [binding] cannot be combined with [module], [document], [query] or [model]");
		}
		if (column == null) {
			throw new MetaDataException("<CaptureCell /> requires a [column]");
		}
		CaptureStep.validateCapture(this);
		executor.executeCaptureCell(this);
	}

	/**
	 * Returns the grid's identifier: the data grid's binding, or the list grid's identifier, as {@link TestGridRows} names it.
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
