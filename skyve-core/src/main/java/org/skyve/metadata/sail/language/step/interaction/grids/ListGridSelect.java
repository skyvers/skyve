package org.skyve.metadata.sail.language.step.interaction.grids;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Selects the row at the specified zero-based {@code row} index in the list grid
 * for the given module, document, query, or model, highlighting it without
 * opening an edit view.
 *
 * <p>The row can instead be located by the text of one of its cells with {@code rowContaining};
 * exactly one of {@code row} and {@code rowContaining} is required (see {@link GridRowStep}).
 *
 * @see ListGridZoom
 * @see ListGridNew
 * @see org.skyve.metadata.sail.execution.Executor#executeListGridSelect
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ListGridSelect implements GridRowStep {

	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	private Integer row;
	private String rowContaining;
	
	/**
	 * Returns the moduleName.
	 * @return the result
	 */
	public @Nullable String getModuleName() {
		return moduleName;
	}

	/**
	 * Sets the moduleName.
	 * @param moduleName the moduleName
	 */
	@XmlAttribute(name = "module")
	public void setModuleName(@Nullable String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	/**
	 * Returns the documentName.
	 * @return the result
	 */
	public @Nullable String getDocumentName() {
		return documentName;
	}

	/**
	 * Sets the documentName.
	 * @param documentName the documentName
	 */
	@XmlAttribute(name = "document")
	public void setDocumentName(@Nullable String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	/**
	 * Returns the queryName.
	 * @return the result
	 */
	public @Nullable String getQueryName() {
		return queryName;
	}

	/**
	 * Sets the queryName.
	 * @param queryName the queryName
	 */
	@XmlAttribute(name = "query")
	public void setQueryName(@Nullable String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	/**
	 * Returns the modelName.
	 * @return the result
	 */
	public @Nullable String getModelName() {
		return modelName;
	}

	/**
	 * Sets the modelName.
	 * @param modelName the modelName
	 */
	@XmlAttribute(name = "model")
	public void setModelName(@Nullable String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
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
		executor.executeListGridSelect(this);
	}

	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		String identifier = NavigateList.listGridIdentifier(context, moduleName, queryName, documentName, modelName);
		return identifier + ".select";
	}
}
