package org.skyve.metadata.sail.language.step.interaction.grids;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Sorts the list grid for the given module, document, query or model by one column, as a user does by
 * clicking the column's header, for example {@code <listGridSort query="qContacts" column="name" descending="true"/>}.
 *
 * <p>{@code column} is the column's binding. The grid is sorted by that column alone, ascending unless
 * {@code descending} is {@code true}. A sort matters to SAIL because {@code rowContaining} picks the first
 * matching row in the order the grid shows.
 *
 * @see ListGridFilter
 * @see org.skyve.metadata.sail.execution.Executor#executeListGridSort
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ListGridSort implements Step {

	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	private String column;
	private Boolean descending;

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
	 * Returns the binding of the column to sort by.
	 * @return the column binding
	 */
	public @Nonnull String getColumn() {
		return column;
	}

	/**
	 * Sets the binding of the column to sort by.
	 * @param column the column binding
	 */
	@XmlAttribute(name = "column", required = true)
	public void setColumn(@Nonnull String column) {
		this.column = UtilImpl.processStringValue(column);
	}

	/**
	 * Returns whether the sort is descending, as written in the script.
	 * @return {@code true} for descending, or {@code false} or {@code null} for ascending
	 */
	public @Nullable Boolean getDescending() {
		return descending;
	}

	/**
	 * Sets whether the sort is descending.
	 * @param descending {@code true} for descending, or {@code false} or {@code null} for ascending
	 */
	@XmlAttribute(name = "descending")
	public void setDescending(@Nullable Boolean descending) {
		this.descending = descending;
	}

	/**
	 * Dispatches to {@link Executor#executeListGridSort(ListGridSort)}.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeListGridSort(this);
	}

	/**
	 * Returns the list grid identifier used by the list grid steps.
	 * @param context the context
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return NavigateList.listGridIdentifier(context, moduleName, queryName, documentName, modelName);
	}
}
