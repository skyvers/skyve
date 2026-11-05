package org.skyve.metadata.sail.language.step.interaction.grids;

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
 * Filters one column of the list grid for the given module, document, query or model, as a user does
 * in the grid's column filters, for example
 * {@code <listGridFilter query="qContacts" column="name" value="Smith{run}" operator="contains"/>}.
 *
 * <p>{@code column} is the column's binding. Filters on different columns are ANDed, and a filter on a
 * column that is already filtered replaces that column's filter. Without a {@code value}, the step clears
 * the column's filter. {@code operator} defaults to {@link FilterOperator#contains}, and needs a
 * {@code value}. The XSD cannot express that rule, so {@link #execute(Executor)} checks it.
 *
 * <p>{@code {run}} and {@code {var:name}} in {@code value} are replaced by the run token and the captured
 * variables; see {@code ContextualExecutor.substituteTokens(String)}.
 *
 * @see ListGridClearFilter
 * @see ListGridSort
 * @see org.skyve.metadata.sail.execution.Executor#executeListGridFilter
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ListGridFilter implements Step {
	/**
	 * How a {@link ListGridFilter} compares a column with its value. These are the operators that both
	 * the SmartClient filter row and the PrimeFaces list grid column filters offer.
	 */
	@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
	@SuppressWarnings("java:S115") // Enum names are SAIL XML values.
	public enum FilterOperator {
		/** The column contains the value. */
		contains,
		/** The column starts with the value. */
		startsWith,
		/** The column equals the value. */
		equals,
		/** The column does not equal the value. */
		notEquals,
		/** The column is less than the value. */
		lessThan,
		/** The column is greater than the value. */
		greaterThan
	}

	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	private String column;
	private String value;
	private FilterOperator operator;

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
	 * Returns the binding of the column to filter.
	 * @return the column binding
	 */
	public @Nonnull String getColumn() {
		return column;
	}

	/**
	 * Sets the binding of the column to filter.
	 * @param column the column binding
	 */
	@XmlAttribute(name = "column", required = true)
	public void setColumn(@Nonnull String column) {
		this.column = UtilImpl.processStringValue(column);
	}

	/**
	 * Returns the value to filter the column by.
	 * @return the value as written in the script, or {@code null} to clear the column's filter
	 */
	public @Nullable String getValue() {
		return value;
	}

	/**
	 * Sets the value to filter the column by.
	 * @param value the value, or {@code null} to clear the column's filter; {@code {run}} and {@code {var:name}} are substituted
	 */
	@XmlAttribute(name = "value")
	public void setValue(@Nullable String value) {
		this.value = UtilImpl.processStringValue(value);
	}

	/**
	 * Returns the operator as written in the script.
	 * @return the operator, or {@code null} for the default; see {@link #getEffectiveOperator()}
	 */
	public @Nullable FilterOperator getOperator() {
		return operator;
	}

	/**
	 * Sets how the column is compared with the value.
	 * @param operator the operator, or {@code null} for {@link FilterOperator#contains}
	 */
	@XmlAttribute(name = "operator")
	public void setOperator(@Nullable FilterOperator operator) {
		this.operator = operator;
	}

	/**
	 * Returns the operator after applying the default.
	 * @return {@code operator} if set, otherwise {@link FilterOperator#contains}
	 */
	public @Nonnull FilterOperator getEffectiveOperator() {
		return (operator == null) ? FilterOperator.contains : operator;
	}

	/**
	 * Checks the attribute combination, then dispatches to {@link Executor#executeListGridFilter(ListGridFilter)}.
	 * @param executor the executor
	 * @throws MetaDataException if {@code operator} is set without a {@code value}
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		if ((operator != null) && (value == null)) {
			throw new MetaDataException("<ListGridFilter /> [operator] needs a [value]: without a value the step clears the column's filter");
		}
		executor.executeListGridFilter(this);
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
