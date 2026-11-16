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
 * Clears every column filter of the list grid for the given module, document, query or model,
 * for example {@code <listGridClearFilter query="qContacts"/>}.
 *
 * @see ListGridFilter
 * @see org.skyve.metadata.sail.execution.Executor#executeListGridClearFilter
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ListGridClearFilter implements Step {

	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;

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
	 * Dispatches to {@link Executor#executeListGridClearFilter(ListGridClearFilter)}.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeListGridClearFilter(this);
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
