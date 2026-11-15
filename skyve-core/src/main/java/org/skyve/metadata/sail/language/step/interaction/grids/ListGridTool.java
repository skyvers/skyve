package org.skyve.metadata.sail.language.step.interaction.grids;

import java.util.Arrays;

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
 * Presses a tool of the list grid for the given module, document, query or model that is not a view action, as a
 * user does with the grid's toolbar, for example {@code <listGridTool query="qContacts" tool="refresh"/>}.
 *
 * <p>{@code tool} is one of the {@link Tool} values; the XSD rejects any other. A renderer whose list grid has no
 * control for the tool fails the step, saying so.
 *
 * <p>The context is unchanged.
 *
 * @see ListGridClearFilter
 * @see org.skyve.metadata.sail.execution.Executor#executeListGridTool
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ListGridTool implements Step {

	/**
	 * A list grid toolbar feature that {@link ListGridTool} presses.
	 */
	@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
	@SuppressWarnings("java:S115") // Enum names are SAIL XML values.
	public enum Tool {
		/** Fetches the grid's rows again, with its filter and sort, as the grid's Refresh tool does. */
		refresh,
		/** Deselects the selected rows, as the grid's Deselect tool does. */
		clearSelection
	}

	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	private Tool tool;

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
	 * Returns the tool to press.
	 * @return the tool, or {@code null} on a step built without one (it then fails to execute)
	 */
	public @Nullable Tool getTool() {
		return tool;
	}

	/**
	 * Sets the tool to press.
	 * @param tool the tool
	 */
	@XmlAttribute(name = "tool", required = true)
	public void setTool(@Nullable Tool tool) {
		this.tool = tool;
	}

	/**
	 * Checks that the step names a tool, then dispatches to {@link Executor#executeListGridTool(ListGridTool)}.
	 * @param executor the executor
	 * @throws MetaDataException if the step has no tool (a step built in code: the XSD requires one)
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		if (tool == null) {
			throw new MetaDataException("<listGridTool /> requires [tool], one of " + Arrays.toString(Tool.values()));
		}
		executor.executeListGridTool(this);
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
