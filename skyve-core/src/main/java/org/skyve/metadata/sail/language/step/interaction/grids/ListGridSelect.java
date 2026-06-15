package org.skyve.metadata.sail.language.step.interaction.grids;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Selects the row at the specified zero-based {@code row} index in the list grid
 * for the given module, document, query, or model, highlighting it without
 * opening an edit view.
 *
 * @see ListGridZoom
 * @see ListGridNew
 * @see org.skyve.metadata.sail.execution.Executor#executeListGridSelect
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ListGridSelect implements Step {

	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	private Integer row;
	
	/**
	 * Returns the moduleName.
	 * @return the result
	 */
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * Sets the moduleName.
	 * @param moduleName the moduleName
	 */
	@XmlAttribute(name = "module")
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	/**
	 * Returns the documentName.
	 * @return the result
	 */
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * Sets the documentName.
	 * @param documentName the documentName
	 */
	@XmlAttribute(name = "document")
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	/**
	 * Returns the queryName.
	 * @return the result
	 */
	public String getQueryName() {
		return queryName;
	}

	/**
	 * Sets the queryName.
	 * @param queryName the queryName
	 */
	@XmlAttribute(name = "query")
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	/**
	 * Returns the modelName.
	 * @return the result
	 */
	public String getModelName() {
		return modelName;
	}

	/**
	 * Sets the modelName.
	 * @param modelName the modelName
	 */
	@XmlAttribute(name = "model")
	public void setModelName(String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	/**
	 * Returns the row.
	 * @return the result
	 */
	public Integer getRow() {
		return row;
	}

	/**
	 * Sets the row.
	 * @param row the row
	 */
	@XmlAttribute(name = "row", required = true)
	public void setRow(Integer row) {
		this.row = row;
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeListGridSelect(this);
	}

	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		String identifier = NavigateList.listGridIdentifier(context, moduleName, queryName, documentName, modelName);
		return identifier + ".select";
	}
}
