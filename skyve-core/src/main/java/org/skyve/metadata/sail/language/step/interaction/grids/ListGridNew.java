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
 * Clicks the New button on the list grid for the specified module, document, query,
 * or model, opening a new edit view for a freshly created instance of that document.
 *
 * <p>Set {@code createView} to {@code true} when the document's edit view has a
 * distinct "create" mode that the executor should target.
 *
 * @see ListGridZoom
 * @see ListGridSelect
 * @see org.skyve.metadata.sail.execution.Executor#executeListGridNew
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ListGridNew implements Step {

	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	private Boolean createView;
	
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
	 * Returns the createView.
	 * @return the result
	 */
	public Boolean getCreateView() {
		return createView;
	}
	
	/**
	 * Sets the createView.
	 * @param createView the createView
	 */
	@XmlAttribute(name = "createView")
	public void setCreateView(Boolean createView) {
		this.createView = createView;
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeListGridNew(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		String identifier = NavigateList.listGridIdentifier(context, moduleName, queryName, documentName, modelName);
		return identifier + ".new";
	}
}
