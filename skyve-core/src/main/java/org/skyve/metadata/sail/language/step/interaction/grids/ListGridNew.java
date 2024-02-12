package org.skyve.metadata.sail.language.step.interaction.grids;

import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Create a new row in a list grid
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ListGridNew implements Step {
	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	private Boolean createView;
	
	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(name = "module")
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(name = "document")
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public String getQueryName() {
		return queryName;
	}

	@XmlAttribute(name = "query")
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	public String getModelName() {
		return modelName;
	}

	@XmlAttribute(name = "model")
	public void setModelName(String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	public Boolean getCreateView() {
		return createView;
	}
	
	@XmlAttribute(name = "createView")
	public void setCreateView(Boolean createView) {
		this.createView = createView;
	}

	@Override
	public void execute(Executor executor) {
		executor.executeListGridNew(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		String identifier = NavigateList.listGridIdentifier(context, moduleName, queryName, documentName, modelName);
		return identifier + ".new";
	}
}
