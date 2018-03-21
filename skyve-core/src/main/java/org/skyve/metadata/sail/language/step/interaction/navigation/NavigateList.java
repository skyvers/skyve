package org.skyve.metadata.sail.language.step.interaction.navigation;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

/**
 * Navigate to a list view.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateList implements Step {
	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	
	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(name = "module", required = true)
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

	@Override
	public void execute(Executor executor) {
		executor.executeNavigateList(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return listGridIdentifier(context, moduleName, queryName, documentName, modelName);
	}
	
	public static String listGridIdentifier(AutomationContext context,
												String moduleName,
												String queryName,
												String documentName,
												String modelName) {
		String result = null;
		
		String keyModuleName = (moduleName == null) ? context.getModuleName() : moduleName;
		String keyDocumentName = (documentName == null) ? context.getDocumentName() : documentName;
		if (queryName != null) {
			result = String.format("%s.%s", keyModuleName, queryName);
		}
		else if (modelName != null) {
			result = String.format("%s.%s.%s", keyModuleName, keyDocumentName, modelName);
		}
		else {
			result = String.format("%s.%s", keyModuleName, keyDocumentName);
		}
		
		return result;
	}
}
