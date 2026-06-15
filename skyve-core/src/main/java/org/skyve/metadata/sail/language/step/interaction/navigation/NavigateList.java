package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Navigates to the list view for the specified module, document, query, or model.
 *
 * <p>At minimum {@code moduleName} and {@code documentName} must be provided.
 * {@code queryName} and {@code modelName} are optional overrides that select a
 * specific named query or list-model when the document has more than one.
 *
 * <p>This class is also the base for {@link NavigateCalendar}, {@link NavigateMap},
 * {@link NavigateTree}, {@link org.skyve.metadata.sail.language.step.context.PushListContext},
 * and {@link org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew}.
 *
 * @see NavigateEdit
 * @see org.skyve.metadata.sail.execution.Executor#executeNavigateList
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateList implements Step {

	private String moduleName;
	private String documentName;
	private String queryName;
	private String modelName;
	
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
	@XmlAttribute(name = "module", required = true)
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
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeNavigateList(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return listGridIdentifier(context, moduleName, queryName, documentName, modelName);
	}
	
	/**
	 * Executes listGridIdentifier.
	 * @param context the context
	 * @param moduleName the moduleName
	 * @param queryName the queryName
	 * @param documentName the documentName
	 * @param modelName the modelName
	 * @return the result
	 */
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
