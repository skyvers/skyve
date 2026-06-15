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
 * Navigates directly to the edit view for the document instance identified by
 * {@code module}, {@code document}, and optionally {@code bizId}.
 *
 * <p>If {@code bizId} is omitted the executor navigates to a new-record edit view.
 *
 * @see NavigateList
 * @see org.skyve.metadata.sail.execution.Executor#executeNavigateEdit
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateEdit implements Step {

	private String moduleName;
	private String documentName;
	private String bizId;
	
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
	@XmlAttribute(name = "document", required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	/**
	 * Returns the bizId.
	 * @return the result
	 */
	public String getBizId() {
		return bizId;
	}

	/**
	 * Sets the bizId.
	 * @param bizId the bizId
	 */
	@XmlAttribute(name = "bizId")
	public void setBizId(String bizId) {
		this.bizId = UtilImpl.processStringValue(bizId);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeNavigateEdit(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return String.format("%s.%s", moduleName, documentName);
	}
}
