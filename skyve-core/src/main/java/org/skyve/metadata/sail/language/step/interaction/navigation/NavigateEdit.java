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
 * Navigate to an edit view.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateEdit implements Step {
	private String moduleName;
	private String documentName;
	private String bizId;
	
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

	@XmlAttribute(name = "document", required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public String getBizId() {
		return bizId;
	}

	@XmlAttribute(name = "bizId")
	public void setBizId(String bizId) {
		this.bizId = UtilImpl.processStringValue(bizId);
	}

	@Override
	public void execute(Executor executor) {
		executor.executeNavigateEdit(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return String.format("%s.%s", moduleName, documentName);
	}
}
