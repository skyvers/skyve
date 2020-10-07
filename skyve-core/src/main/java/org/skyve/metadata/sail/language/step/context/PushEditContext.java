package org.skyve.metadata.sail.language.step.context;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;
import org.skyve.web.UserAgentType;

/**
 * Push a new edit automation context onto the stack.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class PushEditContext implements Step {
	private String moduleName;
	private String documentName;
	private Boolean createView;
	private String uxui;
	private UserAgentType userAgentType;
	
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

	public Boolean getCreateView() {
		return createView;
	}
	
	@XmlAttribute(name = "createView")
	public void setCreateView(Boolean createView) {
		this.createView = createView;
	}

	public String getUxui() {
		return uxui;
	}
	
	@XmlAttribute(name = "uxui")
	public void setUxui(String uxui) {
		this.uxui = UtilImpl.processStringValue(uxui);
	}

	public UserAgentType getUserAgentType() {
		return userAgentType;
	}
	
	@XmlAttribute(name = "userAgentType")
	public void setUserAgentType(UserAgentType userAgentType) {
		this.userAgentType = userAgentType;
	}

	@Override
	public void execute(Executor executor) {
		executor.executePushEditContext(this);
	}

	@Override
	public String getIdentifier(AutomationContext context) {
		return String.format("%s.%s", moduleName, documentName);
	}
}
