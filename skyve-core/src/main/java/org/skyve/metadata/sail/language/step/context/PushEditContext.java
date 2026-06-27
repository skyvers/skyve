package org.skyve.metadata.sail.language.step.context;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;
import org.skyve.web.UserAgentType;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Pushes a new edit-view context onto the automation execution stack for the specified
 * module and document, making that context active for all subsequent steps until a
 * matching {@link PopContext} or {@link ClearContext} is encountered.
 *
 * <p>Optional attributes allow the step to target a specific UX/UI rendering mode
 * ({@code uxui}) and user-agent type ({@code userAgentType}), and to indicate
 * whether the pushed context represents a "create" view ({@code createView}).
 *
 * @see PopContext
 * @see ClearContext
 * @see PushListContext
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class PushEditContext implements Step {

	private String moduleName;
	private String documentName;
	private Boolean createView;
	private String uxui;
	private UserAgentType userAgentType;
	
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
	 * Returns the uxui.
	 * @return the result
	 */
	public String getUxui() {
		return uxui;
	}
	
	/**
	 * Sets the uxui.
	 * @param uxui the uxui
	 */
	@XmlAttribute(name = "uxui")
	public void setUxui(String uxui) {
		this.uxui = UtilImpl.processStringValue(uxui);
	}

	/**
	 * Returns the userAgentType.
	 * @return the result
	 */
	public UserAgentType getUserAgentType() {
		return userAgentType;
	}
	
	/**
	 * Sets the userAgentType.
	 * @param userAgentType the userAgentType
	 */
	@XmlAttribute(name = "userAgentType")
	public void setUserAgentType(UserAgentType userAgentType) {
		this.userAgentType = userAgentType;
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executePushEditContext(this);
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
