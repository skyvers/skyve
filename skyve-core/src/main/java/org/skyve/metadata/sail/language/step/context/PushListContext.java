package org.skyve.metadata.sail.language.step.context;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.web.UserAgentType;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Pushes a new list-view context onto the automation execution stack for the specified
 * module and document, making the list view active for subsequent steps.
 *
 * <p>Extends {@link NavigateList} with optional {@code uxui} and
 * {@code userAgentType} attributes to control which rendered variant of the list
 * is targeted.
 *
 * @see PopContext
 * @see ClearContext
 * @see PushEditContext
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class PushListContext extends NavigateList {
	private String uxui;
	private UserAgentType userAgentType;

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
		executor.executePushListContext(this);
	}

	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return NavigateList.listGridIdentifier(context, getModuleName(), getQueryName(), getDocumentName(), getModelName());
	}
}
