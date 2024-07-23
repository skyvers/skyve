package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import modules.admin.Group.GroupExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Users
 * 
 * @navhas n userInvitationGroups 0..n Group
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class UserList extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UserList";

	/** @hidden */
	public static final String userInvitationGroupsPropertyName = "userInvitationGroups";

	/** @hidden */
	public static final String userInvitiationEmailListPropertyName = "userInvitiationEmailList";

	/** @hidden */
	public static final String bulkCreateWithEmailPropertyName = "bulkCreateWithEmail";

	/** @hidden */
	public static final String defaultModuleNamePropertyName = "defaultModuleName";

	/**
	 * User Invitation Groups
	 * <br/>
	 * The collection of groups that invited users are assigned.
	 **/
	private List<GroupExtension> userInvitationGroups = new ChangeTrackingArrayList<>("userInvitationGroups", this);

	/**
	 * Invitation email addresses
	 * <br/>
	 * The list of emails for users to invite. 
<br/>
Users will be created with the email address as username with the assigned groups.
<br/>
Provide a list separated by either comma or semicolon.
	 **/
	private String userInvitiationEmailList;

	/**
	 * Bulk create with email
	 **/
	private Boolean bulkCreateWithEmail;

	/**
	 * Default Module Name
	 **/
	private String defaultModuleName;

	@Override
	@XmlTransient
	public String getBizModule() {
		return UserList.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UserList.DOCUMENT_NAME;
	}

	public static UserList newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage("User List", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof UserList) && 
					this.getBizId().equals(((UserList) o).getBizId()));
	}

	/**
	 * {@link #userInvitationGroups} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<GroupExtension> getUserInvitationGroups() {
		return userInvitationGroups;
	}

	/**
	 * {@link #userInvitationGroups} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public GroupExtension getUserInvitationGroupsElementById(String bizId) {
		return getElementById(userInvitationGroups, bizId);
	}

	/**
	 * {@link #userInvitationGroups} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setUserInvitationGroupsElementById(String bizId, GroupExtension element) {
		setElementById(userInvitationGroups, element);
	}

	/**
	 * {@link #userInvitationGroups} add.
	 * @param element	The element to add.
	 **/
	public boolean addUserInvitationGroupsElement(GroupExtension element) {
		return userInvitationGroups.add(element);
	}

	/**
	 * {@link #userInvitationGroups} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addUserInvitationGroupsElement(int index, GroupExtension element) {
		userInvitationGroups.add(index, element);
	}

	/**
	 * {@link #userInvitationGroups} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeUserInvitationGroupsElement(GroupExtension element) {
		return userInvitationGroups.remove(element);
	}

	/**
	 * {@link #userInvitationGroups} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public GroupExtension removeUserInvitationGroupsElement(int index) {
		return userInvitationGroups.remove(index);
	}

	/**
	 * {@link #userInvitiationEmailList} accessor.
	 * @return	The value.
	 **/
	public String getUserInvitiationEmailList() {
		return userInvitiationEmailList;
	}

	/**
	 * {@link #userInvitiationEmailList} mutator.
	 * @param userInvitiationEmailList	The new value.
	 **/
	@XmlElement
	public void setUserInvitiationEmailList(String userInvitiationEmailList) {
		preset(userInvitiationEmailListPropertyName, userInvitiationEmailList);
		this.userInvitiationEmailList = userInvitiationEmailList;
	}

	/**
	 * {@link #bulkCreateWithEmail} accessor.
	 * @return	The value.
	 **/
	public Boolean getBulkCreateWithEmail() {
		return bulkCreateWithEmail;
	}

	/**
	 * {@link #bulkCreateWithEmail} mutator.
	 * @param bulkCreateWithEmail	The new value.
	 **/
	@XmlElement
	public void setBulkCreateWithEmail(Boolean bulkCreateWithEmail) {
		preset(bulkCreateWithEmailPropertyName, bulkCreateWithEmail);
		this.bulkCreateWithEmail = bulkCreateWithEmail;
	}

	/**
	 * {@link #defaultModuleName} accessor.
	 * @return	The value.
	 **/
	public String getDefaultModuleName() {
		return defaultModuleName;
	}

	/**
	 * {@link #defaultModuleName} mutator.
	 * @param defaultModuleName	The new value.
	 **/
	@XmlElement
	public void setDefaultModuleName(String defaultModuleName) {
		preset(defaultModuleNamePropertyName, defaultModuleName);
		this.defaultModuleName = defaultModuleName;
	}

	/**
	 * emailConfigured
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isEmailConfigured() {
		return (modules.admin.Configuration.ConfigurationExtension.validSMTPHost());
	}

	/**
	 * {@link #isEmailConfigured} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotEmailConfigured() {
		return (! isEmailConfigured());
	}
}
