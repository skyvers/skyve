package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;

/**
 * User Dashboard
 * 
 * @navhas n currentUser 0..1 User
 * @navhas n roles 0..n UserRole
 * @navhas n jobs 0..n Job
 * @navhas n groups 0..n Group
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public class UserDashboard extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "UserDashboard";

	/** @hidden */
	public static final String currentUserPropertyName = "currentUser";
	/** @hidden */
	public static final String groupMembershipListPropertyName = "groupMembershipList";
	/** @hidden */
	public static final String lastLoginPropertyName = "lastLogin";
	/** @hidden */
	public static final String groupsPropertyName = "groups";
	/** @hidden */
	public static final String rolesPropertyName = "roles";
	/** @hidden */
	public static final String jobsPropertyName = "jobs";

	/**
	 * Current User
	 **/
	private User currentUser = null;
	/**
	 * Group Membership
	 **/
	private String groupMembershipList;
	/**
	 * Last Login
	 **/
	private DateTime lastLogin;
	/**
	 * Groups
	 **/
	private List<Group> groups = new ArrayList<>();
	/**
	 * Roles
	 **/
	private List<UserRole> roles = new ArrayList<>();
	/**
	 * Jobs
	 **/
	private List<Job> jobs = new ArrayList<>();

	@Override
	@XmlTransient
	public String getBizModule() {
		return UserDashboard.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UserDashboard.DOCUMENT_NAME;
	}

	public static UserDashboard newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof UserDashboard) && 
					this.getBizId().equals(((UserDashboard) o).getBizId()));
	}

	/**
	 * {@link #currentUser} accessor.
	 * @return	The value.
	 **/
	public User getCurrentUser() {
		return currentUser;
	}

	/**
	 * {@link #currentUser} mutator.
	 * @param currentUser	The new value.
	 **/
	@XmlElement
	public void setCurrentUser(User currentUser) {
		preset(currentUserPropertyName, currentUser);
		this.currentUser = currentUser;
	}

	/**
	 * {@link #groupMembershipList} accessor.
	 * @return	The value.
	 **/
	public String getGroupMembershipList() {
		return groupMembershipList;
	}

	/**
	 * {@link #groupMembershipList} mutator.
	 * @param groupMembershipList	The new value.
	 **/
	@XmlElement
	public void setGroupMembershipList(String groupMembershipList) {
		preset(groupMembershipListPropertyName, groupMembershipList);
		this.groupMembershipList = groupMembershipList;
	}

	/**
	 * {@link #lastLogin} accessor.
	 * @return	The value.
	 **/
	public DateTime getLastLogin() {
		return lastLogin;
	}

	/**
	 * {@link #lastLogin} mutator.
	 * @param lastLogin	The new value.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	@XmlElement
	public void setLastLogin(DateTime lastLogin) {
		preset(lastLoginPropertyName, lastLogin);
		this.lastLogin = lastLogin;
	}

	/**
	 * {@link #groups} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<Group> getGroups() {
		return groups;
	}

	/**
	 * {@link #groups} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Group getGroupsElementById(String bizId) {
		return getElementById(groups, bizId);
	}

	/**
	 * {@link #groups} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setGroupsElementById(@SuppressWarnings("unused") String bizId, Group element) {
		 setElementById(groups, element);
	}

	/**
	 * {@link #roles} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<UserRole> getRoles() {
		return roles;
	}

	/**
	 * {@link #roles} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public UserRole getRolesElementById(String bizId) {
		return getElementById(roles, bizId);
	}

	/**
	 * {@link #roles} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRolesElementById(@SuppressWarnings("unused") String bizId, UserRole element) {
		 setElementById(roles, element);
	}

	/**
	 * {@link #jobs} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<Job> getJobs() {
		return jobs;
	}

	/**
	 * {@link #jobs} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Job getJobsElementById(String bizId) {
		return getElementById(jobs, bizId);
	}

	/**
	 * {@link #jobs} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setJobsElementById(@SuppressWarnings("unused") String bizId, Job element) {
		 setElementById(jobs, element);
	}
}
