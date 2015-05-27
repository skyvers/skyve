package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.wildcat.domain.AbstractTransientBean;

/**
 * CurrentUser
 * 
 * @navhas n currentUser 0..1 User
 * @navhas n groups 0..n Group
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public class CurrentUser extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "CurrentUser";

	/** @hidden */
	public static final String currentUserPropertyName = "currentUser";
	/** @hidden */
	public static final String groupsPropertyName = "groups";

	private User currentUser = null;
	private List<Group> groups = new ArrayList<>();

	@Override
	@XmlTransient
	public String getBizModule() {
		return CurrentUser.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return CurrentUser.DOCUMENT_NAME;
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof CurrentUser) && 
					this.getBizId().equals(((CurrentUser) o).getBizId()));
	}

	/**
	 * {@link #currentUser} accessor.
	 **/
	public User getCurrentUser() {
		return currentUser;
	}

	/**
	 * {@link #currentUser} mutator.
	 * 
	 * @param currentUser	The new value to set.
	 **/
	@XmlElement
	public void setCurrentUser(User currentUser) {
		preset(currentUserPropertyName, currentUser);
		this.currentUser = currentUser;
	}

	/**
	 * {@link #groups} accessor.
	 **/
	@XmlElement
	public List<Group> getGroups() {
		return groups;
	}

	/**
	 * {@link #groups} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public Group getGroupsElementById(String bizId) {
		return getElementById(groups, bizId);
	}

	/**
	 * {@link #groups} mutator.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 * @param groups	The new value to set.
	 **/
	public void setGroupsElementById(@SuppressWarnings("unused") String bizId, Group element) {
		 setElementById(groups, element);
	}
}
