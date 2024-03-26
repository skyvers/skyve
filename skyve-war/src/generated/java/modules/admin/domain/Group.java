package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;
import modules.admin.Group.GroupExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Group
 * 
 * @navcomposed 1 roles 1..n GroupRole
 * @navcomposed 1 candidateRoles 0..n GroupRole
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class Group extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Group";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String descriptionPropertyName = "description";

	/** @hidden */
	public static final String rolesPropertyName = "roles";

	/** @hidden */
	public static final String candidateRolesPropertyName = "candidateRoles";

	/**
	 * Group Name
	 **/
	private String name;

	/**
	 * Description
	 **/
	private String description;

	/**
	 * Roles
	 **/
	private List<GroupRole> roles = new ChangeTrackingArrayList<>("roles", this);

	/**
	 * Candidate Roles
	 * <br/>
	 * Holds the possible roles that can be assigned to this group.
	 **/
	private List<GroupRole> candidateRoles = new ArrayList<>();

	@Override
	@XmlTransient
	public String getBizModule() {
		return Group.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Group.DOCUMENT_NAME;
	}

	public static GroupExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{name}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Group) && 
					this.getBizId().equals(((Group) o).getBizId()));
	}

	/**
	 * {@link #name} accessor.
	 * @return	The value.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * @param name	The new value.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}

	/**
	 * {@link #description} accessor.
	 * @return	The value.
	 **/
	public String getDescription() {
		return description;
	}

	/**
	 * {@link #description} mutator.
	 * @param description	The new value.
	 **/
	@XmlElement
	public void setDescription(String description) {
		preset(descriptionPropertyName, description);
		this.description = description;
	}

	/**
	 * {@link #roles} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<GroupRole> getRoles() {
		return roles;
	}

	/**
	 * {@link #roles} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public GroupRole getRolesElementById(String bizId) {
		return getElementById(roles, bizId);
	}

	/**
	 * {@link #roles} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRolesElementById(String bizId, GroupRole element) {
		setElementById(roles, element);
	}

	/**
	 * {@link #roles} add.
	 * @param element	The element to add.
	 **/
	public boolean addRolesElement(GroupRole element) {
		boolean result = roles.add(element);
		if (result) {
			element.setParent((GroupExtension) this);
		}
		return result;
	}

	/**
	 * {@link #roles} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addRolesElement(int index, GroupRole element) {
		roles.add(index, element);
		element.setParent((GroupExtension) this);
	}

	/**
	 * {@link #roles} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeRolesElement(GroupRole element) {
		boolean result = roles.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #roles} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public GroupRole removeRolesElement(int index) {
		GroupRole result = roles.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * {@link #candidateRoles} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<GroupRole> getCandidateRoles() {
		return candidateRoles;
	}

	/**
	 * {@link #candidateRoles} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public GroupRole getCandidateRolesElementById(String bizId) {
		return getElementById(candidateRoles, bizId);
	}

	/**
	 * {@link #candidateRoles} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setCandidateRolesElementById(String bizId, GroupRole element) {
		setElementById(candidateRoles, element);
	}

	/**
	 * {@link #candidateRoles} add.
	 * @param element	The element to add.
	 **/
	public boolean addCandidateRolesElement(GroupRole element) {
		boolean result = candidateRoles.add(element);
		if (result) {
			element.setParent((GroupExtension) this);
		}
		return result;
	}

	/**
	 * {@link #candidateRoles} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addCandidateRolesElement(int index, GroupRole element) {
		candidateRoles.add(index, element);
		element.setParent((GroupExtension) this);
	}

	/**
	 * {@link #candidateRoles} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeCandidateRolesElement(GroupRole element) {
		boolean result = candidateRoles.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #candidateRoles} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public GroupRole removeCandidateRolesElement(int index) {
		GroupRole result = candidateRoles.remove(index);
		result.setParent(null);
		return result;
	}
}
