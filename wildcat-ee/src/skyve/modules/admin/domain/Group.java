package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * Group
 * 
 * @navcomposed 1 roles 1..n GroupRole
 * @stereotype "persistent"
 */
@XmlType
public class Group extends AbstractPersistentBean {
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

	private String name;
	private String description;
	private List<GroupRole> roles = new ArrayList<>();

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

	@Override
	@XmlTransient
	public String getBizKey() {
return (getName()==null?"Group":getName());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Group) && 
					this.getBizId().equals(((Group) o).getBizId()));
	}

	/**
	 * {@link #name} accessor.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * 
	 * @param name	The new value to set.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}

	/**
	 * {@link #description} accessor.
	 **/
	public String getDescription() {
		return description;
	}

	/**
	 * {@link #description} mutator.
	 * 
	 * @param description	The new value to set.
	 **/
	@XmlElement
	public void setDescription(String description) {
		preset(descriptionPropertyName, description);
		this.description = description;
	}

	/**
	 * {@link #roles} accessor.
	 **/
	@XmlElement
	public List<GroupRole> getRoles() {
		return roles;
	}

	/**
	 * {@link #roles} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public GroupRole getRolesElementById(String bizId) {
		return getElementById(roles, bizId);
	}

	/**
	 * {@link #roles} mutator.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 * @param roles	The new value to set.
	 **/
	public void setRolesElementById(String bizId, GroupRole element) {
		 setElementById(roles, element);
	}
}
