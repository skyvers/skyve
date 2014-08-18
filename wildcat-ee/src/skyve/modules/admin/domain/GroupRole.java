package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.admin.domain.Group;
import org.skyve.domain.ChildBean;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * GroupRole
 * 
 * @stereotype "persistent child"
 */
@XmlType
public class GroupRole extends AbstractPersistentBean implements ChildBean<Group> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "GroupRole";

	/** @hidden */
	public static final String roleNamePropertyName = "roleName";

	private String roleName;
	private Group parent;

	private Integer bizOrdinal;


	@Override
	@XmlTransient
	public String getBizModule() {
		return GroupRole.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return GroupRole.DOCUMENT_NAME;
	}

	@Override
	@XmlTransient
	public String getBizKey() {
return (getRoleName()==null?"Group Role":getRoleName());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof GroupRole) && 
					this.getBizId().equals(((GroupRole) o).getBizId()));
	}

	/**
	 * {@link #roleName} accessor.
	 **/
	public String getRoleName() {
		return roleName;
	}

	/**
	 * {@link #roleName} mutator.
	 * 
	 * @param roleName	The new value to set.
	 **/
	@XmlElement
	public void setRoleName(String roleName) {
		preset(roleNamePropertyName, roleName);
		this.roleName = roleName;
	}

	@Override
	public Group getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(Group parent) {
		preset(ChildBean.PARENT_NAME, parent);
		this.parent =  parent;
	}

	@Override
	public Integer getBizOrdinal() {
		return bizOrdinal;
	}

	@Override
	@XmlElement
	public void setBizOrdinal(Integer bizOrdinal) {
		preset(ChildBean.ORDINAL_KEY, bizOrdinal);
		this.bizOrdinal =  bizOrdinal;
	}
}
