package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.admin.domain.User;
import org.skyve.domain.ChildBean;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * UserRole
 * 
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
public class UserRole extends AbstractPersistentBean implements ChildBean<User> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "UserRole";

	/** @hidden */
	public static final String roleNamePropertyName = "roleName";

	private String roleName;
	private User parent;

	private Integer bizOrdinal;


	@Override
	@XmlTransient
	public String getBizModule() {
		return UserRole.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UserRole.DOCUMENT_NAME;
	}

	@Override
	@XmlTransient
	public String getBizKey() {
return (getRoleName()==null?"User Role":getRoleName());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof UserRole) && 
					this.getBizId().equals(((UserRole) o).getBizId()));
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
	public User getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(User parent) {
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
