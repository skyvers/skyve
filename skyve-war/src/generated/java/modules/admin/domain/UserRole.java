package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import modules.admin.User.UserExtension;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * User Role
 * 
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class UserRole extends AbstractPersistentBean implements ChildBean<UserExtension> {
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

	/**
	 * Role Name
	 **/
	private String roleName;

	private UserExtension parent;

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

	public static UserRole newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{roleName}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof UserRole) && 
					this.getBizId().equals(((UserRole) o).getBizId()));
	}

	/**
	 * {@link #roleName} accessor.
	 * @return	The value.
	 **/
	public String getRoleName() {
		return roleName;
	}

	/**
	 * {@link #roleName} mutator.
	 * @param roleName	The new value.
	 **/
	@XmlElement
	public void setRoleName(String roleName) {
		preset(roleNamePropertyName, roleName);
		this.roleName = roleName;
	}

	@Override
	public UserExtension getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(UserExtension parent) {
		if (this.parent != parent) {
			preset(ChildBean.PARENT_NAME, parent);
			this.parent = parent;
		}
	}

	@Override
	public Integer getBizOrdinal() {
		return bizOrdinal;
	}

	@Override
	@XmlElement
	public void setBizOrdinal(Integer bizOrdinal) {
		preset(Bean.ORDINAL_NAME, bizOrdinal);
		this.bizOrdinal =  bizOrdinal;
	}
}
