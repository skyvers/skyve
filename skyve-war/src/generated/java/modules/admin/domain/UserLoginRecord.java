package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import modules.admin.UserLoginRecord.UserLoginRecordExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;

/**
 * User Login Record
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public abstract class UserLoginRecord extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UserLoginRecord";

	/** @hidden */
	public static final String userNamePropertyName = "userName";

	/** @hidden */
	public static final String loginDateTimePropertyName = "loginDateTime";

	/** @hidden */
	public static final String failedPropertyName = "failed";

	/**
	 * User Name
	 **/
	private String userName;

	/**
	 * Sign In Date/Time
	 **/
	private DateTime loginDateTime;

	/**
	 * Failed
	 **/
	private Boolean failed;

	@Override
	@XmlTransient
	public String getBizModule() {
		return UserLoginRecord.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UserLoginRecord.DOCUMENT_NAME;
	}

	public static UserLoginRecordExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{userName} @ {loginDateTime}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof UserLoginRecord) && 
					this.getBizId().equals(((UserLoginRecord) o).getBizId()));
	}

	/**
	 * {@link #userName} accessor.
	 * @return	The value.
	 **/
	public String getUserName() {
		return userName;
	}

	/**
	 * {@link #userName} mutator.
	 * @param userName	The new value.
	 **/
	@XmlElement
	public void setUserName(String userName) {
		preset(userNamePropertyName, userName);
		this.userName = userName;
	}

	/**
	 * {@link #loginDateTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getLoginDateTime() {
		return loginDateTime;
	}

	/**
	 * {@link #loginDateTime} mutator.
	 * @param loginDateTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setLoginDateTime(DateTime loginDateTime) {
		preset(loginDateTimePropertyName, loginDateTime);
		this.loginDateTime = loginDateTime;
	}

	/**
	 * {@link #failed} accessor.
	 * @return	The value.
	 **/
	public Boolean getFailed() {
		return failed;
	}

	/**
	 * {@link #failed} mutator.
	 * @param failed	The new value.
	 **/
	@XmlElement
	public void setFailed(Boolean failed) {
		preset(failedPropertyName, failed);
		this.failed = failed;
	}
}
