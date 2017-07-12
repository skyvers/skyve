package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
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
public class UserLoginRecord extends AbstractPersistentBean {
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

	/**
	 * User Name
	 **/
	private String userName;
	/**
	 * Login Date/Time
	 **/
	private DateTime loginDateTime;

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

	public static UserLoginRecord newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{userName} @ {loginDateTime}",
														this);
		}
		catch (Exception e) {
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
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	@XmlElement
	public void setLoginDateTime(DateTime loginDateTime) {
		preset(loginDateTimePropertyName, loginDateTime);
		this.loginDateTime = loginDateTime;
	}
}
