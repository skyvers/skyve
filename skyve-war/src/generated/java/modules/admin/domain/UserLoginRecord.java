package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
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
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
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

	/** @hidden */
	public static final String ipAddressPropertyName = "ipAddress";

	/** @hidden */
	public static final String countryCodePropertyName = "countryCode";

	/** @hidden */
	public static final String countryNamePropertyName = "countryName";

	/**
	 * User Name
	 * <br/>
	 * The user name of the user that attempted to log in.
	 **/
	private String userName;

	/**
	 * Sign In Date/Time
	 * <br/>
	 * The date/time that the user attempted to login.
	 **/
	private DateTime loginDateTime;

	/**
	 * Failed
	 * <br/>
	 * A boolean indicating whether the login attempt of the user failed.
	 **/
	private Boolean failed;

	/**
	 * IP Address
	 * <br/>
	 * The IP Address of the user that has logged in. This shall be compared with next login for 
				security purposes by checking if there was a change in the IP Address.
	 **/
	private String ipAddress;

	/**
	 * Country
	 * <br/>
	 * This is the country code that is derived from the IP Address that is recorded. If it changes 
				from the previous login record an alert is sent to the user. It is only used when the 
				GeoipService is in use i.e if the user has provided a geo-ip key/token.
	 **/
	private String countryCode;

	/**
	 * Country Name
	 * <br/>
	 * This is the country name (in the user's locale) derived from the country code.
				The getter is overridden in the extension class.
	 **/
	private String countryName;

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

	/**
	 * {@link #ipAddress} accessor.
	 * @return	The value.
	 **/
	public String getIpAddress() {
		return ipAddress;
	}

	/**
	 * {@link #ipAddress} mutator.
	 * @param ipAddress	The new value.
	 **/
	@XmlElement
	public void setIpAddress(String ipAddress) {
		preset(ipAddressPropertyName, ipAddress);
		this.ipAddress = ipAddress;
	}

	/**
	 * {@link #countryCode} accessor.
	 * @return	The value.
	 **/
	public String getCountryCode() {
		return countryCode;
	}

	/**
	 * {@link #countryCode} mutator.
	 * @param countryCode	The new value.
	 **/
	@XmlElement
	public void setCountryCode(String countryCode) {
		preset(countryCodePropertyName, countryCode);
		this.countryCode = countryCode;
	}

	/**
	 * {@link #countryName} accessor.
	 * @return	The value.
	 **/
	public String getCountryName() {
		return countryName;
	}

	/**
	 * {@link #countryName} mutator.
	 * @param countryName	The new value.
	 **/
	@XmlElement
	public void setCountryName(String countryName) {
		this.countryName = countryName;
	}
}
