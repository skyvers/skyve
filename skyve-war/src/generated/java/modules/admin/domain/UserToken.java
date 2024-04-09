package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;

/**
 * User Token
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class UserToken extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UserToken";

	/** @hidden */
	public static final String userNamePropertyName = "userName";

	/** @hidden */
	public static final String seriesPropertyName = "series";

	/** @hidden */
	public static final String tokenPropertyName = "token";

	/** @hidden */
	public static final String lastUsedPropertyName = "lastUsed";

	/**
	 * User Name
	 * <br/>
	 * Length is derived User.userName length + bizCustomer length + 1
	 **/
	private String userName;

	/**
	 * Series
	 **/
	private String series;

	/**
	 * Token
	 **/
	private String token;

	/**
	 * Last Used
	 **/
	private Timestamp lastUsed;

	@Override
	@XmlTransient
	public String getBizModule() {
		return UserToken.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UserToken.DOCUMENT_NAME;
	}

	public static UserToken newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{userName}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof UserToken) && 
					this.getBizId().equals(((UserToken) o).getBizId()));
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
	 * {@link #series} accessor.
	 * @return	The value.
	 **/
	public String getSeries() {
		return series;
	}

	/**
	 * {@link #series} mutator.
	 * @param series	The new value.
	 **/
	@XmlElement
	public void setSeries(String series) {
		preset(seriesPropertyName, series);
		this.series = series;
	}

	/**
	 * {@link #token} accessor.
	 * @return	The value.
	 **/
	public String getToken() {
		return token;
	}

	/**
	 * {@link #token} mutator.
	 * @param token	The new value.
	 **/
	@XmlElement
	public void setToken(String token) {
		preset(tokenPropertyName, token);
		this.token = token;
	}

	/**
	 * {@link #lastUsed} accessor.
	 * @return	The value.
	 **/
	public Timestamp getLastUsed() {
		return lastUsed;
	}

	/**
	 * {@link #lastUsed} mutator.
	 * @param lastUsed	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setLastUsed(Timestamp lastUsed) {
		preset(lastUsedPropertyName, lastUsed);
		this.lastUsed = lastUsed;
	}
}
