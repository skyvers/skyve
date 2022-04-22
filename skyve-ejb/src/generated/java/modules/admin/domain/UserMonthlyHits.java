package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * User Monthly Hits
 * 
 * @depend - - - Device
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class UserMonthlyHits extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UserMonthlyHits";

	/** @hidden */
	public static final String userNamePropertyName = "userName";

	/** @hidden */
	public static final String yearPropertyName = "year";

	/** @hidden */
	public static final String monthPropertyName = "month";

	/** @hidden */
	public static final String userAgentHeaderPropertyName = "userAgentHeader";

	/** @hidden */
	public static final String devicePropertyName = "device";

	/** @hidden */
	public static final String numberOfHitsPropertyName = "numberOfHits";

	/**
	 * Device
	 **/
	@XmlEnum
	public static enum Device implements Enumeration {
		phone("P", "Phone"),
		tablet("T", "Tablet"),
		desktop("D", "Desktop"),
		other("O", "Other");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private Device(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static Device fromCode(String code) {
			Device result = null;

			for (Device value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Device fromLocalisedDescription(String description) {
			Device result = null;

			for (Device value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				Device[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (Device value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * User Name
	 **/
	private String userName;

	/**
	 * Year
	 **/
	private Integer year;

	/**
	 * Month
	 **/
	private Integer month;

	/**
	 * User-Agent Header
	 **/
	private String userAgentHeader;

	/**
	 * Device
	 **/
	private Device device;

	/**
	 * Number Of Hits
	 **/
	private Integer numberOfHits;

	@Override
	@XmlTransient
	public String getBizModule() {
		return UserMonthlyHits.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UserMonthlyHits.DOCUMENT_NAME;
	}

	public static UserMonthlyHits newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{userName} - {year}/{month} = {numberOfHits}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof UserMonthlyHits) && 
					this.getBizId().equals(((UserMonthlyHits) o).getBizId()));
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
	 * {@link #year} accessor.
	 * @return	The value.
	 **/
	public Integer getYear() {
		return year;
	}

	/**
	 * {@link #year} mutator.
	 * @param year	The new value.
	 **/
	@XmlElement
	public void setYear(Integer year) {
		preset(yearPropertyName, year);
		this.year = year;
	}

	/**
	 * {@link #month} accessor.
	 * @return	The value.
	 **/
	public Integer getMonth() {
		return month;
	}

	/**
	 * {@link #month} mutator.
	 * @param month	The new value.
	 **/
	@XmlElement
	public void setMonth(Integer month) {
		preset(monthPropertyName, month);
		this.month = month;
	}

	/**
	 * {@link #userAgentHeader} accessor.
	 * @return	The value.
	 **/
	public String getUserAgentHeader() {
		return userAgentHeader;
	}

	/**
	 * {@link #userAgentHeader} mutator.
	 * @param userAgentHeader	The new value.
	 **/
	@XmlElement
	public void setUserAgentHeader(String userAgentHeader) {
		preset(userAgentHeaderPropertyName, userAgentHeader);
		this.userAgentHeader = userAgentHeader;
	}

	/**
	 * {@link #device} accessor.
	 * @return	The value.
	 **/
	public Device getDevice() {
		return device;
	}

	/**
	 * {@link #device} mutator.
	 * @param device	The new value.
	 **/
	@XmlElement
	public void setDevice(Device device) {
		preset(devicePropertyName, device);
		this.device = device;
	}

	/**
	 * {@link #numberOfHits} accessor.
	 * @return	The value.
	 **/
	public Integer getNumberOfHits() {
		return numberOfHits;
	}

	/**
	 * {@link #numberOfHits} mutator.
	 * @param numberOfHits	The new value.
	 **/
	@XmlElement
	public void setNumberOfHits(Integer numberOfHits) {
		preset(numberOfHitsPropertyName, numberOfHits);
		this.numberOfHits = numberOfHits;
	}
}
