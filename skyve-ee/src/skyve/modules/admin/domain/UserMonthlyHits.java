package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * UserMonthlyHits
 * 
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
	public static final String numberOfHitsPropertyName = "numberOfHits";

	private String userName;
	private Integer year;
	private Integer month;
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

	public static UserMonthlyHits newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{userName} - {year}/{month} = {numberOfHits}",
														this);
		}
		catch (Exception e) {
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
	 **/
	public String getUserName() {
		return userName;
	}

	/**
	 * {@link #userName} mutator.
	 * 
	 * @param userName	The new value to set.
	 **/
	@XmlElement
	public void setUserName(String userName) {
		preset(userNamePropertyName, userName);
		this.userName = userName;
	}

	/**
	 * {@link #year} accessor.
	 **/
	public Integer getYear() {
		return year;
	}

	/**
	 * {@link #year} mutator.
	 * 
	 * @param year	The new value to set.
	 **/
	@XmlElement
	public void setYear(Integer year) {
		preset(yearPropertyName, year);
		this.year = year;
	}

	/**
	 * {@link #month} accessor.
	 **/
	public Integer getMonth() {
		return month;
	}

	/**
	 * {@link #month} mutator.
	 * 
	 * @param month	The new value to set.
	 **/
	@XmlElement
	public void setMonth(Integer month) {
		preset(monthPropertyName, month);
		this.month = month;
	}

	/**
	 * {@link #numberOfHits} accessor.
	 **/
	public Integer getNumberOfHits() {
		return numberOfHits;
	}

	/**
	 * {@link #numberOfHits} mutator.
	 * 
	 * @param numberOfHits	The new value to set.
	 **/
	@XmlElement
	public void setNumberOfHits(Integer numberOfHits) {
		preset(numberOfHitsPropertyName, numberOfHits);
		this.numberOfHits = numberOfHits;
	}
}
