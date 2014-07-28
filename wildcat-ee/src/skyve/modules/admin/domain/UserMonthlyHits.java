package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * UserMonthlyHits
 * 
 * @stereotype "persistent"
 */
@XmlType
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

	@Override
	@XmlTransient
	public String getBizKey() {
return getUserName() + " - " + getYear() + '/' + getMonth() + " = " + getNumberOfHits();
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
	 **/
	@XmlElement
	public void setNumberOfHits(Integer numberOfHits) {
		preset(numberOfHitsPropertyName, numberOfHits);
		this.numberOfHits = numberOfHits;
	}
}
