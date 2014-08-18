package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.domain.types.Timestamp;
import org.skyve.wildcat.domain.AbstractPersistentBean;
import org.skyve.wildcat.domain.types.jaxb.TimestampMapper;

/**
 * Job
 * 
 * @stereotype "persistent"
 */
@XmlType
public class Job extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Job";

	/** @hidden */
	public static final String startTimePropertyName = "startTime";
	/** @hidden */
	public static final String endTimePropertyName = "endTime";
	/** @hidden */
	public static final String displayNamePropertyName = "displayName";
	/** @hidden */
	public static final String percentCompletePropertyName = "percentComplete";
	/** @hidden */
	public static final String statusPropertyName = "status";
	/** @hidden */
	public static final String logPropertyName = "log";

	private Timestamp startTime;
	private Timestamp endTime;
	private String displayName;
	private Integer percentComplete;
	private String status;
	private String log;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Job.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Job.DOCUMENT_NAME;
	}

	@Override
	@XmlTransient
	public String getBizKey() {
return (getDisplayName()==null?"Job":getDisplayName());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Job) && 
					this.getBizId().equals(((Job) o).getBizId()));
	}

	/**
	 * {@link #startTime} accessor.
	 **/
	public Timestamp getStartTime() {
		return startTime;
	}

	/**
	 * {@link #startTime} mutator.
	 * 
	 * @param startTime	The new value to set.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	@XmlElement
	public void setStartTime(Timestamp startTime) {
		preset(startTimePropertyName, startTime);
		this.startTime = startTime;
	}

	/**
	 * {@link #endTime} accessor.
	 **/
	public Timestamp getEndTime() {
		return endTime;
	}

	/**
	 * {@link #endTime} mutator.
	 * 
	 * @param endTime	The new value to set.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	@XmlElement
	public void setEndTime(Timestamp endTime) {
		preset(endTimePropertyName, endTime);
		this.endTime = endTime;
	}

	/**
	 * {@link #displayName} accessor.
	 **/
	public String getDisplayName() {
		return displayName;
	}

	/**
	 * {@link #displayName} mutator.
	 * 
	 * @param displayName	The new value to set.
	 **/
	@XmlElement
	public void setDisplayName(String displayName) {
		preset(displayNamePropertyName, displayName);
		this.displayName = displayName;
	}

	/**
	 * {@link #percentComplete} accessor.
	 **/
	public Integer getPercentComplete() {
		return percentComplete;
	}

	/**
	 * {@link #percentComplete} mutator.
	 * 
	 * @param percentComplete	The new value to set.
	 **/
	@XmlElement
	public void setPercentComplete(Integer percentComplete) {
		preset(percentCompletePropertyName, percentComplete);
		this.percentComplete = percentComplete;
	}

	/**
	 * {@link #status} accessor.
	 **/
	public String getStatus() {
		return status;
	}

	/**
	 * {@link #status} mutator.
	 * 
	 * @param status	The new value to set.
	 **/
	@XmlElement
	public void setStatus(String status) {
		preset(statusPropertyName, status);
		this.status = status;
	}

	/**
	 * {@link #log} accessor.
	 **/
	public String getLog() {
		return log;
	}

	/**
	 * {@link #log} mutator.
	 * 
	 * @param log	The new value to set.
	 **/
	@XmlElement
	public void setLog(String log) {
		preset(logPropertyName, log);
		this.log = log;
	}
}
