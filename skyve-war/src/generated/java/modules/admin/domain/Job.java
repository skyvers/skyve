package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import modules.admin.Job.JobExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;

/**
 * Job
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class Job extends AbstractPersistentBean {
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

	/** @hidden */
	public static final String beanBizIdPropertyName = "beanBizId";

	/** @hidden */
	public static final String beanModuleNamePropertyName = "beanModuleName";

	/** @hidden */
	public static final String beanDocumentNamePropertyName = "beanDocumentName";

	/** @hidden */
	public static final String instanceIdPropertyName = "instanceId";

	/**
	 * Start Time
	 **/
	private Timestamp startTime;

	/**
	 * End Time
	 **/
	private Timestamp endTime;

	/**
	 * Name
	 **/
	private String displayName;

	/**
	 * Percent Complete
	 **/
	private Integer percentComplete;

	/**
	 * Status
	 **/
	private String status;

	/**
	 * Log
	 **/
	private String log;

	/**
	 * Bean Biz Id
	 **/
	private String beanBizId;

	/**
	 * Bean Module Name
	 * <br/>
	 * The name of the module for the bean.
	 **/
	private String beanModuleName;

	/**
	 * Bean Document Name
	 * <br/>
	 * The name of the document for the bean.
	 **/
	private String beanDocumentName;

	/**
	 * Instance ID
	 **/
	private String instanceId;

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

	public static JobExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Job {displayName}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Job) && 
					this.getBizId().equals(((Job) o).getBizId()));
	}

	/**
	 * {@link #startTime} accessor.
	 * @return	The value.
	 **/
	public Timestamp getStartTime() {
		return startTime;
	}

	/**
	 * {@link #startTime} mutator.
	 * @param startTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setStartTime(Timestamp startTime) {
		preset(startTimePropertyName, startTime);
		this.startTime = startTime;
	}

	/**
	 * {@link #endTime} accessor.
	 * @return	The value.
	 **/
	public Timestamp getEndTime() {
		return endTime;
	}

	/**
	 * {@link #endTime} mutator.
	 * @param endTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setEndTime(Timestamp endTime) {
		preset(endTimePropertyName, endTime);
		this.endTime = endTime;
	}

	/**
	 * {@link #displayName} accessor.
	 * @return	The value.
	 **/
	public String getDisplayName() {
		return displayName;
	}

	/**
	 * {@link #displayName} mutator.
	 * @param displayName	The new value.
	 **/
	@XmlElement
	public void setDisplayName(String displayName) {
		preset(displayNamePropertyName, displayName);
		this.displayName = displayName;
	}

	/**
	 * {@link #percentComplete} accessor.
	 * @return	The value.
	 **/
	public Integer getPercentComplete() {
		return percentComplete;
	}

	/**
	 * {@link #percentComplete} mutator.
	 * @param percentComplete	The new value.
	 **/
	@XmlElement
	public void setPercentComplete(Integer percentComplete) {
		preset(percentCompletePropertyName, percentComplete);
		this.percentComplete = percentComplete;
	}

	/**
	 * {@link #status} accessor.
	 * @return	The value.
	 **/
	public String getStatus() {
		return status;
	}

	/**
	 * {@link #status} mutator.
	 * @param status	The new value.
	 **/
	@XmlElement
	public void setStatus(String status) {
		preset(statusPropertyName, status);
		this.status = status;
	}

	/**
	 * {@link #log} accessor.
	 * @return	The value.
	 **/
	public String getLog() {
		return log;
	}

	/**
	 * {@link #log} mutator.
	 * @param log	The new value.
	 **/
	@XmlElement
	public void setLog(String log) {
		preset(logPropertyName, log);
		this.log = log;
	}

	/**
	 * {@link #beanBizId} accessor.
	 * @return	The value.
	 **/
	public String getBeanBizId() {
		return beanBizId;
	}

	/**
	 * {@link #beanBizId} mutator.
	 * @param beanBizId	The new value.
	 **/
	@XmlElement
	public void setBeanBizId(String beanBizId) {
		preset(beanBizIdPropertyName, beanBizId);
		this.beanBizId = beanBizId;
	}

	/**
	 * {@link #beanModuleName} accessor.
	 * @return	The value.
	 **/
	public String getBeanModuleName() {
		return beanModuleName;
	}

	/**
	 * {@link #beanModuleName} mutator.
	 * @param beanModuleName	The new value.
	 **/
	@XmlElement
	public void setBeanModuleName(String beanModuleName) {
		preset(beanModuleNamePropertyName, beanModuleName);
		this.beanModuleName = beanModuleName;
	}

	/**
	 * {@link #beanDocumentName} accessor.
	 * @return	The value.
	 **/
	public String getBeanDocumentName() {
		return beanDocumentName;
	}

	/**
	 * {@link #beanDocumentName} mutator.
	 * @param beanDocumentName	The new value.
	 **/
	@XmlElement
	public void setBeanDocumentName(String beanDocumentName) {
		preset(beanDocumentNamePropertyName, beanDocumentName);
		this.beanDocumentName = beanDocumentName;
	}

	/**
	 * {@link #instanceId} accessor.
	 * @return	The value.
	 **/
	public String getInstanceId() {
		return instanceId;
	}

	/**
	 * {@link #instanceId} mutator.
	 * @param instanceId	The new value.
	 **/
	@XmlElement
	public void setInstanceId(String instanceId) {
		preset(instanceIdPropertyName, instanceId);
		this.instanceId = instanceId;
	}

	/**
	 * True when this job can be cancelled - it is still running and does not have a 
				status. This depends on the job implementing the ability to be cancelled.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isCancellable() {
		return (getStatus() == null);
	}

	/**
	 * {@link #isCancellable} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotCancellable() {
		return (! isCancellable());
	}

	/**
	 * True when this job can be re-run - it has a status, and a unique job name.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isRerunnabble() {
		return (((JobExtension)this).rerunnable());
	}

	/**
	 * {@link #isRerunnabble} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotRerunnabble() {
		return (! isRerunnabble());
	}
}
