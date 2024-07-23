package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import modules.admin.Job.JobExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Jobs
 * 
 * @navhas n runningJobs 0..n Job
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class Jobs extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Jobs";

	/** @hidden */
	public static final String runningJobsPropertyName = "runningJobs";

	/**
	 * Running Jobs
	 **/
	private List<JobExtension> runningJobs = new ChangeTrackingArrayList<>("runningJobs", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return Jobs.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Jobs.DOCUMENT_NAME;
	}

	public static Jobs newInstance() {
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
		return toString();

	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Jobs) && 
					this.getBizId().equals(((Jobs) o).getBizId()));
	}

	/**
	 * {@link #runningJobs} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<JobExtension> getRunningJobs() {
		return runningJobs;
	}

	/**
	 * {@link #runningJobs} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public JobExtension getRunningJobsElementById(String bizId) {
		return getElementById(runningJobs, bizId);
	}

	/**
	 * {@link #runningJobs} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRunningJobsElementById(String bizId, JobExtension element) {
		setElementById(runningJobs, element);
	}

	/**
	 * {@link #runningJobs} add.
	 * @param element	The element to add.
	 **/
	public boolean addRunningJobsElement(JobExtension element) {
		return runningJobs.add(element);
	}

	/**
	 * {@link #runningJobs} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addRunningJobsElement(int index, JobExtension element) {
		runningJobs.add(index, element);
	}

	/**
	 * {@link #runningJobs} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeRunningJobsElement(JobExtension element) {
		return runningJobs.remove(element);
	}

	/**
	 * {@link #runningJobs} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public JobExtension removeRunningJobsElement(int index) {
		return runningJobs.remove(index);
	}

	/**
	 * Whether this instance is configured to run schedule jobs
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isJobScheduler() {
		return (org.skyve.impl.util.UtilImpl.JOB_SCHEDULER);
	}

	/**
	 * {@link #isJobScheduler} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotJobScheduler() {
		return (! isJobScheduler());
	}
}
