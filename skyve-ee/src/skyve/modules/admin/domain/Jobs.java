package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * Jobs
 * 
 * @navhas n runningJobs 0..n Job
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
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
	private List<Job> runningJobs = new ArrayList<>();

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

	public static Jobs newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
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
	public List<Job> getRunningJobs() {
		return runningJobs;
	}

	/**
	 * {@link #runningJobs} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Job getRunningJobsElementById(String bizId) {
		return getElementById(runningJobs, bizId);
	}

	/**
	 * {@link #runningJobs} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRunningJobsElementById(@SuppressWarnings("unused") String bizId, Job element) {
		 setElementById(runningJobs, element);
	}
}
