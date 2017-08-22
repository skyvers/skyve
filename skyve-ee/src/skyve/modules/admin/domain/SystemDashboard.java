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
 * System Dashboard
 * 
 * @navhas n jobs 0..n Job
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public class SystemDashboard extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "SystemDashboard";

	/** @hidden */
	public static final String jobsPropertyName = "jobs";

	/**
	 * Jobs
	 **/
	private List<Job> jobs = new ArrayList<>();

	@Override
	@XmlTransient
	public String getBizModule() {
		return SystemDashboard.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return SystemDashboard.DOCUMENT_NAME;
	}

	public static SystemDashboard newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof SystemDashboard) && 
					this.getBizId().equals(((SystemDashboard) o).getBizId()));
	}

	/**
	 * {@link #jobs} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<Job> getJobs() {
		return jobs;
	}

	/**
	 * {@link #jobs} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Job getJobsElementById(String bizId) {
		return getElementById(jobs, bizId);
	}

	/**
	 * {@link #jobs} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setJobsElementById(@SuppressWarnings("unused") String bizId, Job element) {
		 setElementById(jobs, element);
	}
}
