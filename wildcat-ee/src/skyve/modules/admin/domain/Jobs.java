package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.wildcat.domain.AbstractTransientBean;

/**
 * Jobs
 * 
 * @navhas n runningJobs 0..n Job
 * @stereotype "transient"
 */
@XmlType
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

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Jobs) && 
					this.getBizId().equals(((Jobs) o).getBizId()));
	}

	/**
	 * {@link #runningJobs} accessor.
	 **/
	@XmlElement
	public List<Job> getRunningJobs() {
		return runningJobs;
	}

	/**
	 * {@link #runningJobs} mutator.
	 **/
	public Job getRunningJobsElementById(String bizId) {
		return findElementById(runningJobs, bizId);
	}
}
