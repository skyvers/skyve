package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.wildcat.domain.AbstractTransientBean;

/**
 * Display
 * 
 * @navhas n jobs 0..n Job
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public class Display extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Display";

	/** @hidden */
	public static final String jobsPropertyName = "jobs";

	private List<Job> jobs = new ArrayList<>();

	@Override
	@XmlTransient
	public String getBizModule() {
		return Display.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Display.DOCUMENT_NAME;
	}

	public static Display newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Display) && 
					this.getBizId().equals(((Display) o).getBizId()));
	}

	/**
	 * {@link #jobs} accessor.
	 **/
	@XmlElement
	public List<Job> getJobs() {
		return jobs;
	}

	/**
	 * {@link #jobs} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public Job getJobsElementById(String bizId) {
		return getElementById(jobs, bizId);
	}

	/**
	 * {@link #jobs} mutator.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 * @param jobs	The new value to set.
	 **/
	public void setJobsElementById(@SuppressWarnings("unused") String bizId, Job element) {
		 setElementById(jobs, element);
	}
}
