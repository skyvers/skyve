package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * Snapshot
 * 
 * @navhas n copyToUser 0..1 User
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class Snapshot extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Snapshot";

	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";
	/** @hidden */
	public static final String queryNamePropertyName = "queryName";
	/** @hidden */
	public static final String namePropertyName = "name";
	/** @hidden */
	public static final String snapshotPropertyName = "snapshot";
	/** @hidden */
	public static final String copyToUserPropertyName = "copyToUser";
	/** @hidden */
	public static final String copyToUserSnapshotNamePropertyName = "copyToUserSnapshotName";

	private String moduleName;
	private String queryName;
	private String name;
	private String snapshot;
	private User copyToUser = null;
	private String copyToUserSnapshotName;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Snapshot.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Snapshot.DOCUMENT_NAME;
	}

	public static Snapshot newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{name} for {queryName} in module {moduleName}",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Snapshot) && 
					this.getBizId().equals(((Snapshot) o).getBizId()));
	}

	/**
	 * {@link #moduleName} accessor.
	 **/
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * {@link #moduleName} mutator.
	 * 
	 * @param moduleName	The new value to set.
	 **/
	@XmlElement
	public void setModuleName(String moduleName) {
		preset(moduleNamePropertyName, moduleName);
		this.moduleName = moduleName;
	}

	/**
	 * {@link #queryName} accessor.
	 **/
	public String getQueryName() {
		return queryName;
	}

	/**
	 * {@link #queryName} mutator.
	 * 
	 * @param queryName	The new value to set.
	 **/
	@XmlElement
	public void setQueryName(String queryName) {
		preset(queryNamePropertyName, queryName);
		this.queryName = queryName;
	}

	/**
	 * {@link #name} accessor.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * 
	 * @param name	The new value to set.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}

	/**
	 * {@link #snapshot} accessor.
	 **/
	public String getSnapshot() {
		return snapshot;
	}

	/**
	 * {@link #snapshot} mutator.
	 * 
	 * @param snapshot	The new value to set.
	 **/
	@XmlElement
	public void setSnapshot(String snapshot) {
		preset(snapshotPropertyName, snapshot);
		this.snapshot = snapshot;
	}

	/**
	 * {@link #copyToUser} accessor.
	 **/
	public User getCopyToUser() {
		return copyToUser;
	}

	/**
	 * {@link #copyToUser} mutator.
	 * 
	 * @param copyToUser	The new value to set.
	 **/
	@XmlElement
	public void setCopyToUser(User copyToUser) {
		this.copyToUser = copyToUser;
	}

	/**
	 * {@link #copyToUserSnapshotName} accessor.
	 **/
	public String getCopyToUserSnapshotName() {
		return copyToUserSnapshotName;
	}

	/**
	 * {@link #copyToUserSnapshotName} mutator.
	 * 
	 * @param copyToUserSnapshotName	The new value to set.
	 **/
	@XmlElement
	public void setCopyToUserSnapshotName(String copyToUserSnapshotName) {
		this.copyToUserSnapshotName = copyToUserSnapshotName;
	}
}
