package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * Snapshot
 * 
 * @stereotype "persistent"
 */
@XmlType
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

	private String moduleName;
	private String queryName;
	private String name;
	private String snapshot;

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

	@Override
	@XmlTransient
	public String getBizKey() {
return (name == null ? "New Snapshot" : name);
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
	 **/
	@XmlElement
	public void setSnapshot(String snapshot) {
		preset(snapshotPropertyName, snapshot);
		this.snapshot = snapshot;
	}
}
