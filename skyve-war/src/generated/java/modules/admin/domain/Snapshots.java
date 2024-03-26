package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Snapshots
 * 
 * @navhas n snapshotsToReorder 0..n Snapshot
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class Snapshots extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Snapshots";

	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";

	/** @hidden */
	public static final String queryNamePropertyName = "queryName";

	/** @hidden */
	public static final String snapshotsToReorderPropertyName = "snapshotsToReorder";

	/**
	 * Module
	 **/
	private String moduleName;

	/**
	 * List
	 **/
	private String queryName;

	/**
	 * Snapshots
	 **/
	private List<Snapshot> snapshotsToReorder = new ChangeTrackingArrayList<>("snapshotsToReorder", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return Snapshots.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Snapshots.DOCUMENT_NAME;
	}

	public static Snapshots newInstance() {
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
		return ((o instanceof Snapshots) && 
					this.getBizId().equals(((Snapshots) o).getBizId()));
	}

	/**
	 * {@link #moduleName} accessor.
	 * @return	The value.
	 **/
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * {@link #moduleName} mutator.
	 * @param moduleName	The new value.
	 **/
	@XmlElement
	public void setModuleName(String moduleName) {
		preset(moduleNamePropertyName, moduleName);
		this.moduleName = moduleName;
	}

	/**
	 * {@link #queryName} accessor.
	 * @return	The value.
	 **/
	public String getQueryName() {
		return queryName;
	}

	/**
	 * {@link #queryName} mutator.
	 * @param queryName	The new value.
	 **/
	@XmlElement
	public void setQueryName(String queryName) {
		preset(queryNamePropertyName, queryName);
		this.queryName = queryName;
	}

	/**
	 * {@link #snapshotsToReorder} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<Snapshot> getSnapshotsToReorder() {
		return snapshotsToReorder;
	}

	/**
	 * {@link #snapshotsToReorder} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Snapshot getSnapshotsToReorderElementById(String bizId) {
		return getElementById(snapshotsToReorder, bizId);
	}

	/**
	 * {@link #snapshotsToReorder} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setSnapshotsToReorderElementById(String bizId, Snapshot element) {
		setElementById(snapshotsToReorder, element);
	}

	/**
	 * {@link #snapshotsToReorder} add.
	 * @param element	The element to add.
	 **/
	public boolean addSnapshotsToReorderElement(Snapshot element) {
		return snapshotsToReorder.add(element);
	}

	/**
	 * {@link #snapshotsToReorder} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addSnapshotsToReorderElement(int index, Snapshot element) {
		snapshotsToReorder.add(index, element);
	}

	/**
	 * {@link #snapshotsToReorder} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeSnapshotsToReorderElement(Snapshot element) {
		return snapshotsToReorder.remove(element);
	}

	/**
	 * {@link #snapshotsToReorder} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public Snapshot removeSnapshotsToReorderElement(int index) {
		return snapshotsToReorder.remove(index);
	}
}
