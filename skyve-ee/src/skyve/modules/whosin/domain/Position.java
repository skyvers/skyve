package modules.whosin.domain;

import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

/**
 * Staff position.
 * 
 * @navhas n staff 0..1 Staff
 * @navhas n reportsTo 0..1 Position
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
public class Position extends AbstractPersistentBean implements HierarchicalBean<Position> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "whosin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Position";

	/** @hidden */
	public static final String positionTitlePropertyName = "positionTitle";
	/** @hidden */
	public static final String staffPropertyName = "staff";
	/** @hidden */
	public static final String reportsToPropertyName = "reportsTo";
	/** @hidden */
	public static final String demoDataPropertyName = "demoData";

	/**
	 * Position Title
	 **/
	private String positionTitle;
	/**
	 * Staff Person
	 **/
	private Staff staff = null;
	/**
	 * Reports To
	 **/
	private Position reportsTo = null;
	/**
	 * Demonstration Data
	 * <br/>
	 * If this is set, the data was created by the demo data job and can safely be deleted.
	 **/
	private Boolean demoData;
	private String bizParentId;


	@Override
	@XmlTransient
	public String getBizModule() {
		return Position.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Position.DOCUMENT_NAME;
	}

	public static Position newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{positionTitle} ({staff.contact.name} {staff.contact.mobile})",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Position) && 
					this.getBizId().equals(((Position) o).getBizId()));
	}

	/**
	 * {@link #positionTitle} accessor.
	 * @return	The value.
	 **/
	public String getPositionTitle() {
		return positionTitle;
	}

	/**
	 * {@link #positionTitle} mutator.
	 * @param positionTitle	The new value.
	 **/
	@XmlElement
	public void setPositionTitle(String positionTitle) {
		preset(positionTitlePropertyName, positionTitle);
		this.positionTitle = positionTitle;
	}

	/**
	 * {@link #staff} accessor.
	 * @return	The value.
	 **/
	public Staff getStaff() {
		return staff;
	}

	/**
	 * {@link #staff} mutator.
	 * @param staff	The new value.
	 **/
	@XmlElement
	public void setStaff(Staff staff) {
		preset(staffPropertyName, staff);
		this.staff = staff;
	}

	/**
	 * {@link #reportsTo} accessor.
	 * @return	The value.
	 **/
	public Position getReportsTo() {
		return reportsTo;
	}

	/**
	 * {@link #reportsTo} mutator.
	 * @param reportsTo	The new value.
	 **/
	@XmlElement
	public void setReportsTo(Position reportsTo) {
		this.reportsTo = reportsTo;
	}

	/**
	 * {@link #demoData} accessor.
	 * @return	The value.
	 **/
	public Boolean getDemoData() {
		return demoData;
	}

	/**
	 * {@link #demoData} mutator.
	 * @param demoData	The new value.
	 **/
	@XmlElement
	public void setDemoData(Boolean demoData) {
		preset(demoDataPropertyName, demoData);
		this.demoData = demoData;
	}

	@Override
	public String getBizParentId() {
		return bizParentId;
	}

	@Override
	@XmlElement
	public void setBizParentId(String bizParentId) {
		preset(HierarchicalBean.PARENT_ID, bizParentId);
		this.bizParentId = bizParentId;
	}

	@Override
	public Position getParent() {
		Position result = null;

		if (bizParentId != null) {
			Persistence p = CORE.getPersistence();
			DocumentQuery q = p.newDocumentQuery(Position.MODULE_NAME, Position.DOCUMENT_NAME);
			q.getFilter().addEquals(Bean.DOCUMENT_ID, bizParentId);
			result = q.retrieveBean();
		}

		return result;
	}

	@Override
	@XmlTransient
	public List<Position> getChildren() {
		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(Position.MODULE_NAME, Position.DOCUMENT_NAME);
		q.getFilter().addEquals(HierarchicalBean.PARENT_ID, bizParentId);
		return q.beanResults();
	}
}
