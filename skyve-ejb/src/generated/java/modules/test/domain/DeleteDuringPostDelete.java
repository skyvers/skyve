package modules.test.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * All Persistent
 * <br/>
 * Delete during postDelete test document
 * 
 * @navhas n aggregatedAssociation 0..1 AllAttributesPersistent
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class DeleteDuringPostDelete extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "DeleteDuringPostDelete";

	/** @hidden */
	public static final String aggregatedAssociationPropertyName = "aggregatedAssociation";

	/**
	 * Aggregated Association
	 **/
	private AllAttributesPersistent aggregatedAssociation = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return DeleteDuringPostDelete.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DeleteDuringPostDelete.DOCUMENT_NAME;
	}

	public static DeleteDuringPostDelete newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{bizId}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DeleteDuringPostDelete) && 
					this.getBizId().equals(((DeleteDuringPostDelete) o).getBizId()));
	}

	/**
	 * {@link #aggregatedAssociation} accessor.
	 * @return	The value.
	 **/
	public AllAttributesPersistent getAggregatedAssociation() {
		return aggregatedAssociation;
	}

	/**
	 * {@link #aggregatedAssociation} mutator.
	 * @param aggregatedAssociation	The new value.
	 **/
	@XmlElement
	public void setAggregatedAssociation(AllAttributesPersistent aggregatedAssociation) {
		if (this.aggregatedAssociation != aggregatedAssociation) {
			preset(aggregatedAssociationPropertyName, aggregatedAssociation);
			this.aggregatedAssociation = aggregatedAssociation;
		}
	}
}
