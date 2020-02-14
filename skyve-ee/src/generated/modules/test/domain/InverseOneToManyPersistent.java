package modules.test.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Inverse One To Many Persistent
 * <br/>
 * One to many inverse.
 * 
 * @navhas n aggAssociation 0..1 InverseOneToManyPersistent
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class InverseOneToManyPersistent extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";
	/** @hidden */
	public static final String DOCUMENT_NAME = "InverseOneToManyPersistent";

	/** @hidden */
	public static final String aggAssociationPropertyName = "aggAssociation";
	/** @hidden */
	public static final String invAggAssociationPropertyName = "invAggAssociation";

	/**
	 * Aggregated Association
	 **/
	private InverseOneToManyPersistent aggAssociation = null;
	/**
	 * Inverse
	 **/
	private List<InverseOneToManyPersistent> invAggAssociation = new ArrayList<>();

	@Override
	@XmlTransient
	public String getBizModule() {
		return InverseOneToManyPersistent.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return InverseOneToManyPersistent.DOCUMENT_NAME;
	}

	public static InverseOneToManyPersistent newInstance() {
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
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"Inverse One To Many Persistent",
														this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof InverseOneToManyPersistent) && 
					this.getBizId().equals(((InverseOneToManyPersistent) o).getBizId()));
	}

	/**
	 * {@link #aggAssociation} accessor.
	 * @return	The value.
	 **/
	public InverseOneToManyPersistent getAggAssociation() {
		return aggAssociation;
	}

	/**
	 * {@link #aggAssociation} mutator.
	 * @param aggAssociation	The new value.
	 **/
	@XmlElement
	public void setAggAssociation(InverseOneToManyPersistent aggAssociation) {
		if (this.aggAssociation != aggAssociation) {
			preset(aggAssociationPropertyName, aggAssociation);
			InverseOneToManyPersistent oldAggAssociation = this.aggAssociation;
			this.aggAssociation = aggAssociation;
			if ((aggAssociation != null) && (aggAssociation.getInvAggAssociationElementById(getBizId()) == null)) {
				aggAssociation.getInvAggAssociation().add(this);
			}
			if (oldAggAssociation != null) {
				oldAggAssociation.getInvAggAssociation().remove(this);
			}
		}
	}

	/**
	 * {@link #invAggAssociation} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<InverseOneToManyPersistent> getInvAggAssociation() {
		return invAggAssociation;
	}

	/**
	 * {@link #invAggAssociation} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public InverseOneToManyPersistent getInvAggAssociationElementById(String bizId) {
		return getElementById(invAggAssociation, bizId);
	}

	/**
	 * {@link #invAggAssociation} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setInvAggAssociationElementById(String bizId, InverseOneToManyPersistent element) {
		setElementById(invAggAssociation, element);
	}

	/**
	 * {@link #invAggAssociation} add.
	 * @param element	The element to add.
	 **/
	public void addInvAggAssociationElement(InverseOneToManyPersistent element) {
		invAggAssociation.add(element);
		element.setAggAssociation(this);
	}

	/**
	 * {@link #invAggAssociation} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addInvAggAssociationElement(int index, InverseOneToManyPersistent element) {
		invAggAssociation.add(index, element);
		element.setAggAssociation(this);
	}

	/**
	 * {@link #invAggAssociation} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeInvAggAssociationElement(InverseOneToManyPersistent element) {
		boolean result = invAggAssociation.remove(element);
		if (result) {
			element.setAggAssociation(null);
		}
		return result;
	}

	/**
	 * {@link #invAggAssociation} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public InverseOneToManyPersistent removeInvAggAssociationElement(int index) {
		InverseOneToManyPersistent result = invAggAssociation.remove(index);
		result.setAggAssociation(null);
		return result;
	}
}
