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
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Inverse Many To Many Persistent
 * <br/>
 * Many to many inverse.
 * 
 * @navhas n aggCollection 0..n InverseManyToManyPersistent
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class InverseManyToManyPersistent extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";
	/** @hidden */
	public static final String DOCUMENT_NAME = "InverseManyToManyPersistent";

	/** @hidden */
	public static final String aggCollectionPropertyName = "aggCollection";
	/** @hidden */
	public static final String invAggCollectionPropertyName = "invAggCollection";

	/**
	 * Aggregated Collection
	 **/
	private List<InverseManyToManyPersistent> aggCollection = new ChangeTrackingArrayList<>("aggCollection", this);
	/**
	 * Inverse
	 **/
	private List<InverseManyToManyPersistent> invAggCollection = new ArrayList<>();

	@Override
	@XmlTransient
	public String getBizModule() {
		return InverseManyToManyPersistent.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return InverseManyToManyPersistent.DOCUMENT_NAME;
	}

	public static InverseManyToManyPersistent newInstance() {
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
														"Inverse Many To Many Persistent",
														this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof InverseManyToManyPersistent) && 
					this.getBizId().equals(((InverseManyToManyPersistent) o).getBizId()));
	}

	/**
	 * {@link #aggCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<InverseManyToManyPersistent> getAggCollection() {
		return aggCollection;
	}

	/**
	 * {@link #aggCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public InverseManyToManyPersistent getAggCollectionElementById(String bizId) {
		return getElementById(aggCollection, bizId);
	}

	/**
	 * {@link #aggCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setAggCollectionElementById(String bizId, InverseManyToManyPersistent element) {
		setElementById(aggCollection, element);
	}

	/**
	 * {@link #invAggCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<InverseManyToManyPersistent> getInvAggCollection() {
		return invAggCollection;
	}

	/**
	 * {@link #invAggCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public InverseManyToManyPersistent getInvAggCollectionElementById(String bizId) {
		return getElementById(invAggCollection, bizId);
	}

	/**
	 * {@link #invAggCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setInvAggCollectionElementById(String bizId, InverseManyToManyPersistent element) {
		setElementById(invAggCollection, element);
	}
}
