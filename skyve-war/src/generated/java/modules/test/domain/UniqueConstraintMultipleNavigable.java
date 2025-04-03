package modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Unique Constraint Multiple Navigable
 * <br/>
 * UniqueConstraint with an inverse to make sure UC processing navigates to the same object twice.
 * 
 * @navhas n aggAssociation 0..1 UniqueConstraintMultipleNavigable
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class UniqueConstraintMultipleNavigable extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UniqueConstraintMultipleNavigable";

	/** @hidden */
	public static final String uniqueNamePropertyName = "uniqueName";

	/** @hidden */
	public static final String aggAssociationPropertyName = "aggAssociation";

	/** @hidden */
	public static final String invAggAssociationPropertyName = "invAggAssociation";

	/**
	 * Unique Name
	 **/
	private String uniqueName;

	/**
	 * Aggregated Association
	 **/
	private UniqueConstraintMultipleNavigable aggAssociation = null;

	/**
	 * Inverse
	 **/
	private UniqueConstraintMultipleNavigable invAggAssociation;

	@Override
	@XmlTransient
	public String getBizModule() {
		return UniqueConstraintMultipleNavigable.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UniqueConstraintMultipleNavigable.DOCUMENT_NAME;
	}

	public static UniqueConstraintMultipleNavigable newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Unique Constraint Multiple Navigable", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #uniqueName} accessor.
	 * @return	The value.
	 **/
	public String getUniqueName() {
		return uniqueName;
	}

	/**
	 * {@link #uniqueName} mutator.
	 * @param uniqueName	The new value.
	 **/
	@XmlElement
	public void setUniqueName(String uniqueName) {
		preset(uniqueNamePropertyName, uniqueName);
		this.uniqueName = uniqueName;
	}

	/**
	 * {@link #aggAssociation} accessor.
	 * @return	The value.
	 **/
	public UniqueConstraintMultipleNavigable getAggAssociation() {
		return aggAssociation;
	}

	/**
	 * {@link #aggAssociation} mutator.
	 * @param aggAssociation	The new value.
	 **/
	@XmlElement
	public void setAggAssociation(UniqueConstraintMultipleNavigable aggAssociation) {
		if (this.aggAssociation != aggAssociation) {
			preset(aggAssociationPropertyName, aggAssociation);
			UniqueConstraintMultipleNavigable oldAggAssociation = this.aggAssociation;
			this.aggAssociation = aggAssociation;
			if (aggAssociation != null) {
				aggAssociation.setInvAggAssociation(this);
			}
			if (oldAggAssociation != null) {
				oldAggAssociation.nullInvAggAssociation();
			}
		}
	}

	public void nullAggAssociation() {
		this.aggAssociation = null;
	}

	/**
	 * {@link #invAggAssociation} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public UniqueConstraintMultipleNavigable getInvAggAssociation() {
		return invAggAssociation;
	}

	/**
	 * {@link #invAggAssociation} mutator.
	 * @param invAggAssociation	The new value.
	 **/
	public void setInvAggAssociation(UniqueConstraintMultipleNavigable invAggAssociation) {
		if (this.invAggAssociation != invAggAssociation) {
			UniqueConstraintMultipleNavigable oldInvAggAssociation = this.invAggAssociation;
			this.invAggAssociation = invAggAssociation;
			if (invAggAssociation != null) {
				invAggAssociation.setAggAssociation(this);
			}
			if (oldInvAggAssociation != null) {
				oldInvAggAssociation.nullAggAssociation();
			}
		}
	}

	public void nullInvAggAssociation() {
		this.invAggAssociation = null;
	}
}
