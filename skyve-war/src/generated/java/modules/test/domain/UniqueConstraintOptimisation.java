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
 * All Unique Constraint Optimisation
 * <br/>
 * Unique constraint to test if SQL is issued.
 * 
 * @navhas n persistentAssociation 0..1 UniqueConstraintOptimisation
 * @navhas n nonPersistentAssociation 0..1 UniqueConstraintOptimisation
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator", date = "2024-03-15T01:02:36.000Z")
public class UniqueConstraintOptimisation extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UniqueConstraintOptimisation";

	/** @hidden */
	public static final String uc1PropertyName = "uc1";

	/** @hidden */
	public static final String uc2PropertyName = "uc2";

	/** @hidden */
	public static final String uc3PropertyName = "uc3";

	/** @hidden */
	public static final String nonPersistentAssociationPropertyName = "nonPersistentAssociation";

	/** @hidden */
	public static final String persistentAssociationPropertyName = "persistentAssociation";

	/**
	 * Text
	 **/
	private String uc1;

	/**
	 * Text
	 **/
	private String uc2;

	/**
	 * Text
	 **/
	private String uc3;

	/**
	 * Non Persistent Association
	 **/
	private UniqueConstraintOptimisation nonPersistentAssociation = null;

	/**
	 * Persistent Association
	 **/
	private UniqueConstraintOptimisation persistentAssociation = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return UniqueConstraintOptimisation.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UniqueConstraintOptimisation.DOCUMENT_NAME;
	}

	public static UniqueConstraintOptimisation newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{uc1}/{uc2}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof UniqueConstraintOptimisation) && 
					this.getBizId().equals(((UniqueConstraintOptimisation) o).getBizId()));
	}

	/**
	 * {@link #uc1} accessor.
	 * @return	The value.
	 **/
	public String getUc1() {
		return uc1;
	}

	/**
	 * {@link #uc1} mutator.
	 * @param uc1	The new value.
	 **/
	@XmlElement
	public void setUc1(String uc1) {
		preset(uc1PropertyName, uc1);
		this.uc1 = uc1;
	}

	/**
	 * {@link #uc2} accessor.
	 * @return	The value.
	 **/
	public String getUc2() {
		return uc2;
	}

	/**
	 * {@link #uc2} mutator.
	 * @param uc2	The new value.
	 **/
	@XmlElement
	public void setUc2(String uc2) {
		preset(uc2PropertyName, uc2);
		this.uc2 = uc2;
	}

	/**
	 * {@link #uc3} accessor.
	 * @return	The value.
	 **/
	public String getUc3() {
		return uc3;
	}

	/**
	 * {@link #uc3} mutator.
	 * @param uc3	The new value.
	 **/
	@XmlElement
	public void setUc3(String uc3) {
		this.uc3 = uc3;
	}

	/**
	 * {@link #nonPersistentAssociation} accessor.
	 * @return	The value.
	 **/
	public UniqueConstraintOptimisation getNonPersistentAssociation() {
		return nonPersistentAssociation;
	}

	/**
	 * {@link #nonPersistentAssociation} mutator.
	 * @param nonPersistentAssociation	The new value.
	 **/
	@XmlElement
	public void setNonPersistentAssociation(UniqueConstraintOptimisation nonPersistentAssociation) {
		if (this.nonPersistentAssociation != nonPersistentAssociation) {
			preset(nonPersistentAssociationPropertyName, nonPersistentAssociation);
			this.nonPersistentAssociation = nonPersistentAssociation;
		}
	}

	/**
	 * {@link #persistentAssociation} accessor.
	 * @return	The value.
	 **/
	public UniqueConstraintOptimisation getPersistentAssociation() {
		return persistentAssociation;
	}

	/**
	 * {@link #persistentAssociation} mutator.
	 * @param persistentAssociation	The new value.
	 **/
	@XmlElement
	public void setPersistentAssociation(UniqueConstraintOptimisation persistentAssociation) {
		if (this.persistentAssociation != persistentAssociation) {
			preset(persistentAssociationPropertyName, persistentAssociation);
			this.persistentAssociation = persistentAssociation;
		}
	}
}
