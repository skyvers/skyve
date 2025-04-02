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
 * Unique Constraint Persistent
 * <br/>
 * UniqueConstraint with 2 persistent references and 2 transient references to UniqueConstraintNonNukllable
 * 
 * @navhas n persistentReference 0..1 UniqueConstraintPersistent
 * @navhas n persistent1 0..1 UniqueConstraintNonNullable
 * @navhas n nonPersistent1 0..1 UniqueConstraintNonNullable
 * @navhas n persistent2 0..1 UniqueConstraintNonNullable
 * @navhas n nonPersistentReference 0..1 UniqueConstraintPersistent
 * @navhas n nonPersistent2 0..1 UniqueConstraintNonNullable
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class UniqueConstraintPersistent extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UniqueConstraintPersistent";

	/** @hidden */
	public static final String persistent1PropertyName = "persistent1";

	/** @hidden */
	public static final String persistent2PropertyName = "persistent2";

	/** @hidden */
	public static final String nonPersistent1PropertyName = "nonPersistent1";

	/** @hidden */
	public static final String nonPersistent2PropertyName = "nonPersistent2";

	/** @hidden */
	public static final String persistentReferencePropertyName = "persistentReference";

	/** @hidden */
	public static final String nonPersistentReferencePropertyName = "nonPersistentReference";

	/**
	 * Persistent 1
	 **/
	private UniqueConstraintNonNullable persistent1 = null;

	/**
	 * Persistent 2
	 **/
	private UniqueConstraintNonNullable persistent2 = null;

	/**
	 * Non Persistent 1
	 **/
	private UniqueConstraintNonNullable nonPersistent1 = null;

	/**
	 * Non Persistent 2
	 **/
	private UniqueConstraintNonNullable nonPersistent2 = null;

	/**
	 * Persistent Reference
	 **/
	private UniqueConstraintPersistent persistentReference = null;

	/**
	 * Non Persistent Reference
	 **/
	private UniqueConstraintPersistent nonPersistentReference = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return UniqueConstraintPersistent.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UniqueConstraintPersistent.DOCUMENT_NAME;
	}

	public static UniqueConstraintPersistent newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Unique Constraint Persistent", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #persistent1} accessor.
	 * @return	The value.
	 **/
	public UniqueConstraintNonNullable getPersistent1() {
		return persistent1;
	}

	/**
	 * {@link #persistent1} mutator.
	 * @param persistent1	The new value.
	 **/
	@XmlElement
	public void setPersistent1(UniqueConstraintNonNullable persistent1) {
		if (this.persistent1 != persistent1) {
			preset(persistent1PropertyName, persistent1);
			this.persistent1 = persistent1;
		}
	}

	/**
	 * {@link #persistent2} accessor.
	 * @return	The value.
	 **/
	public UniqueConstraintNonNullable getPersistent2() {
		return persistent2;
	}

	/**
	 * {@link #persistent2} mutator.
	 * @param persistent2	The new value.
	 **/
	@XmlElement
	public void setPersistent2(UniqueConstraintNonNullable persistent2) {
		if (this.persistent2 != persistent2) {
			preset(persistent2PropertyName, persistent2);
			this.persistent2 = persistent2;
		}
	}

	/**
	 * {@link #nonPersistent1} accessor.
	 * @return	The value.
	 **/
	public UniqueConstraintNonNullable getNonPersistent1() {
		return nonPersistent1;
	}

	/**
	 * {@link #nonPersistent1} mutator.
	 * @param nonPersistent1	The new value.
	 **/
	@XmlElement
	public void setNonPersistent1(UniqueConstraintNonNullable nonPersistent1) {
		if (this.nonPersistent1 != nonPersistent1) {
			preset(nonPersistent1PropertyName, nonPersistent1);
			this.nonPersistent1 = nonPersistent1;
		}
	}

	/**
	 * {@link #nonPersistent2} accessor.
	 * @return	The value.
	 **/
	public UniqueConstraintNonNullable getNonPersistent2() {
		return nonPersistent2;
	}

	/**
	 * {@link #nonPersistent2} mutator.
	 * @param nonPersistent2	The new value.
	 **/
	@XmlElement
	public void setNonPersistent2(UniqueConstraintNonNullable nonPersistent2) {
		if (this.nonPersistent2 != nonPersistent2) {
			preset(nonPersistent2PropertyName, nonPersistent2);
			this.nonPersistent2 = nonPersistent2;
		}
	}

	/**
	 * {@link #persistentReference} accessor.
	 * @return	The value.
	 **/
	public UniqueConstraintPersistent getPersistentReference() {
		return persistentReference;
	}

	/**
	 * {@link #persistentReference} mutator.
	 * @param persistentReference	The new value.
	 **/
	@XmlElement
	public void setPersistentReference(UniqueConstraintPersistent persistentReference) {
		if (this.persistentReference != persistentReference) {
			preset(persistentReferencePropertyName, persistentReference);
			this.persistentReference = persistentReference;
		}
	}

	/**
	 * {@link #nonPersistentReference} accessor.
	 * @return	The value.
	 **/
	public UniqueConstraintPersistent getNonPersistentReference() {
		return nonPersistentReference;
	}

	/**
	 * {@link #nonPersistentReference} mutator.
	 * @param nonPersistentReference	The new value.
	 **/
	@XmlElement
	public void setNonPersistentReference(UniqueConstraintPersistent nonPersistentReference) {
		if (this.nonPersistentReference != nonPersistentReference) {
			preset(nonPersistentReferencePropertyName, nonPersistentReference);
			this.nonPersistentReference = nonPersistentReference;
		}
	}
}
