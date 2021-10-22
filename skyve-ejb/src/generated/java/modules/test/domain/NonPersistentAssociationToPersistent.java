package modules.test.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * NonPersistentAssociationToPersistent
 * <br/>
 * Test if a transient association to a persistent document will cascade.
 * 
 * @navhas n association 0..1 AllAttributesPersistent
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class NonPersistentAssociationToPersistent extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "NonPersistentAssociationToPersistent";

	/** @hidden */
	public static final String textPropertyName = "text";

	/** @hidden */
	public static final String associationPropertyName = "association";

	/**
	 * Text
	 **/
	private String text;

	/**
	 * Association
	 **/
	private AllAttributesPersistent association = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return NonPersistentAssociationToPersistent.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return NonPersistentAssociationToPersistent.DOCUMENT_NAME;
	}

	public static NonPersistentAssociationToPersistent newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{text}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof NonPersistentAssociationToPersistent) && 
					this.getBizId().equals(((NonPersistentAssociationToPersistent) o).getBizId()));
	}

	/**
	 * {@link #text} accessor.
	 * @return	The value.
	 **/
	public String getText() {
		return text;
	}

	/**
	 * {@link #text} mutator.
	 * @param text	The new value.
	 **/
	@XmlElement
	public void setText(String text) {
		preset(textPropertyName, text);
		this.text = text;
	}

	/**
	 * {@link #association} accessor.
	 * @return	The value.
	 **/
	public AllAttributesPersistent getAssociation() {
		return association;
	}

	/**
	 * {@link #association} mutator.
	 * @param association	The new value.
	 **/
	@XmlElement
	public void setAssociation(AllAttributesPersistent association) {
		if (this.association != association) {
			preset(associationPropertyName, association);
			this.association = association;
		}
	}
}
