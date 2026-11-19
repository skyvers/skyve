package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Model Lookup Row
 * 
 * @navhas n selection 0..1 LookupDescription
 * @navhas n singleSelection 0..1 LookupDescription
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ModelLookupRow extends AbstractPersistentBean implements ChildBean<ModelLookupFixture> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ModelLookupRow";

	/** @hidden */
	public static final String selectionPropertyName = "selection";

	/** @hidden */
	public static final String singleSelectionPropertyName = "singleSelection";

	/**
	 * Grid Model Lookup
	 **/
	private LookupDescription selection = null;

	/**
	 * Single-column Model Lookup
	 **/
	private LookupDescription singleSelection = null;

	private ModelLookupFixture parent;

	private Integer bizOrdinal;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ModelLookupRow.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ModelLookupRow.DOCUMENT_NAME;
	}

	public static ModelLookupRow newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{selection}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #selection} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getSelection() {
		return selection;
	}

	/**
	 * {@link #selection} mutator.
	 * @param selection	The new value.
	 **/
	@XmlElement
	public void setSelection(LookupDescription selection) {
		if (this.selection != selection) {
			preset(selectionPropertyName, selection);
			this.selection = selection;
		}
	}

	/**
	 * {@link #singleSelection} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getSingleSelection() {
		return singleSelection;
	}

	/**
	 * {@link #singleSelection} mutator.
	 * @param singleSelection	The new value.
	 **/
	@XmlElement
	public void setSingleSelection(LookupDescription singleSelection) {
		if (this.singleSelection != singleSelection) {
			preset(singleSelectionPropertyName, singleSelection);
			this.singleSelection = singleSelection;
		}
	}

	@Override
	public ModelLookupFixture getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(ModelLookupFixture parent) {
		if (this.parent != parent) {
			preset(ChildBean.PARENT_NAME, parent);
			this.parent = parent;
		}
	}

	@Override
	public Integer getBizOrdinal() {
		return bizOrdinal;
	}

	@Override
	@XmlElement
	public void setBizOrdinal(Integer bizOrdinal) {
		preset(Bean.ORDINAL_NAME, bizOrdinal);
		this.bizOrdinal =  bizOrdinal;
	}
}
