package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Document Number
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class DocumentNumber extends AbstractPersistentBean implements org.skyve.domain.app.admin.DocumentNumber {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "DocumentNumber";

	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";

	/** @hidden */
	public static final String documentNamePropertyName = "documentName";

	/** @hidden */
	public static final String sequenceNamePropertyName = "sequenceName";

	/** @hidden */
	public static final String documentNumberPropertyName = "documentNumber";

	/**
	 * Module Name
	 * <br/>
	 * The name of the module.
	 **/
	private String moduleName;

	/**
	 * Document Name
	 * <br/>
	 * The name of the document.
	 **/
	private String documentName;

	/**
	 * Sequence
	 * <br/>
	 * The name of the field or sequence.
	 **/
	private String sequenceName;

	/**
	 * Number
	 * <br/>
	 * The last used number for the document (the next number used will increment this).
	 **/
	private String documentNumber;

	@Override
	@XmlTransient
	public String getBizModule() {
		return DocumentNumber.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DocumentNumber.DOCUMENT_NAME;
	}

	public static DocumentNumber newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{moduleName}.{documentName}.{sequenceName}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DocumentNumber) && 
					this.getBizId().equals(((DocumentNumber) o).getBizId()));
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
	 * {@link #documentName} accessor.
	 * @return	The value.
	 **/
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * {@link #documentName} mutator.
	 * @param documentName	The new value.
	 **/
	@XmlElement
	public void setDocumentName(String documentName) {
		preset(documentNamePropertyName, documentName);
		this.documentName = documentName;
	}

	/**
	 * {@link #sequenceName} accessor.
	 * @return	The value.
	 **/
	public String getSequenceName() {
		return sequenceName;
	}

	/**
	 * {@link #sequenceName} mutator.
	 * @param sequenceName	The new value.
	 **/
	@XmlElement
	public void setSequenceName(String sequenceName) {
		preset(sequenceNamePropertyName, sequenceName);
		this.sequenceName = sequenceName;
	}

	/**
	 * {@link #documentNumber} accessor.
	 * @return	The value.
	 **/
	public String getDocumentNumber() {
		return documentNumber;
	}

	/**
	 * {@link #documentNumber} mutator.
	 * @param documentNumber	The new value.
	 **/
	@XmlElement
	public void setDocumentNumber(String documentNumber) {
		preset(documentNumberPropertyName, documentNumber);
		this.documentNumber = documentNumber;
	}
}
