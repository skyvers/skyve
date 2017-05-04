package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * DocumentNumber
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class DocumentNumber extends AbstractPersistentBean {
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
	 * The name of the module.
	 **/
	private String moduleName;
	/**
	 * The name of the document.
	 **/
	private String documentName;
	/**
	 * The name of the field or sequence.
	 **/
	private String sequenceName;
	/**
	 * The last used number for the document.
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

	public static DocumentNumber newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{moduleName}.{documentName}.{sequenceName}",
														this);
		}
		catch (Exception e) {
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
	 **/
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * {@link #moduleName} mutator.
	 * 
	 * @param moduleName	The new value to set.
	 **/
	@XmlElement
	public void setModuleName(String moduleName) {
		preset(moduleNamePropertyName, moduleName);
		this.moduleName = moduleName;
	}

	/**
	 * {@link #documentName} accessor.
	 **/
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * {@link #documentName} mutator.
	 * 
	 * @param documentName	The new value to set.
	 **/
	@XmlElement
	public void setDocumentName(String documentName) {
		preset(documentNamePropertyName, documentName);
		this.documentName = documentName;
	}

	/**
	 * {@link #sequenceName} accessor.
	 **/
	public String getSequenceName() {
		return sequenceName;
	}

	/**
	 * {@link #sequenceName} mutator.
	 * 
	 * @param sequenceName	The new value to set.
	 **/
	@XmlElement
	public void setSequenceName(String sequenceName) {
		preset(sequenceNamePropertyName, sequenceName);
		this.sequenceName = sequenceName;
	}

	/**
	 * {@link #documentNumber} accessor.
	 **/
	public String getDocumentNumber() {
		return documentNumber;
	}

	/**
	 * {@link #documentNumber} mutator.
	 * 
	 * @param documentNumber	The new value to set.
	 **/
	@XmlElement
	public void setDocumentNumber(String documentNumber) {
		preset(documentNumberPropertyName, documentNumber);
		this.documentNumber = documentNumber;
	}
}
