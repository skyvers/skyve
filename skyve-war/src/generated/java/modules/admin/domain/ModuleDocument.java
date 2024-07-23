package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * Module Document
 * <br/>
 * Non-persistent document used to store a selection of a document belonging to a specific
		module. This allows it to be used in a lookupDescription, e.g. Control Panel - Generate 
		Test Data, or when refreshing documents as part of DataMaintenance.
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ModuleDocument extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ModuleDocument";

	/** @hidden */
	public static final String includePropertyName = "include";

	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";

	/** @hidden */
	public static final String documentNamePropertyName = "documentName";

	/** @hidden */
	public static final String modDocNamePropertyName = "modDocName";

	/**
	 * Include
	 * <br/>
	 * Used to indicate if this document is to be included when refreshing as part of DataMaintenance.
	 **/
	private Boolean include;

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
	 * Document Name
	 * <br/>
	 * The name of the document.
	 **/
	private String modDocName;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ModuleDocument.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ModuleDocument.DOCUMENT_NAME;
	}

	public static ModuleDocument newInstance() {
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
		return toString();

	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ModuleDocument) && 
					this.getBizId().equals(((ModuleDocument) o).getBizId()));
	}

	/**
	 * {@link #include} accessor.
	 * @return	The value.
	 **/
	public Boolean getInclude() {
		return include;
	}

	/**
	 * {@link #include} mutator.
	 * @param include	The new value.
	 **/
	@XmlElement
	public void setInclude(Boolean include) {
		preset(includePropertyName, include);
		this.include = include;
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
	 * {@link #modDocName} accessor.
	 * @return	The value.
	 **/
	public String getModDocName() {
		return modDocName;
	}

	/**
	 * {@link #modDocName} mutator.
	 * @param modDocName	The new value.
	 **/
	@XmlElement
	public void setModDocName(String modDocName) {
		preset(modDocNamePropertyName, modDocName);
		this.modDocName = modDocName;
	}
}
