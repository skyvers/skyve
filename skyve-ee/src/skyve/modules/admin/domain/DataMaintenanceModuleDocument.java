package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * DataMaintenanceModuleDocument
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public class DataMaintenanceModuleDocument extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "DataMaintenanceModuleDocument";

	/** @hidden */
	public static final String includePropertyName = "include";
	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";
	/** @hidden */
	public static final String documentNamePropertyName = "documentName";
	/** @hidden */
	public static final String modDocNamePropertyName = "modDocName";

	private Boolean include;
	/**
	 * The name of the module.
	 **/
	private String moduleName;
	/**
	 * The name of the document.
	 **/
	private String documentName;
	/**
	 * The name of the document.
	 **/
	private String modDocName;

	@Override
	@XmlTransient
	public String getBizModule() {
		return DataMaintenanceModuleDocument.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DataMaintenanceModuleDocument.DOCUMENT_NAME;
	}

	public static DataMaintenanceModuleDocument newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DataMaintenanceModuleDocument) && 
					this.getBizId().equals(((DataMaintenanceModuleDocument) o).getBizId()));
	}

	/**
	 * {@link #include} accessor.
	 **/
	public Boolean getInclude() {
		return include;
	}

	/**
	 * {@link #include} mutator.
	 * 
	 * @param include	The new value to set.
	 **/
	@XmlElement
	public void setInclude(Boolean include) {
		preset(includePropertyName, include);
		this.include = include;
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
	 * {@link #modDocName} accessor.
	 **/
	public String getModDocName() {
		return modDocName;
	}

	/**
	 * {@link #modDocName} mutator.
	 * 
	 * @param modDocName	The new value to set.
	 **/
	@XmlElement
	public void setModDocName(String modDocName) {
		preset(modDocNamePropertyName, modDocName);
		this.modDocName = modDocName;
	}
}
