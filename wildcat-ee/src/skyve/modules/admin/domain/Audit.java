package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.domain.types.Timestamp;
import org.skyve.wildcat.domain.AbstractPersistentBean;
import org.skyve.wildcat.domain.types.jaxb.TimestampMapper;

/**
 * Audit
 * 
 * @navhas n current 0..1 Audit
 * @stereotype "persistent"
 */
@XmlType
public class Audit extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Audit";

	/** @hidden */
	public static final String auditModuleNamePropertyName = "auditModuleName";
	/** @hidden */
	public static final String auditDocumentNamePropertyName = "auditDocumentName";
	/** @hidden */
	public static final String auditBizIdPropertyName = "auditBizId";
	/** @hidden */
	public static final String auditBizKeyPropertyName = "auditBizKey";
	/** @hidden */
	public static final String timestampPropertyName = "timestamp";
	/** @hidden */
	public static final String auditPropertyName = "audit";
	/** @hidden */
	public static final String currentPropertyName = "current";

	private String auditModuleName;
	private String auditDocumentName;
	private String auditBizId;
	private String auditBizKey;
	private Timestamp timestamp;
	private String audit;
	private Audit current = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Audit.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Audit.DOCUMENT_NAME;
	}

	@Override
	@XmlTransient
	public String getBizKey() {
return auditBizKey;
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Audit) && 
					this.getBizId().equals(((Audit) o).getBizId()));
	}

	/**
	 * {@link #auditModuleName} accessor.
	 **/
	public String getAuditModuleName() {
		return auditModuleName;
	}

	/**
	 * {@link #auditModuleName} mutator.
	 * 
	 * @param auditModuleName	The new value to set.
	 **/
	@XmlElement
	public void setAuditModuleName(String auditModuleName) {
		preset(auditModuleNamePropertyName, auditModuleName);
		this.auditModuleName = auditModuleName;
	}

	/**
	 * {@link #auditDocumentName} accessor.
	 **/
	public String getAuditDocumentName() {
		return auditDocumentName;
	}

	/**
	 * {@link #auditDocumentName} mutator.
	 * 
	 * @param auditDocumentName	The new value to set.
	 **/
	@XmlElement
	public void setAuditDocumentName(String auditDocumentName) {
		preset(auditDocumentNamePropertyName, auditDocumentName);
		this.auditDocumentName = auditDocumentName;
	}

	/**
	 * {@link #auditBizId} accessor.
	 **/
	public String getAuditBizId() {
		return auditBizId;
	}

	/**
	 * {@link #auditBizId} mutator.
	 * 
	 * @param auditBizId	The new value to set.
	 **/
	@XmlElement
	public void setAuditBizId(String auditBizId) {
		preset(auditBizIdPropertyName, auditBizId);
		this.auditBizId = auditBizId;
	}

	/**
	 * {@link #auditBizKey} accessor.
	 **/
	public String getAuditBizKey() {
		return auditBizKey;
	}

	/**
	 * {@link #auditBizKey} mutator.
	 * 
	 * @param auditBizKey	The new value to set.
	 **/
	@XmlElement
	public void setAuditBizKey(String auditBizKey) {
		preset(auditBizKeyPropertyName, auditBizKey);
		this.auditBizKey = auditBizKey;
	}

	/**
	 * {@link #timestamp} accessor.
	 **/
	public Timestamp getTimestamp() {
		return timestamp;
	}

	/**
	 * {@link #timestamp} mutator.
	 * 
	 * @param timestamp	The new value to set.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	@XmlElement
	public void setTimestamp(Timestamp timestamp) {
		preset(timestampPropertyName, timestamp);
		this.timestamp = timestamp;
	}

	/**
	 * {@link #audit} accessor.
	 **/
	public String getAudit() {
		return audit;
	}

	/**
	 * {@link #audit} mutator.
	 * 
	 * @param audit	The new value to set.
	 **/
	@XmlElement
	public void setAudit(String audit) {
		preset(auditPropertyName, audit);
		this.audit = audit;
	}

	/**
	 * {@link #current} accessor.
	 **/
	public Audit getCurrent() {
		return current;
	}

	/**
	 * {@link #current} mutator.
	 * 
	 * @param current	The new value to set.
	 **/
	@XmlElement
	public void setCurrent(Audit current) {
		preset(currentPropertyName, current);
		this.current = current;
	}
}
