package org.skyve.impl.domain;

import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.domain.types.jaxb.OptimisticLockMapper;
import org.skyve.impl.util.UUIDv7;

import jakarta.persistence.MappedSuperclass;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * 
 * @author Mike NOTE - DO NOT MAKE ANY METHOD IN THIS CLASS FINAL...
 */
@XmlType
@MappedSuperclass
public abstract class AbstractPersistentBean extends AbstractBean implements PersistentBean {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -3598872446903601438L;

	private String bizId = UUIDv7.create().toString();

	private Integer bizVersion;

	private OptimisticLock bizLock;
	
	private String bizKey;

	private String bizCustomer;
	
	/**
	 * Indicates whether a bean is a part of the currently selected tag.
	 * This is NOT a persistent field - it is derived based on the currently selected tag (if any)
	 */
	private Boolean bizTagged;

	private String bizFlagComment;

	private String bizDataGroupId;

	private String bizUserId;

	@Override
	public String getBizId() {
		return bizId;
	}

	@XmlAttribute
	public void setBizId(String bizId) {
		this.bizId = bizId;
	}

	@Override
	public Integer getBizVersion() {
		return bizVersion;
	}

	@Override
	@XmlAttribute
	public void setBizVersion(Integer bizVersion) {
		this.bizVersion = bizVersion;
	}

	@Override
	public OptimisticLock getBizLock() {
		return bizLock;
	}

	@Override
	@XmlAttribute
	@XmlJavaTypeAdapter(OptimisticLockMapper.class)
	public void setBizLock(OptimisticLock bizLock) {
		this.bizLock = bizLock;
	}

	@Override
	public abstract String getBizKey();

	@Override
	@XmlTransient
	public void setBizKey(String bizKey) {
		this.bizKey = bizKey;
	}
	
	@Override
	public int hashCode() {
		return bizId.hashCode();
	}

	@Override
	public String toString() {
		return super.toString() + '#' + bizId;
	}

	@Override
	public String getBizCustomer() {
		return bizCustomer;
	}

	@Override
	@XmlTransient
	public void setBizCustomer(String bizCustomer) {
		this.bizCustomer = bizCustomer;
	}

	@Override
	public String getBizFlagComment() {
		return bizFlagComment;
	}

	@Override
	@XmlElement
	public void setBizFlagComment(String bizFlagComment) {
		this.bizFlagComment = bizFlagComment;
	}

	@Override
	public Boolean getBizTagged() {
		return bizTagged;
	}

	@Override
	@XmlTransient
	public void setBizTagged(Boolean bizTagged) {
		this.bizTagged = bizTagged;
	}

	@Override
	public String getBizDataGroupId() {
		return bizDataGroupId;
	}

	@Override
	@XmlElement
	public void setBizDataGroupId(String bizDataGroupId) {
		this.bizDataGroupId = bizDataGroupId;
	}

	@Override
	public String getBizUserId() {
		return bizUserId;
	}

	@Override
	@XmlElement
	public void setBizUserId(String bizUserId) {
		this.bizUserId = bizUserId;
	}

	/**
	 * A useful string for being able to describe the record in a human readable way (via the bizKey) whilst also
	 * being able to uniquely identify that record (via the bizId).
	 *
	 * @return A string containing the bizKey and the bizId of the record.
	 */
	public String getBizKeyBizIdString() {
		StringBuilder result = new StringBuilder(bizKey.length() + 39);
		result.append(bizKey).append(" (").append(bizId).append(')');
		return result.toString();
	}
}
