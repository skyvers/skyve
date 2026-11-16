package org.skyve.impl.domain;

import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.domain.types.jaxb.OptimisticLockMapper;
import org.skyve.impl.util.UUIDv7;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.persistence.MappedSuperclass;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Base implementation of {@link PersistentBean} that carries Hibernate ORM
 * annotations and the bidirectional JAXB/XML mapping for persistent beans.
 *
 * <p>Provides the standard persistent-bean fields: {@code bizId},
 * {@code bizVersion}, {@code bizLock}, and {@code bizKey}.
 *
 * <p>Invariant: no method in this class may be declared {@code final} because
 * Hibernate requires the ability to create subclass proxies via bytecode enhancement.
 *
 * @see PersistentBean
 * @see AbstractBean
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
	public @Nonnull String getBizId() {
		return bizId;
	}

	@XmlAttribute
	public void setBizId(@Nonnull String bizId) {
		this.bizId = bizId;
	}

	@Override
	public @Nullable Integer getBizVersion() {
		return bizVersion;
	}

	@Override
	@XmlAttribute
	public void setBizVersion(@Nullable Integer bizVersion) {
		this.bizVersion = bizVersion;
	}

	@Override
	public @Nullable OptimisticLock getBizLock() {
		return bizLock;
	}

	@Override
	@XmlAttribute
	@XmlJavaTypeAdapter(OptimisticLockMapper.class)
	public void setBizLock(@Nullable OptimisticLock bizLock) {
		this.bizLock = bizLock;
	}

	@Override
	public abstract @Nullable String getBizKey();

	@Override
	@XmlTransient
	public void setBizKey(@Nullable String bizKey) {
		this.bizKey = bizKey;
	}
	
	@Override
	public @Nullable String getBizCustomer() {
		return bizCustomer;
	}

	@Override
	@XmlTransient
	public void setBizCustomer(@Nonnull String bizCustomer) {
		this.bizCustomer = bizCustomer;
	}

	@Override
	public @Nullable String getBizFlagComment() {
		return bizFlagComment;
	}

	@Override
	@XmlElement
	public void setBizFlagComment(@Nullable String bizFlagComment) {
		this.bizFlagComment = bizFlagComment;
	}

	@Override
	public @Nullable Boolean getBizTagged() {
		return bizTagged;
	}

	@Override
	@XmlTransient
	public void setBizTagged(@Nullable Boolean bizTagged) {
		this.bizTagged = bizTagged;
	}

	@Override
	public @Nullable String getBizDataGroupId() {
		return bizDataGroupId;
	}

	@Override
	@XmlElement
	public void setBizDataGroupId(@Nullable String bizDataGroupId) {
		this.bizDataGroupId = bizDataGroupId;
	}

	@Override
	public @Nullable String getBizUserId() {
		return bizUserId;
	}

	@Override
	@XmlElement
	public void setBizUserId(@Nonnull String bizUserId) {
		this.bizUserId = bizUserId;
	}

	/**
	 * A useful string for being able to describe the record in a human readable way (via the bizKey) whilst also
	 * being able to uniquely identify that record (via the bizId).
	 *
	 * @return A string containing the bizKey and the bizId of the record.
	 */
	public @Nonnull String getBizKeyBizIdString() {
		StringBuilder result = new StringBuilder(bizKey.length() + 39);
		result.append(bizKey).append(" (").append(bizId).append(')');
		return result.toString();
	}
}
