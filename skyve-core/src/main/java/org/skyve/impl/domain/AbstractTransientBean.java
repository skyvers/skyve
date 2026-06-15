package org.skyve.impl.domain;

import org.skyve.domain.TransientBean;
import org.skyve.impl.util.UUIDv7;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract base for domain beans that are never persisted to the database.
 *
 * <p>Extends {@link AbstractBean} with a pre-generated {@link org.skyve.impl.util.UUIDv7}
 * {@code bizId} so that every transient instance has a stable identity even without
 * a persistence context.  Subclasses are JAXB-annotated but do not carry a
 * {@link org.skyve.metadata.model.Persistent} descriptor.
 *
 * <p>Threading: not thread-safe.  Instances are bound to a single request or session
 * and must not be shared across threads without external synchronisation.
 *
 * @see AbstractBean
 * @see TransientBean
 */
@XmlType
public abstract class AbstractTransientBean extends AbstractBean implements TransientBean {
	private static final long serialVersionUID = -6469229627937972374L;

	private String bizId = UUIDv7.create().toString();

	private String bizCustomer;

	private String bizDataGroupId;

	private String bizUserId;

	@Override
	public String getBizId() {
		return bizId;
	}

	public void setBizId(String bizId) {
		this.bizId = bizId;
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
	
	@Override
	public boolean isPersisted() {
		return false;
	}

	@Override
	public boolean isNotPersisted() {
		return true;
	}
}
