package org.skyve.impl.domain;

import org.skyve.domain.TransientBean;
import org.skyve.impl.util.UUIDv7;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

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
