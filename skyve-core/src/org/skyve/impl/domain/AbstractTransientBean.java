package org.skyve.impl.domain;

import java.util.UUID;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.TransientBean;
import org.skyve.impl.domain.AbstractBean;

@XmlType
public abstract class AbstractTransientBean extends AbstractBean implements TransientBean {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -6469229627937972374L;

	private String bizId = UUID.randomUUID().toString();

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
}
