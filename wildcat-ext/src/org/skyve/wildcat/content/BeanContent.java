package org.skyve.wildcat.content;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.PersistentBean;

public class BeanContent extends Content {
	public BeanContent(PersistentBean bean) {
		this.bizCustomer = bean.getBizCustomer();
		this.bizModule = bean.getBizModule();
		this.bizDocument = bean.getBizDocument();
		this.bizDataGroupId = bean.getBizDataGroupId();
		this.bizUserId = bean.getBizUserId();
		this.bizId = bean.getBizId();
	}

	private Map<String, String> properties = new TreeMap<>();

	public Map<String, String> getProperties() {
		return properties;
	}
}
