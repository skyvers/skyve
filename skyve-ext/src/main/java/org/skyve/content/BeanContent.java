package org.skyve.content;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.PersistentBean;

public class BeanContent extends Content {
	private static final long serialVersionUID = 8279368686334430859L;

	public BeanContent(PersistentBean bean) {
		super(bean.getBizCustomer(),
				bean.getBizModule(),
				bean.getBizDocument(),
				bean.getBizDataGroupId(),
				bean.getBizUserId(),
				bean.getBizId());
	}

	private Map<String, String> properties = new TreeMap<>();

	public Map<String, String> getProperties() {
		return properties;
	}
}
